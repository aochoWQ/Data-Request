library(shiny)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(leaflet)
library(stringr)
library(plotly)
library(sf)
library(ggplot2)
library(shinycssloaders)


load("SJ_dashboard_data.RData") # Created in SJ_data_download.Rmd (Line345) to include results_SJ_metals_only2, , criteria,  polygon_shapefiles

sj_data = results_SJ_metals_only2[results_SJ_metals_only2$UnitFlag=="Accept",] #This filters out Sediment data. Assuming we don't want to use sediment data.
sj_data$id=sj_data$MonitoringLocationIdentifier
choices_j = unique(unlist(strsplit(as.character(sj_data$Jurisdictions), ", ")))
choices_j1 <- choices_j[choices_j!="NA"]

#******FIX sj_data has multiple jurisdictions.. do we want to simplify to one jurisdiction?

# Define UI

# load(SJ_app_resources.Rdata) # Assuming this loads results_SJ_metals_only2

ui <- dashboardPage(
  dashboardHeader(title = "San Juan Metals Data Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parameter Summaries", tabName = "param_sum", icon = icon("vial")),
      menuItem("Map & Time Series", tabName = "map_time_series", icon = icon("map"),selected = TRUE)#,
      #menuItem("Time Series", tabName = "time_series", icon = icon("line-chart"))
    ),
    selectInput("sampleFraction", "Select Sample Fraction", choices = unique(sj_data$ResultSampleFractionText)),
    selectInput("parameter", "Select Parameter", choices = sort(unique(sj_data$CharacteristicName))),
    selectInput("jurisdiction", "Select Jurisdiction(s)", choices = choices_j, selected = choices_j1, multiple = TRUE),
    dateRangeInput("dateFilter","Date Range",start="2015-01-01"),
    checkboxInput("nonDetect", "Include NonDetect Values", value = TRUE),
    bsTooltip(id = "nonDetect", title = "Include measurements where no detection was noted.", placement = "right", trigger = "hover"),
    checkboxInput("nonSJ_Site", "Include Non-SJ WIIN Project Sites", value = FALSE),
    bsTooltip(id = "nonSJ_Site", title = "Include sites not in SJ WIIN 39 Site List but still in SJ watershed.", placement = "right", trigger = "hover")
  ),
  dashboardBody(
    tags$head(
    tags$style(HTML("
    
                  .main-sidebar {
                    position: fixed; /* Fixed Sidebar (stay in place on scroll and position relative to viewport) */
                    height: 100%;
                    overflow-y: auto; /* Scrollable contents if viewport is shorter than content. */
                }
                /* Change the background color of the main content area */
                .content-wrapper {
                    background-color: #FFFFFF !important; /* Use !important to override default styles */
                }
                /* Style for the map and plot outputs to ensure they have space */
                .leaflet-output, .plotly-output {
                    margin-bottom: 20px;
                    height: 600px; 
                }
              
            "))
  ),
    tabItems(
      tabItem(tabName = "map_time_series",
              tags$p("Percentiles were calculated based off of average values from filtered data.", style = "padding: 4px; font-size: 14px; text-align:center;"),
              
              leafletOutput("sj_site_map", height = "600px"),
              tags$p("Click on any site on the map to see the corresponding time series plot for that site.", style = "padding: 8px; font-size: 14px; text-align:center;"),
              tags$div(style = "height: 20px;"),
              plotlyOutput("timeSeriesPlot")),
      tabItem(tabName = "param_sum",
              fluidRow(
                column(12,
                       h3("Parameter Summary", style = "text-align: center;"),
                       withSpinner(textOutput("loadingPlot"), type = 8, color = "#000000", size = 2),

                ),
              plotOutput("param_facet", height = "3800px", width = "1000px")
                      )
              )
  
        )
  )
  )#UI END


server <- function(input, output, session) {
  
  filteredData <- reactive({
    #print("dates")
    start=as.Date(input$dateFilter[1])
    end=as.Date(input$dateFilter[2])
    data <- sj_data %>%
      filter(ResultSampleFractionText %in% input$sampleFraction,
             if (input$nonDetect) TRUE else SJ_ND != "Yes",
             if (input$nonSJ_Site) TRUE else SJ_PROJECT_SITE!="No")%>%
      filter(stringr::str_detect(Jurisdictions, paste(input$jurisdiction, collapse = "|")))%>%
      filter(ActivityStartDate>start &ActivityStartDate<end)
    data
  })
  
  output$sj_site_map <- renderLeaflet({
    aggregated_data <-filteredData()
    
    
    aggregated_data <- aggregated_data %>%
      filter(CharacteristicName %in% input$parameter)%>%
      group_by(CharacteristicName, MonitoringLocationIdentifier, ResultSampleFractionText) %>%
      summarise(
        AverageValue = mean(SJ_ResultValue, na.rm = TRUE),  # Replace SJ_ResultValue with your actual column name
        Latitude = first(LatitudeMeasure),  # Assuming LatitudeMeasure is constant per group
        Longitude = first(LongitudeMeasure),  # Assuming LongitudeMeasure is constant per group
        .groups = "drop"  # This drops the grouping so you can use the data frame normally afterwards
      )%>%
      mutate(color = ifelse(AverageValue >= quantile(AverageValue, 0.75, na.rm = TRUE), "red",
                     ifelse(AverageValue >= quantile(AverageValue, 0.5, na.rm = TRUE),"orange",
                            ifelse(AverageValue >= quantile(AverageValue, 0.25, na.rm = TRUE),"yellow", "green"))))
    
    #****** Add a box area to view based off of the sites in aggregated_data
    
    leaflet()%>%
    addProviderTiles("Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
      addMapPane("Sites", zIndex = 415)%>%
      addMapPane("Tribes", zIndex = 410)%>%
      addCircleMarkers(data=aggregated_data,lng=~Longitude,lat=~Latitude, color = ~color, radius = 8,
                       layerId = ~MonitoringLocationIdentifier,fillOpacity = 0.8,label = ~MonitoringLocationIdentifier,group="Sites",options = pathOptions(pane = "Sites"))%>%
      addLegend(position = "bottomleft",
                labels = c("Above 75th Percentile", "50th to 75th Percentile", "25th to 50th Percentile", "Below 25th Percentile"),
colors = c("red","orange","yellow","green"))%>%
      addPolygons(data=sj_tribes_final,group = "Tribes",fillOpacity = 0.3,color="blue",weight = 2,options = pathOptions(pane = "Tribes"),
                  popup=paste0(
                    "Tribe Name: ", sj_tribes_final$NAME)) 
    #******FIX Change color on tribes, Add state polygons, Add SJ Project Area Polygon      %>%
        #leaflet::addLayersControl(position ="topleft",overlayGroups = c("Tribes"),
        #options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))
  })#End of renderLeaflet

  
  # observe(input$sj_site_map_marker_click{
     #******FIX Change color on clicked site
  # })
  
  
  output$timeSeriesPlot <- renderPlotly({
    
    #******FIX ADD Criteria lines to plot using Filters above plot, Jurisdiction, Use 
    #*
    
    plot_data <- filteredData()
    
    if(!is.null(input$sj_site_map_marker_click)) {
    click_info <- input$sj_site_map_marker_click
    clicked_id <- click_info$id
    }
    else{
      clicked_id=unique(plot_data$MonitoringLocationIdentifier)[1]
    }
    # Extract the relevant data for plotting
    plot_data <- plot_data[plot_data$MonitoringLocationIdentifier == clicked_id, ]
    
    # Generate the time series plot
    plot_ly(plot_data, x = ~ActivityStartDate, y = ~SJ_ResultValue, type = 'scatter', mode = 'markers',marker=list(size=10)) %>%
      layout(title = paste("Time Series of",input$sampleFraction,input$parameter,"for",clicked_id,sep=" "),
             xaxis = list(title = "Sample Dates"),  # Set x-axis label
             yaxis = list(title = "mg/L")  # Set y-axis label
      )
  })
  
  output$param_facet <- renderPlot({
    
    param_dat <- filteredData()  
    param_dat=param_dat%>%
      select(CharacteristicName,ActivityStartDate,SJ_ResultValue,ResultSampleFractionText)%>%
      distinct()

    isPlotLoading(FALSE)
    ggplot(param_dat, aes(x = ActivityStartDate, y = SJ_ResultValue,color=CharacteristicName)) +
      geom_point(size = 3, alpha = 0.6) +
      labs(x = "Sample Dates",
           y = "mg/L") +
      facet_wrap(~CharacteristicName, scales = "free",ncol = 1) +  # Organize by rows
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") 
    
    
  })
  isPlotLoading <- reactiveVal(TRUE)
  output$loadingPlot <- renderText({
    if(isPlotLoading()) {
      "Loading plot..."
    } else {
      "This plot shows the time series data for all parameters. These plots do not respond to the parameter name filter on the left-hand side."
    }
  })
  
  
  #******FIX Download data button 
  #*Ask group what kind of data would they want downloaded what would they want to dig into?
  #*Possibly in the main side-bar to download filtered data? OR create another tabItem() with filterable data download..

}


# Run the app
shinyApp(ui, server)

