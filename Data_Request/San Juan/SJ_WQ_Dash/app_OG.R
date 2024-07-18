library(shiny)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(DT)
library(leaflet)
library(stringr)
library(plotly)
library(sf)
library(ggplot2)
library(shinycssloaders)
library(scales)

source("helper_functions.R") # Has a few functions for calculating criteria.

load("SJ_dashboard_data.RData") # Created in SJ_data_download.Rmd (Line345) to include results_SJ_metals_only2, , criteria,  polygon_shapefiles

sj_data <- results_SJ_metals_only2[results_SJ_metals_only2$UnitFlag=="Accept",] #This filters out Sediment data. Assuming we don't want to use sediment data.
sj_data$id <- sj_data$MonitoringLocationIdentifier #Adding column for clicked map id?
choices_j1 <- sort(unique(sj_data$Jurisdiction[sj_data$Jurisdiction!="NA"]))
choices_frac <- unique(sj_data$ResultSampleFractionText)
choices_char <- sort(unique(sj_data$CharacteristicName))

#******FIX sj_data has multiple jurisdictions.. do we want to simplify to one jurisdiction?

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "San Juan Basin Metals Analysis Dashboard" , titleWidth = "30%") ,
  dashboardSidebar(
    sidebarMenu(
      #menuItem("Parameter Overview", tabName = "param_sum", icon = icon("vial")),
      menuItem("Percentile Map", tabName = "map_time_series", icon = icon("map"),selected = TRUE),
      menuItem("Jurisdiction Criteria Map", tabName = "jurisdiction_assessments", icon = icon("line-chart"))
    ) ),
  dashboardBody(
    tags$head(
    tags$style(HTML("
                  .main-header {
                    position: fixed; /* Fixed Header (stay in place on scroll and position relative to viewport) */
                    width: 100%; /* Ensure the header spans the entire width */
                    z-index: 1030; /* Ensure the header is above other elements */
                  }
                  .main-sidebar {
                    position: fixed; /* Fixed Sidebar (stay in place on scroll and position relative to viewport) */
                    height: 100%;
                    overflow-y: auto; /* Scrollable contents if viewport is shorter than content. */
                  }
                .content-wrapper, .right-side, .main-footer {
                  margin-top: 40px; /* Adjust based on the header height */
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
              h3("Parameter Percentile Map", style = "text-align: center;"),
              tags$p("The map displays the percentile distribution of average metal concentrations based on user-selected parameters and sample fractions (filters on left side). Each point represents a percentile ranking, highlighting areas of varying concentration levels within the chosen jurisdictions.", 
                     style = "padding: 4px; font-size: 14px; text-align:center;"),
              fluidRow( selectInput("sampleFraction", "Select Sample Fraction", choices = choices_frac),
                        selectInput("parameter", "Select Parameter", choices = sort(unique(sj_data$CharacteristicName))),
                        selectInput("jurisdiction", "Select Jurisdiction(s)", choices = choices_j1, selected = choices_j1, multiple = TRUE),
                        dateRangeInput("dateFilter","Date Range",start="2015-01-01"),
                        checkboxInput("nonDetect", "Include NonDetect Values", value = TRUE),
                        bsTooltip(id = "nonDetect", title = "Include measurements where no detection was noted.", placement = "right", trigger = "hover"),
                        checkboxInput("nonSJ_Site", "Include Non-SJ WIIN Project Sites", value = FALSE),
                        bsTooltip(id = "nonSJ_Site", title = "Include sites not in SJ WIIN 39 Site List but still in SJ watershed.", placement = "right", trigger = "hover")
              ),
              leafletOutput("sj_site_map", height = "600px"),
              tags$p("Click on any site on the map to see the corresponding time series plot for that site.", style = "padding: 8px; font-size: 14px; text-align:center;"),
              tags$div(style = "height: 20px;"),
              plotlyOutput("timeSeriesPlot")),
      # tabItem(tabName = "param_sum",
      #         fluidRow(
      #           column(12,
      #                  h3("Parameter Overview & Time Series", style = "text-align: center;"),
      #                  withSpinner(textOutput("loadingPlot"), type = 8, color = "#000000", size = 2),
      #           ),
      #         plotOutput("param_facet", height = "3800px", width = "1000px")
      #                 )
      #         ),
      tabItem(tabName = "jurisdiction_assessments",
           fluidRow(
             column(12,h3("Jurisdiction Specific Criteria", style = "text-align: center;"),p("This tab allows you to explore the data by applying Jurisdiction specific criteria. The elements in this tab are independent from the filters on the sidebar. Use filters within this window to explore data.")),
                  fluidRow(column(4,selectInput("jurisdiction_2", "Jurisdiction", choices = choices_j1, multiple = F,selected = "Colorado")),
                           column(4, selectInput("parameter_2", "Parameter(s)", choices = c(),multiple = T,selected = "Aluminum")),
                            column(4, selectInput("sampleFraction_2", "Sample Fraction(s)", choices = c(),multiple=T, selected = "Total"))
                             ),
                  shinyjqui::jqui_resizable(bsCollapse(multiple=T,open=1,
                       bsCollapsePanel(list(icon('plus-circle'), icon('map-marked-alt'),"Review map"), value=1,
                             leafletOutput("assessment_map_juris", height="600px", width="100%")
                       ),
                       bsCollapsePanel(list(icon('plus-circle'), icon('table'), p("Exceedance Summary Table", em(" - Exceedance based off of selected jurisdiction, parameter and fractions"))), value=3,
                                       fluidRow(dataTableOutput("exceedance_table"))
                       ),
                       bsCollapsePanel(list(icon('plus-circle'), icon('line-chart'), p("Site Assessment Plots", em(" - Scatter plots against jurisdiction criterias."))), value=2,
                                       fluidRow(selectInput("plot_site", "Select Site", choices = NULL,multiple = F)),
                                       fluidRow(plotlyOutput("jurisdiction_timeSeriesPlot")) )
      ) ) ) )
      )
  
        )
  )#UI END


server <- function(input, output, session) {
  
  # Debounced reactive for data processing
    exceedance_criteria <- debounce(
      reactive({
        req(input$sampleFraction_2)
        req(input$parameter_2)
        subset_sj <- sj_data %>%
          filter(ResultSampleFractionText %in% input$sampleFraction_2,
                 CharacteristicName %in% input$parameter_2,
                 Jurisdiction == input$jurisdiction_2)
        
        
        subset_criteria <- combined_all_criteria3 %>%
          filter(ResultSampleFractionText %in% input$sampleFraction_2,
                 CharacteristicName %in% input$parameter_2,
                 Jurisdiction %in% input$jurisdiction_2)

        subset_sj <- merge(subset_sj, subset_criteria) 
        
        
        subset_sj <- subset_sj%>%
          mutate(flag_missing_hardness = (sapply(CF, depends_on_hardness) | sapply(Criterion_Formula_mgL, depends_on_hardness)) & is.na(hardness)) %>%
          rowwise() %>%
          mutate(
            CF_calculated = ifelse(sapply(CF, depends_on_hardness) & !is.na(hardness),
                                   evaluate_formula(CF, hardness), as.numeric(CF)),
            criteria_calculated = ifelse(!is.na(Criterion_Formula_mgL),
                                         eval(parse(text = str_replace_all(Criterion_Formula_mgL, 
                                                                           c("CF" = as.character(CF_calculated),
                                                                             "hardness" = as.character(hardness))))), Criteria_mg_L),
            exceed = ifelse(SJ_ResultValue > criteria_calculated, 1, 0),
            exceed = ifelse(is.na(exceed), 0, exceed),
            missing_hardness = ifelse(flag_missing_hardness == TRUE, 1, 0)) %>%
          ungroup()
        
        exceedance_summary <- subset_sj %>%
          group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, CharacteristicName, Screening.Criteria, Use, Jurisdiction, ResultSampleFractionText) %>%
          summarize(missing_hardness = sum(missing_hardness),
                    sample_count = n() - missing_hardness,
                    exceedance = sum(exceed),
                    exceed_perc = exceedance / sample_count,
                    .groups = 'drop')%>%
          arrange(desc(exceed_perc))
        
        list(subset_sj = subset_sj, exceedance_summary = exceedance_summary)
      }), 1500)
    
  
  filteredData <- debounce(reactive({

    start=as.Date(input$dateFilter[1])
    end=as.Date(input$dateFilter[2])
    data <- sj_data %>%
      filter(ResultSampleFractionText %in% input$sampleFraction,
             if (input$nonDetect) TRUE else SJ_ND != "Yes",
             if (input$nonSJ_Site) TRUE else SJ_PROJECT_SITE!="No")%>%
      filter(stringr::str_detect(Jurisdiction, paste(input$jurisdiction, collapse = "|")))%>%
      filter(ActivityStartDate>start &ActivityStartDate<end)
    data
  }),800)
  
  output$sj_site_map <- renderLeaflet({
    aggregated_data <-filteredData()

    aggregated_data <- aggregated_data %>%
      filter(CharacteristicName %in% input$parameter)%>%
      group_by(CharacteristicName, MonitoringLocationIdentifier, ResultSampleFractionText) %>%
      summarise(
        AverageValue = mean(SJ_ResultValue, na.rm = TRUE),  
        Latitude = first(LatitudeMeasure),  # 
        Longitude = first(LongitudeMeasure),  # 
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
      addLegend(position = "topright",
                labels = c("Above 75th Percentile", "50th to 75th Percentile", "25th to 50th Percentile", "Below 25th Percentile"),
colors = c("red","orange","yellow","green"))%>%
      addPolygons(data=sj_tribes_final,group = "Tribes",fillOpacity = 0.3,color="blue",weight = 2,options = pathOptions(pane = "Tribes"),
                  popup=paste0(
                    "Tribe Name: ", sj_tribes_final$NAME)) 

  })#End of renderLeaflet

  output$assessment_map_juris <- renderLeaflet({
    exceedance_summary <-exceedance_criteria()$exceedance_summary
    
    avg_exceedance = exceedance_summary%>%
      filter(!is.na(exceed_perc))%>%
      group_by(MonitoringLocationIdentifier,LatitudeMeasure,LongitudeMeasure)%>%
      summarise(avg_exceedance = mean(exceed_perc))
    
    # Create a color gradient from yellow to red
    color_function <- col_numeric(palette = c("yellow", "red"), domain = range(avg_exceedance$avg_exceedance, na.rm = TRUE))
    
    avg_exceedance = avg_exceedance%>%
     mutate(color = color_function(avg_exceedance))
    
    #****** Add a box area to view based off of the sites in aggregated_data
    avg_exceedance_sf <- st_as_sf(avg_exceedance, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)
    view=sf::st_bbox(avg_exceedance_sf)
    leaflet()%>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
      addMapPane("Sites", zIndex = 415)%>%
      addMapPane("Tribes", zIndex = 410)%>%
      addCircleMarkers(data=avg_exceedance_sf,color = ~color, radius = 8,
                       layerId = ~MonitoringLocationIdentifier,fillOpacity = 0.8,label = ~MonitoringLocationIdentifier,group="Sites",options = pathOptions(pane = "Sites"))%>%
      addLegend(position = "topright",
                labels = c("Lower Avg Exceedance", "Higher Avg Exceedance"),
                colors = c("yellow","red"))%>%
      addPolygons(data=sj_tribes_final,group = "Tribes",fillOpacity = 0.3,color="blue",weight = 2,options = pathOptions(pane = "Tribes"),
                  popup=paste0(
                    "Tribe Name: ", sj_tribes_final$NAME))%>%
    fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4]))

  })#End of renderLeaflet
  
  # observe(input$sj_site_map_marker_click{
     #******FIX Change color on clicked site
  # })
  
  
  output$timeSeriesPlot <- renderPlotly({
    
    #******FIX ADD Criteria lines to plot using Filters above plot, Jurisdiction, Use 
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
  }) #End of plotly
  
  # output$param_facet <- renderPlot({
  #   param_dat <- filteredData()  
  #   param_dat=param_dat%>%
  #     select(CharacteristicName,ActivityStartDate,SJ_ResultValue,ResultSampleFractionText)%>%
  #     distinct()
  # 
  #   isPlotLoading(FALSE)
  #   ggplot(param_dat, aes(x = ActivityStartDate, y = SJ_ResultValue,color=CharacteristicName)) +
  #     geom_point(size = 3, alpha = 0.6) +
  #     labs(x = "Sample Dates",
  #          y = "mg/L") +
  #     facet_wrap(~CharacteristicName, scales = "free",ncol = 1) +  # Organize by rows
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #     scale_x_date(date_breaks = "1 year", date_labels = "%Y") 
  # })
  
  # isPlotLoading <- reactiveVal(TRUE)
  # output$loadingPlot <- renderText({
  #   if(isPlotLoading()) {
  #     "Loading plot..."
  #   } else {
  #     "This section displays time series data for various parameters across the selected sample fractions and jurisdictions.
  #     Note that while the plots dynamically update based on your selections for sample fractions and jurisdictions, they do not filter by the specific parameter name chosen. 
  #     Instead, they provide a comprehensive overview of temporal trends for all parameters within the selected filters."
  #   }
  # })
  
  output$exceedance_table <- renderDataTable({
    req(exceedance_criteria()$exceedance_summary)
    t <- exceedance_criteria()$exceedance_summary%>%
      rename(Site_ID = MonitoringLocationIdentifier, Latitude=LatitudeMeasure, Longitude=LongitudeMeasure, Parameter=CharacteristicName, Use_Category=Screening.Criteria, Fraction=ResultSampleFractionText)
    datatable(t,options = list(scrollX = TRUE, pageLength = 5), selection = 'single')
  })
  
  observe({
    site_choices <- unique(exceedance_criteria()$subset_sj$MonitoringLocationIdentifier)
    updateSelectInput(session, "plot_site", choices = site_choices, selected = site_choices[1])
  })
  
  observe({
    req(input$jurisdiction_2)  # Ensure jurisdiction input is available
    j_data <- combined_all_criteria3[combined_all_criteria3$Jurisdiction == input$jurisdiction_2, ]
    frac_choices <- sort(unique(j_data$ResultSampleFractionText))
    param_choices <- sort(unique(j_data$CharacteristicName))
    updateSelectInput(session, "parameter_2", choices = param_choices)
  })
  
  observe({
    req(input$parameter_2)
    j_data <- combined_all_criteria3%>% filter(Jurisdiction == input$jurisdiction_2 & CharacteristicName %in% input$parameter_2)
    frac_choices <- sort(unique(j_data$ResultSampleFractionText))
    updateSelectInput(session, "sampleFraction_2", choices = frac_choices)
  }) 
  
  # Update plot_site input based on table selection
  observeEvent(input$exceedance_table_rows_selected, {
    selected_row <- exceedance_criteria()$exceedance_summary[input$exceedance_table_rows_selected,]
    if (nrow(selected_row) > 0) {
      updateSelectInput(session, "plot_site", selected = selected_row$MonitoringLocationIdentifier[[1]])
    }
   
  })
  
  output$jurisdiction_timeSeriesPlot <- renderPlotly({
    plot_data <- exceedance_criteria()$subset_sj

    plot_data <- plot_data[plot_data$MonitoringLocationIdentifier == input$plot_site, ]

    # Generate the time series plot
    p <- plot_ly(data = plot_data, x = ~ActivityStartDate, y = ~SJ_ResultValue, type = 'scatter', mode = 'markers',marker=list(size=10),name = ~MonitoringLocationIdentifier) %>%
      layout(title = paste(input$sampleFraction_2,input$parameter_2,"for",input$plot_site,sep=" "),
             xaxis = list(title = "Sample Dates"),  # Set x-axis label
             yaxis = list(title = "mg/L")  # Set y-axis label
      )
    # Add lines for criteria_calculated, grouped by Use
    unique_uses <- unique(plot_data$Use)
    for(use in unique_uses) {
      criteria_data <- plot_data[plot_data$Use == use, ]
      p <- p %>%
        add_trace( x = ~ActivityStartDate, y = ~criteria_calculated, data = criteria_data, type = 'scatter',
          mode = 'lines+markers', marker = list(size = 6),name = paste(use, "Criteria"),
          line = list(dash = 'dash', width = 2)
        )
    }
    p
  }) #End of plotly
  
  #******FIX Download data button 
  #*Ask group what kind of data would they want downloaded what would they want to dig into?
  #*Possibly in the main side-bar to download filtered data? OR create another tabItem() with filterable data download..

}


# Run the app
shinyApp(ui, server)

