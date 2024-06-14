
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(data.table)
library(DT)
library(dtplyr)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinycssloaders)
library(sf)
library(wqTools)
library(bigrquery)

# Set the environment variable
#Sys.setenv(BIGQUERY_AUTH_FILE = "/Users/alanochoa/Documents/GitHub/Data_Request/HF_and_IR_Data_Assessments/alan_local_keys.json")
bq_auth(path = "alan_local_keys.json")
project_id <- 'ut-deq-highfreq-dev'


au_poly <- wqTools::au_poly #******FIX Change this to one that has the summary of assessments.


# au_poly <- au_poly%>%filter(ASSESS_ID%in%bq_wqp_near_sites$ASSESS_ID |ASSESS_ID%in%bq_sites$ASSESS_ID)

load("wqp_hf_app.RData") #dt_trimmed_acc_data,trimmed_masterSite,bq_sites ,bq_wqp_near_sites


trimmed_lt_asmnt = lazy_dt(dt_trimmed_acc_data)%>%
  select(ASSESS_ID,MonitoringLocationIdentifier,R3172ParameterName,ResultIdentifier,ActivityStartDate,ActivityStartTime.Time,
         IR_Value,IR_Unit,Year,ActivityDateTime)%>%
  distinct()

param_options <- unique(dt_trimmed_acc_data$R3172ParameterName)

bq_sites <- bq_sites %>% filter(!is.na(ASSESS_ID))

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Comparing HF BigQuery Data and Assessed WQP Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Site Comparison", tabName = "Site_map", icon = icon("map"),selected = TRUE)#,
      #menuItem("Time Series", tabName = "time_series", icon = icon("line-chart"))
    ),
    # selectInput("useCriteria", "Select Use Criteria", choices = unique(trimmed_lt_asmnt$BeneficialUse)),
    selectInput("parameter", "Select Parameter", choices = sort(param_options), selected = "pH"),
    #checkboxInput("noAU", "Include No AU BQ Sites", value = FALSE),
    dateRangeInput("dateFilter","Date Range",start="2015-01-01")
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
            "))
    ),
    tabItems(
      tabItem(tabName = "Site_map",
              withSpinner(leafletOutput("site_map", height = "600px"), type = 8, color = "#000000", size = 2),
              tags$div(".....", style = "padding: 8px; font-size: 14px; text-align:center;"),
              textOutput("BQ_Estimate"),
              tags$div(style = "height: 20px;"),
              plotlyOutput("timeSeriesPlot")),
      tabItem(tabName = "param_sum",
              fluidRow(
                column(12,
                       h3("Parameter Summary", style = "text-align: center;"),
                       withSpinner(textOutput("loadingPlot"), type = 8, color = "#000000", size = 2)
                ),
                plotOutput("param_facet", height = "3800px", width = "1000px")
              )
      )
    )
  )
)#UI END

#The following columns may be helpful for filtering, displaying data and criteria.. 
# ParameterQualifier,FrequencyCombined,FrequencyNumber,NumericCriterion,DailyAggFun,AsmntAggPeriod,CriterionType

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  output$site_map <- renderLeaflet({  #trimmed_masterSite,bq_sites
    #****** Add a box area to view based off of the sites or selected AU?
    leaflet()%>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
      addMapPane("AU", zIndex = 410)%>%
      addMapPane("Highlight", zIndex = 413)%>%
      addMapPane("Markers", zIndex = 415)%>%
      addMapPane("AssessMarkers", zIndex = 420)%>%
      addCircleMarkers(data=trimmed_masterSite,lng=~LongitudeMeasure,lat=~LatitudeMeasure, color = "orange",fillColor = "yellow", radius = 5,
                       layerId = ~MonitoringLocationIdentifier,fillOpacity = 0.8,label = ~MonitoringLocationIdentifier,group="AssessedSites",
                       options = pathOptions(pane = "AssessMarkers"))%>%
      addCircleMarkers(data = bq_sites, lng = ~LongitudeMeasure, lat = ~LatitudeMeasure, fillColor = ~assess_color, radius = 5,
                       layerId = ~HF_Location_ID, fillOpacity = 0.8, label = lapply(bq_sites$Label,HTML), group = "BQSites", 
                       options = pathOptions(pane = "Markers"))%>%
      addPolygons(data=au_poly,group = "AU",fillOpacity = 0.3,color="pink",weight = 2,options = pathOptions(pane = "AU"),layerId = ~ASSESS_ID,
                  popup=paste0("AU Name: ", au_poly$AU_NAME)#****** FIX POPUP ADD Assessment Summary
                  )%>%
    leaflet::addLayersControl(position ="topleft",overlayGroups = c("AU","BQSites","AssessedSites"),
    options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))%>%
    addLegend(position = "topright",
              labels = c("BQ Site - Unassessed AU", "BQ Site - Assessed AU", "Assessed Site"),
              colors = c("red","blue","yellow"))
  })#End of renderLeaflet
  
  
  selected_au <- reactiveVal(NULL)
  selected_sites <- reactiveVal(NULL)
  query_results <- reactiveVal(NULL)
  
  # React to clicking AU
  observeEvent(input$site_map_shape_click, {
    selected_au <- input$site_map_shape_click$id

    # Make site lists in selected AU
    selected_sites(bq_wqp_near_sites[bq_wqp_near_sites$ASSESS_ID==selected_au,])
    
    # Highlight Selected AU - Do we also want to highlight and grow the selected BQ and IR sites?
    sel_au_poly <- au_poly %>% filter(ASSESS_ID == selected_au)
      leafletProxy("site_map") %>%
        clearGroup("Highlight") %>%
        addPolygons(data = sel_au_poly, group = "Highlight", fillOpacity = 0.3, color = "#FF00FF", weight = 2, 
                    options = pathOptions(pane = "Highlight"), layerId = ~ASSESS_ID, popup = ~paste0("AU Name: ", sel_au_poly$AU_NAME))
  })
  
  
  observe({
    req(selected_sites())
      if(nrow(selected_sites())>0){
        hf_id_string <- paste(selected_sites()$HF_Site, collapse = ", ")
        
        sql <- sprintf("SELECT HF_Location_ID,Measurement_Description, Average_Water_Measurement,MLID,Location_Desc, DateTime_Local,Sample_Year FROM `ut-deq-highfreq-dev.High_Frequency_Flat_Views.looker_flatview_all_data`
                        WHERE Measurement_Description IN ('Water Temperature', 'pH', 'Dissolved Oxygen') AND HF_Location_ID IN (%s)", hf_id_string)
        sample_query_job <- bq_project_query(project_id, sql)
        
        # Download data for selected site
        sample_result <- bq_table_download(sample_query_job)
        query_results(sample_result)
        
        # Col: HF_Location_ID,Measurement_Description, Average_Water_Measurement,MLID,Location_Desc, DateTime_Local
        output$BQ_Estimate <- renderPrint({
          print(unique(sample_result$Measurement_Description))
          nrow(sample_result)
        })
        
      }
    else {
      output$BQ_Estimate <- renderPrint({
        cat("No sites selected or found.")
    })
    }
       }) #End of Query Observer
  

  
  output$timeSeriesPlot <- renderPlotly({
    req(selected_sites())
    req(query_results())##******FIX If Data takes a while to download, and selected_sites() is available
    #instantly this may be causing the issue. Can we do something similar as leaflet_proxy? 
    # First plot ir_plot_data then update the plot when query_results available?
     #*******FIX Have a separate table that has the summary assessment table that can then be viewed.
      if (nrow(selected_sites())>0){
        mlids <-unique(selected_sites()$MonitoringLocationIdentifier)
        # Columns for trimmed_lt_asmnt: (ASSESS_ID,MonitoringLocationIdentifier,R3172ParameterName,ResultIdentifier,ActivityStartDate,ActivityStartTime.Time,
        #IR_Value,IR_Unit )
        
        #Not sure if adding Date Filters is useful or not.. It may be when pulling data from BQ.
        start=as.Date(input$dateFilter[1])
        end=as.Date(input$dateFilter[2])
        
        # 
        ir_plot_data <- as.data.frame(trimmed_lt_asmnt) %>% # Plotly may only accept data frames..
          filter(MonitoringLocationIdentifier %in% mlids,
                 R3172ParameterName==input$parameter) #%>%
          #mutate(ActivityStartDate = as.Date(ActivityStartDate))
        
        site_years <-unique(as.character(format(ir_plot_data$ActivityDateTime,"%Y")))
        str(site_years)
        bq_results <- as.data.frame(query_results())
        bq_results$Sample_Year <-as.character(bq_results$Sample_Year)
        print(paste("BQ Row Counts",nrow(bq_results)))
        print(unique(bq_results$Sample_Year))
        str(bq_results)
        
       
              
        bq_results <- bq_results %>% 
          merge(.,param_translation)%>%
          select(-Measurement_Description)%>%
          rename(IR_Value = Average_Water_Measurement,MonitoringLocationIdentifier = Location_Desc,
                 ActivityDateTime = DateTime_Local)%>%
          filter(Sample_Year%in%site_years,
                 R3172ParameterName==input$parameter)
       
         print(paste("BQ Row Counts",nrow(bq_results)))

         
        bq_results$ActivityDateTime<- as.POSIXct(bq_results$ActivityDateTime, format="%Y-%m-%dT%H:%M:%S")
        print(paste("BQ Row Counts",nrow(bq_results)))
        
        print(paste("BEFORE ROW COUNTS ",nrow(ir_plot_data)))
        ir_plot_data <- merge(ir_plot_data,bq_results,all.x=T,all.y=T)
        print(paste("AFTER ROW COUNTS ",nrow(ir_plot_data)))
        
        
        # Generate the time series plot
        #******FIX ADD shade/highlight between 25th and 75th percentiles of BQ data to see where along the WQP data falls under.
        p_units = unique(ir_plot_data$IR_Unit)
        plot_ly(ir_plot_data, x = ~ActivityDateTime, y = ~IR_Value, type = 'scatter', mode = 'markers',color = ~MonitoringLocationIdentifier,  # Differentiate by MonitoringLocationIdentifier
                text = ~paste("Location ID:", MonitoringLocationIdentifier),marker=list(size=10)) %>%
          layout(title = paste("Time Series of",input$parameter,"for",sep=" "),
                 xaxis = list(title = "Sample Dates"),  # Set x-axis label
                 yaxis = list(title = paste("TEST",p_units,sep = " "))  # Set y-axis label
          )
      }
    else {print("No sites in this AU.")}
 
  }) # END OF RENDER PLOTLY



  
  ##******FIX Something similar to this plot could be useful to display interesting findings where WQP 
  ##*samples differ drastically from HF Site data
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
  #     "This plot shows the time series data for all parameters. These plots do not respond to the parameter name filter on the left-hand side."
  #   }
  # })
  
  #******FIX Download data button 
  #*Ask group what kind of data would they want downloaded what would they want to dig into?
  #*Possibly in the main side-bar to download filtered data? OR create another tabItem() with filterable data download..
  
  
  
} #END OF SERVER


# Run the app
shinyApp(ui, server)
