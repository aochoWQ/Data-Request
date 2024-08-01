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
library(writexl)
library(markdown)
library(shinyalert)



source("helper_functions.R") # Has a few functions for calculating criteria.
load("SJ_dashboard_data.RData") # Created in SJ_data_download.Rmd (Line345) to include results_SJ_metals_only2, , criteria,  polygon_shapefiles

combined_all_criteria3=combined_all_criteria4
results_SJ_metals_only2 = results_SJ_metals_only3


sj_data <- results_SJ_metals_only2[results_SJ_metals_only2$UnitFlag=="Accept",] #This filters out Sediment data. Assuming we don't want to use sediment data.
sj_data$id <- sj_data$MonitoringLocationIdentifier #Adding column for clicked map id?
choices_j1 <- sort(unique(sj_data$Jurisdiction[sj_data$Jurisdiction!="NA"]))
choices_frac <- unique(sj_data$ResultSampleFractionText)
choices_char <- sort(unique(sj_data$CharacteristicName))

#******FIX sj_data has multiple jurisdictions.. do we want to simplify to one jurisdiction?


# Define UI
source("helper_functions.R") # Has a few functions for calculating criteria.
load("SJ_dashboard_data.RData") # Created in SJ_data_download.Rmd (Line345) to include results_SJ_metals_only2, , criteria,  polygon_shapefiles

combined_all_criteria3=combined_all_criteria4
results_SJ_metals_only2 = results_SJ_metals_only3

sj_data <- results_SJ_metals_only2[results_SJ_metals_only2$UnitFlag=="Accept",] #This filters out Sediment data. Assuming we don't want to use sediment data.
sj_data$id <- sj_data$MonitoringLocationIdentifier #Adding column for clicked map id?
choices_j1 <- sort(unique(sj_data$Jurisdiction[sj_data$Jurisdiction!="NA"]))
choices_frac <- unique(sj_data$ResultSampleFractionText)
choices_char <- sort(unique(sj_data$CharacteristicName))

# Define UI
ui <- navbarPage(
  title = "San Juan Basin Metals Analysis Dashboard", 
  header = tags$head(
    tags$style(HTML("
      .navbar-default {
        background-color: #216C8A; /* Blue color for the navbar */
        border-color: #216 C8A;
      }
      .navbar-default .navbar-brand {
        color: white;
      }
      .navbar-default .navbar-brand:hover {
        color: lightgray;
      }
      .navbar-default .navbar-nav > li > a {
        color: white;
      }
      .navbar-default .navbar-nav > li > a:hover {
        color: lightgray;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        color: white;
        background-color: #3CABD8; /* Different color for the selected tab */
      }
      .split-layout-div {
        white-space: normal;
        overflow: hidden;
        word-wrap: break-word;
      }
    "))
  ),
  tabPanel("Watershed Summary Map",
           fluidPage(
             h3("Watershed Summary Map", style = "text-align: center;"),
             tags$p("The map displays the percentile distribution of average metal concentrations based on user-selected parameters and sample fractions (filters on left side). Each point represents a percentile ranking, highlighting areas of varying concentration levels within the chosen jurisdictions.", 
                    style = "padding: 4px; font-size: 14px; text-align:center;"),
             column(2, 
                    fluidRow(selectInput("sampleFraction", "Select Sample Fraction", choices = choices_frac)),
                    fluidRow(selectInput("parameter", "Select Parameter", choices = sort(unique(sj_data$CharacteristicName)))),
                    fluidRow(dateRangeInput("dateFilter","Date Range",start="2015-01-01")),
                    fluidRow(checkboxInput("nonDetect", "Include NonDetect Values", value = TRUE)),
                    bsTooltip(id = "nonDetect", title = "Include measurements where no detection was noted.", placement = "right", trigger = "hover"),
                    fluidRow(checkboxInput("nonSJ_Site", "Include Non-SJ WIIN Project Sites", value = FALSE)),
                    bsTooltip(id = "nonSJ_Site", title = "Include sites not in SJ WIIN 39 Site List but still in SJ watershed.", placement = "right", trigger = "hover")
             ),
             column(10,
                    leafletOutput("sj_site_map", height = "600px"),
                    tags$p(""),
                    tags$p("Click on any site on the map to see the corresponding time series plot for that site.", style = "color: red; padding: 8px; font-size: 16px; text-align:center;"),
                    tags$div(style = "height: 20px;"),
                    plotlyOutput("timeSeriesPlot"))
           )
  ),
  tabPanel("Jurisdiction Specific Criteria",
           fluidPage(
             splitLayout(
               cellWidths = c("20%", "80%"), # Adjust the widths as needed
               cellArgs = list(style = "padding: 10px"), # Adding some padding for better spacing
               div(class = "split-layout-div",
                   p("Select option for ALL dropdown filters to display map. To remove options click on them and then click backspace/delete.", style = "color: red;text-align: center;"),
                   selectInput("jurisdiction_2", "Jurisdiction", choices = choices_j1, multiple = FALSE, selected = "Colorado"),
                   selectInput("parameter_2", "Parameter(s)", choices = c(), multiple = TRUE, selected = "Aluminum"),
                   selectInput("sampleFraction_2", "Sample Fraction(s)", choices = c(), multiple = TRUE),
                   selectInput("criteriaUse_2", "Uses", choices = c(), multiple = TRUE),
                   checkboxInput("nonDetect_2", "Include NonDetect Values", value = FALSE),
                   checkboxInput("nonSJ_Site_2", "Include Non-SJ WIIN Project Sites", value = FALSE),
                   dateRangeInput("dateFilter_2", "Date Range", start = "2015-01-01")
               ),
               div(class = "split-layout-div",
                   h3("Jurisdiction Specific Criteria", style = "text-align: center;"),
                   p("This tab allows you to explore the data by applying Jurisdiction specific criteria. The elements in this tab are independent from the filters on the sidebar. Use filters within this window to explore data."),
                   shinyjqui::jqui_resizable(
                     bsCollapse(
                       multiple = TRUE,
                       bsCollapsePanel(
                         list(icon('plus-circle'), icon('map-marked-alt'), "Review map"), value = 1,
                         leafletOutput("assessment_map_juris", height = "600px", width = "100%")
                       ),
                       bsCollapsePanel(
                         list(icon('plus-circle'), icon('table'), p("Exceedance Summary Table", em(" - Based off of selected jurisdiction, parameter, fractions, and uses"))), value = 3,
                         dataTableOutput("exceedance_table")
                       ),
                       bsCollapsePanel(
                         list(icon('plus-circle'), icon('line-chart'), p("Site Assessment Plots", em(" - Scatter plots against use criterias."))), value = 2,
                         fluidRow(
                           column(4, selectInput("plot_site", "Select Site", choices = NULL, multiple = FALSE)),
                           column(2, selectInput("plot_param", "Select Parameter", choices = NULL, multiple = FALSE))
                         ),
                         plotlyOutput("jurisdiction_timeSeriesPlot")
                       ),
                       bsCollapsePanel(
                         list(icon('plus-circle'), icon('line-chart'), p("Download Data", em(" - based off of user selected filters."))), value = 2,
                         tags$p(
                           "The download file includes data for ALL SITES in the filtered dataset. The Excel (.XLSX) file has two tabs: 'Exceedance Summary' and 'Sample Data'. 
                  The 'Exceedance Summary' tab contains sample counts and exceedance values based on the selected Jurisdiction, Parameter, Fractions, and Uses. 
                  The 'Sample Data' tab includes raw data with uses criteria and additional metadata.",
                           style = "padding: 8px; font-size: 14px; text-align:center;"
                         ),
                         downloadButton("downloadData", "Download Data")
                       )
                     )
                   )
               )
             )
           )
  ),
  tabPanel("Help & Info",
           fluidPage(
             fluidRow(
               box(status = "primary", solidHeader = TRUE, width = 12,
                   includeMarkdown("how_to.md")
               )
             )
           )
  )
)#END UI


server <- function(input, output, session) {
  
  ##TAB 1 Watershed Summary
    
  filteredData <- debounce(reactive({
    start=as.Date(input$dateFilter[1])
    end=as.Date(input$dateFilter[2])
    data <- sj_data %>%
      filter(ResultSampleFractionText %in% input$sampleFraction,
             if (input$nonDetect) TRUE else SJ_ND != "Yes",
             if (input$nonSJ_Site) TRUE else SJ_PROJECT_SITE!="No")%>%
      filter(ActivityStartDate>start &ActivityStartDate<end)
    data
  }),800)
  
  quartiles <- reactiveVal()
  
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
    print(length(unique(aggregated_data$MonitoringLocationIdentifier)))
    
    quartiles <- quantile(aggregated_data$AverageValue, probs = c(0.25, 0.5,0.75), na.rm = TRUE)
    quartiles(quartiles)
    
    #****** Add a box area to view based off of the sites in aggregated_data
    aggregated_data_sf <- st_as_sf(aggregated_data, coords = c("Longitude", "Latitude"), crs = 4326)
    view=sf::st_bbox(aggregated_data_sf)
    
    leaflet()%>%
    addProviderTiles("Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
      addMapPane("Sites", zIndex = 415)%>%
      addMapPane("Tribes", zIndex = 410)%>%
      addCircleMarkers(data=aggregated_data,lng=~Longitude,lat=~Latitude, color = ~color, radius = 8,
                       layerId = ~MonitoringLocationIdentifier,fillOpacity = 0.8,label = ~MonitoringLocationIdentifier,group="Sites",options = pathOptions(pane = "Sites"))%>%
      addLegend(position = "topright",
                labels = c("Above 75th Percentile", "50th to 75th Percentile", "25th to 50th Percentile", "Below 25th Percentile","Tribal Lands"),
colors = c("red","orange","yellow","green","blue"))%>%
      addPolygons(data=sj_tribes_final,group = "Tribes",fillOpacity = 0.05,color="blue",weight = 2,options = pathOptions(pane = "Tribes"),
                  popup=paste0(
                    "Tribe Name: ", sj_tribes_final$NAME))%>%
      fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) 
  })#End of renderLeaflet

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
    plot_data <- plot_data[plot_data$MonitoringLocationIdentifier == clicked_id, ]%>%
      filter(CharacteristicName %in% input$parameter)
    
    q25 <- quartiles()[1]
    q50 <- quartiles()[2]
    q75 <- quartiles()[3]
    # Generate the time series plot
    plot_ly(plot_data, x = ~ActivityStartDate, y = ~SJ_ResultValue, type = 'scatter',name="Data Points", mode = 'markers',marker=list(size=10)) %>%
      add_lines(x = ~ActivityStartDate, y =q25,  color = I("yellow"),marker = list(size = 2), name = "1st Quartile") %>%
      add_lines(x = ~ActivityStartDate, y =q50,  color = I("orange"),marker = list(size = 2), name ="2nd Quartile") %>%
      add_lines(x = ~ActivityStartDate, y =q75,  color = I("red"),marker = list(size = 2), name = "3rd Quartile") %>%
      layout(title = paste("Time Series of",input$sampleFraction,input$parameter,"for",clicked_id,sep=" "),
             xaxis = list(title = "Sample Dates"),  # Set x-axis label
             yaxis = list(title = "mg/L")  # Set y-axis label
      )
  }) #End of plotly
  
  
  
  
  ## TAB 2 Jurisdiction Tab
  # Debounced reactive for data processing
  exceedance_criteria <- debounce(
    reactive({
      req(input$sampleFraction_2)
      req(input$parameter_2)
      req(input$criteriaUse_2)
      
      start=as.Date(input$dateFilter_2[1])
      end=as.Date(input$dateFilter_2[2])
      
      subset_sj0 <- sj_data %>%
        filter(ResultSampleFractionText %in% input$sampleFraction_2,
               CharacteristicName %in% input$parameter_2,
               Jurisdiction == input$jurisdiction_2,
               ActivityStartDate>start &ActivityStartDate<end,
               if (input$nonDetect_2) TRUE else SJ_ND != "Yes",
               if (input$nonSJ_Site_2) TRUE else SJ_PROJECT_SITE!="No")
      
      subset_criteria <- combined_all_criteria3 %>%
        filter(ResultSampleFractionText %in% input$sampleFraction_2,
               CharacteristicName %in% input$parameter_2,
               Jurisdiction %in% input$jurisdiction_2,
               Use %in% input$criteriaUse_2)
      
      subset_sj <- merge(subset_sj0, subset_criteria) 
      
      print(nrow(subset_sj))
      
      if (nrow(subset_sj)>0) {
        #Are all calculated criteria in the ug/L? If so then they will need to be divided by 1000 to convert to mg/L
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
            missing_hardness = ifelse(flag_missing_hardness == TRUE, 1, 0),
            exceed_crit_below__half_mdl = ifelse(exceed==1 & SJ_ND=="Yes",1,0)) %>%
          ungroup()
        print(length(unique(subset_sj$MonitoringLocationIdentifier)))
        
        
        exceedance_summary <- subset_sj %>%
          group_by(MonitoringLocationIdentifier, CharacteristicName, Screening.Criteria, Use, ResultSampleFractionText, LatitudeMeasure, LongitudeMeasure,Jurisdiction) %>%
          summarize(missing_hardness = sum(missing_hardness),
                    mdl_concern_n = sum(exceed_crit_below__half_mdl),
                    sample_count = n() - missing_hardness,
                    exceedances = sum(exceed),
                    exceed_perc = round((exceedances / sample_count) * 100, 2),
                    .groups = 'drop')%>%
          arrange(desc(exceed_perc))%>%
          select(MonitoringLocationIdentifier,CharacteristicName,Screening.Criteria, Use, ResultSampleFractionText,exceed_perc,sample_count,exceedances,exceed_perc,mdl_concern_n,missing_hardness, LatitudeMeasure, LongitudeMeasure,Jurisdiction)
        
        list(subset_sj = subset_sj, exceedance_summary = exceedance_summary)
      }
      else {
        sj_d <- sj_data %>%
          filter(CharacteristicName %in% input$parameter_2,
                 Jurisdiction == input$jurisdiction_2)
        
        # Create the table output as a character vector
        table_output <- paste(capture.output(table(sj_d$ResultSampleFractionText)), collapse = "<br>")
        
        shinyalert("No Data",html = TRUE, HTML(paste0(
          "<p>There are no matching data for the criteria filters selected.</p>",
          "<p>Data row count: ", nrow(subset_sj0), "</p>",
          "<p>Criteria row count: ", nrow(subset_criteria), "</p>",
          "<br>",
          "<p>Fraction counts for ",input$jurisdiction_2," - ", paste(input$parameter_2),":", "</p>",
          "<p>", table_output, "</p>"
        )), type = "warning")
        
      list(subset_sj = NULL, exceedance_summary = NULL)
      }
      
    }), 1500)
  
  output$assessment_map_juris <- renderLeaflet({
    req(exceedance_criteria())
    exceedance_summary <-exceedance_criteria()$exceedance_summary
    req(exceedance_summary)
    avg_exceedance = exceedance_summary%>%
      mutate(exceed_perc=ifelse(is.na(exceed_perc),0,exceed_perc))%>%
      group_by(MonitoringLocationIdentifier,LatitudeMeasure,LongitudeMeasure)%>%
      summarise(avg_exceedance = mean(exceed_perc))
    
    # Create a color gradient from yellow to red
    color_function <- col_numeric(palette = c("yellow", "red"), domain = range(avg_exceedance$avg_exceedance, na.rm = TRUE))

    unique_values <- unique(avg_exceedance$avg_exceedance)
   # str(avg_exceedance)
    
    if (length(unique_values) == 1) {
      # If there is only one unique value, set a default color
      default_color <-   if (unique_values[1] == 0) "grey" else "orange"
      avg_exceedance <- avg_exceedance %>%
        mutate(color = default_color)

      
    } else {
      # Create a color gradient from yellow to red
      avg_exceedance <- avg_exceedance %>%
        mutate(color = color_function(avg_exceedance))
      
      
    }
    
    #****** Add a box area to view based off of the sites in aggregated_data
    if(length(unique(avg_exceedance$MonitoringLocationIdentifier))<2){
      # Get the coordinates of the single site
      site_coords <- avg_exceedance[1, c("LongitudeMeasure", "LatitudeMeasure")]
      avg_exceedance_sf <- st_as_sf(avg_exceedance, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)
      
      #buffer for zooming out
      buffer <- 0.05 
      
      # Create a bounding box slightly larger than the point
      view <- sf::st_bbox(c( xmin = site_coords$LongitudeMeasure - buffer, ymin = site_coords$LatitudeMeasure - buffer,
        xmax = site_coords$LongitudeMeasure + buffer, ymax = site_coords$LatitudeMeasure + buffer
      ))
    } else{
      avg_exceedance_sf <- st_as_sf(avg_exceedance, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)
      view=sf::st_bbox(avg_exceedance_sf)
    }
    
  
    
    leaflet()%>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
      addMapPane("Sites", zIndex = 415)%>%
      addMapPane("Tribes", zIndex = 410)%>%
      addCircleMarkers(data=avg_exceedance_sf,color = ~color, radius = 8,
                       layerId = ~MonitoringLocationIdentifier,fillOpacity = 0.8,label = ~MonitoringLocationIdentifier,group="Sites",options = pathOptions(pane = "Sites"))%>%
      addLegend(position = "topright",
                labels = c("Lower Avg Exceedance", "Higher Avg Exceedance","Tribal Lands"),
                colors = c("yellow","red","blue"))%>%
      addPolygons(data=sj_tribes_final,group = "Tribes",fillOpacity = 0.05,color="blue",weight = 2,options = pathOptions(pane = "Tribes"),
                  popup=paste0(
                    "Tribe Name: ", sj_tribes_final$NAME))%>%
    fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4]))

  })#End of renderLeaflet
  
  
  output$exceedance_table <- renderDataTable({
    req(exceedance_criteria()$exceedance_summary)
    t <- exceedance_criteria()$exceedance_summary%>%
      rename(Site_ID = MonitoringLocationIdentifier, Latitude=LatitudeMeasure, Longitude=LongitudeMeasure, Parameter=CharacteristicName, Use_Category=Screening.Criteria, Fraction=ResultSampleFractionText)
    datatable(t,options = list(scrollX = TRUE, pageLength = 5), selection = 'single')
  })
  
  #Plot site filters 
  observe({ 
    site_choices <- unique(exceedance_criteria()$subset_sj$MonitoringLocationIdentifier)
    updateSelectInput(session, "plot_site", choices = site_choices, selected = site_choices[1])
  })
  
  #plot param filters
  observe({ 
    req()
    param_plot_choices <- unique(exceedance_criteria()$subset_sj[exceedance_criteria()$subset_sj$MonitoringLocationIdentifier%in%input$plot_site,]$CharacteristicName)
    updateSelectInput(session, "plot_param", choices = param_plot_choices, selected = param_plot_choices[1])
  })
  
  #parameter_2 filter update
  observe({ 
    req(input$jurisdiction_2)  # Ensure jurisdiction input is available
    j_data <- combined_all_criteria3[combined_all_criteria3$Jurisdiction == input$jurisdiction_2, ]
    frac_choices <- sort(unique(j_data$ResultSampleFractionText))
    param_choices <- sort(unique(j_data$CharacteristicName))
    updateSelectInput(session, "parameter_2", choices = param_choices)
  })
  
  #Fraction_2 filter update
  observe({ 
    req(input$parameter_2)
    j_data <- combined_all_criteria3%>% filter(Jurisdiction == input$jurisdiction_2 & CharacteristicName %in% input$parameter_2)
    frac_choices <- sort(unique(j_data$ResultSampleFractionText))
    updateSelectInput(session, "sampleFraction_2", choices = frac_choices)
  }) 
  
  #Criteria_2 filter update
  observe({
    req(input$sampleFraction_2)
    j_data <- combined_all_criteria3%>% filter(Jurisdiction == input$jurisdiction_2 & CharacteristicName %in% input$parameter_2 & ResultSampleFractionText %in% input$sampleFraction_2)
    use_choices <- sort(unique(j_data$Use))
    updateSelectInput(session, "criteriaUse_2", choices = use_choices)
  }) 
  
  
  # Update plot_site input based on table selection
  observeEvent(input$exceedance_table_rows_selected, {
    selected_row <- exceedance_criteria()$exceedance_summary[input$exceedance_table_rows_selected,]
    if (nrow(selected_row) > 0) {
      updateSelectInput(session, "plot_site", selected = selected_row$MonitoringLocationIdentifier[[1]])
      updateSelectInput(session, "plot_param", selected = selected_row$CharacteristicName[1])
    }
  })
  
  #Jurisdiction Plots
  output$jurisdiction_timeSeriesPlot <- renderPlotly({
    req(exceedance_criteria()$subset_sj)
    plot_data <- exceedance_criteria()$subset_sj

    plot_data <- plot_data[(plot_data$MonitoringLocationIdentifier == input$plot_site & plot_data$CharacteristicName == input$plot_param), ]
    plot_data <- plot_data[order(plot_data$ActivityStartDate), ]

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
  
  
  # Data Download
  output$downloadData <- downloadHandler(
    filename = function() {
      parameters <- paste(input$parameter_2, collapse = "-")
      paste("Data_w_Exceedance_Summary", input$jurisdiction_2, parameters, Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      # Get the current values of the reactive data tables
      subset_sj_data <- exceedance_criteria()$subset_sj
      exceedance_summary_data <- exceedance_criteria()$exceedance_summary
      
      # Write the data to an Excel file using writexl
      write_xlsx( list("Summary Data" = subset_sj_data, "Exceedance Summary" = exceedance_summary_data),
        path = file, format_headers = FALSE, col_names = TRUE
      ) } )
}


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
# Run the app
shinyApp(ui, server)
