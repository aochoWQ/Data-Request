library(wqTools)
library(irTools)
library(leaflet)
library(shinyBS)
library(plotly)
library(sf)
library(rgdal)
library(mapedit)
library(RColorBrewer)
library(shiny)
library(data.table)
library(dplyr)
library(DT)

#getwd()
#setwd("/Users/alanochoa/Documents/GitHub/Data_Request/TMDL_Prioritization/Post_RPS_ANALYSIS")

load("RPS_Summary_Map.Rdata") # This file was created in.. Post_RPS_Workshop_Analysis.Rmd
#Scenario_summary_BU,huc12_poly,results_all1_cleaned,au_poly_trim,All_RPS_Results,au_poly_og,tmdl_rev
ag_use = data.frame(`Beneficial Use`=c("Agriculture"),"Scenario"=c("7A"))
names(ag_use)=names(Scenario_summary_BU)
Scenario_summary_BU=rbind(Scenario_summary_BU,ag_use)

get_color <- function(min_count, max_count, value) {
  pal <- colorNumeric(palette = c("yellow", "red"), domain = c(min_count, max_count))
  pal(value)
}

#Read url arcgis- https://community.esri.com/t5/gis-life-blog/accessing-arcgis-rest-services-using-r/ba-p/898451
watershed_plan_9element_url =paste0("https://services2.arcgis.com/NnxP4LZ3zX8wWmP9/arcgis/rest/services/Watershed_Protection_Activities_WFL1/FeatureServer/0", "/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")
wp_9_elem_poly = st_read(watershed_plan_9element_url)

watershed_plan_url=paste0("https://services2.arcgis.com/NnxP4LZ3zX8wWmP9/arcgis/rest/services/Watershed_Protection_Activities_WFL1/FeatureServer/1","/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")
# Read the GeoJSON output as an sf object
watershed_plan_poly=st_read(watershed_plan_url)

tmdl_url=paste0("https://services2.arcgis.com/NnxP4LZ3zX8wWmP9/arcgis/rest/services/Watershed_Protection_Activities_WFL1/FeatureServer/2","/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")
tmdl_poly=st_read(tmdl_url)

# Assuming wp_9_elem_poly, watershed_plan_poly, and tmdl_poly are all sf objects
combined_wp_poly = bind_rows(wp_9_elem_poly, watershed_plan_poly, tmdl_poly)

combined_wp_poly$label = with(combined_wp_poly, paste(Project_Type, Project_Name, paste("<a href='", Project_Documents, "' target='_blank'>Project Document</a>",sep=""), sep="<br>"))

suppressWarnings({centroids=sf::st_centroid(au_poly_trim)})
suppressWarnings({centroidsHUC=sf::st_centroid(huc12_poly)})

# User interface
ui <-fluidPage(
  headerPanel(
    title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo_draft.png', height = 125, width = 100*2.85*1.75), target="_blank"),
    
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), 
      windowTitle="RPS Review",
      tags$style("
      .custom-notes {
          font-size: 12px; /* Adjust font size as needed */
      }
       
      .tooltip {
        max-width: 300px; /* Maximum width of tooltip */
      }
      
      .tooltip .tooltiptext {
      background-color: #555;
        color: #fff;
      }
      legend-tooltip {
        position: relative;
        display: inline-block;
        cursor: pointer;
        text-decoration: underline;
      }

      .legend-tooltip .tooltiptext {
        visibility: hidden;
        width: 200px;
        background-color: #555;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 5px 0;
        position: absolute;
        z-index: 1;
        bottom: 108%;
        left: -70px;
        margin-left: 0px;
        opacity: 0;
        transition: opacity 0.3s;
      }

      .legend-tooltip .tooltiptext::after {
        content: '';
        position: absolute;
        top: 100%; /* At the bottom of the tooltip */
        left: 90%;
        margin-left: -5px;
        border-width: 5px;
        border-style: solid;
        border-color: #555 transparent transparent transparent;
      }

      .legend-tooltip:hover .tooltiptext {
        visibility: visible;
        opacity: 1;
      }
      
      info legend .leaflet-control .legend g > text {
      fill: white !important;  /* Set font color of legend digit values to white */
      }"
                              )
  )),
  h3("Watershed Planning and Prioritization Maps"),
  br(),
  h4("Notes"),
  p(class="custom-notes","Use the magnifying glass on the left hand side to search for either an ASSESS_ID or a HUC_12 id on the map."),
  p(class="custom-notes","Clicking on an Assessment Unit (AU) on the map will update the tables below with detailed information."),
  #Map
  column(12, shinyjqui::jqui_resizable(bsCollapse(multiple=T,open=1,
       bsCollapsePanel(list(icon('plus-circle'), icon('map-marked-alt'),"Review map"), value=2,
                       # Map
                       uiOutput('map_rev_filter'),
                       verbatimTextOutput("clicked_polygon_details"),
                       shinycssloaders::withSpinner(leaflet::leafletOutput("assessment_map", height="600px", width="100%"),size=2, color="#0080b7")
   
       ),

       # #Table showing 
       bsCollapsePanel(list(icon('plus-circle'), icon('table'), p("Prioritization Results Table", em(" - Prioritization responses for the selected AU. Includes all rows for all Actions."))), value=3,
                       fluidRow(DTOutput("rps_results"))
                       ),
       bsCollapsePanel(list(icon('plus-circle'), icon('table'), p("RPS Index Results Table", em(" - Displays the raw RPS index results for the selected AU."))), value=2,
                       fluidRow(DTOutput("rps_index"))
       )


  ) ) ) )


server <- function(input, output, session){
  
  # Empty reactive objects
  reactive_objects=reactiveValues()
  updateMap= function() {
    req(input$action_filter, input$basin_filter, input$bu_filter, input$map_type)
    
    scen_target=Scenario_summary_BU[Scenario_summary_BU$`Beneficial Use`%in%input$bu_filter,]
    target_results <- results_all1_cleaned %>%
      filter(Action_plan_bin%in%input$action_filter & Basin%in%input$basin_filter & Scen%in%scen_target$Scenario)%>%
      select(ASSESS_ID, Basin, Scen,Action_plan_bin) %>%
      group_by(ASSESS_ID, Basin) %>%
      summarise(PriorityCount = n(),
                All_Scen = paste(unique(Scen), collapse = ", "),
                All_actions=paste(unique(Action_plan_bin),collapse = ", "),
                .groups = 'drop',)
    
    basin_summary <- target_results %>%
      group_by(Basin) %>%
      summarise(
        MinCount = min(PriorityCount, na.rm = TRUE),
        MaxCount = max(PriorityCount, na.rm = TRUE),
        .groups = 'drop'
      )
    target_results=merge(target_results,basin_summary,all.x=T)
    
    target_results <- target_results %>%
      mutate(color = mapply(get_color, MinCount, MaxCount, PriorityCount))
    
    
    au_huc=results_all1_cleaned[results_all1_cleaned$ASSESS_ID%in%target_results$ASSESS_ID,c("ASSESS_ID","HUC_12")]
    
    target_hucs=huc12_poly[huc12_poly$HUC_12%in%au_huc$HUC_12,]
    target_hucs= within(target_hucs,{
      lab=paste0("<strong>HUC12 Name: </strong>",HU_12_NAME,
                 '<br />', "<strong>HUC12 ID:</strong> ",HUC_12)
    })
    
    au_poly_trim1=merge(au_poly_trim,target_results)
    au_poly_trim1=within(au_poly_trim1, {
      lab=paste0(
        '<div style="max-width: 550px; overflow-wrap: break-word; background: white; padding: 2px; border: 1px grey;">',
        "<strong>AU name: </strong>", AU_NAME,
        '<br />', "<strong>AU ID:</strong> ", ASSESS_ID,
        '<br />',"<strong>Approved TMDL: </strong>",Cat_4A,
        '<br />',"<strong>Impairments: </strong>",Cat_5,
        '<br><strong> PriorityCount: </strong>', PriorityCount,
        '<br> <strong>Scenarios: </strong>',All_Scen,
        '<br> <strong>Actions: </strong>',All_actions,
        '</div>')
    })
    
    if(input$map_type=="Action Category"){
      au_poly_trim1=au_poly_trim1%>%
        mutate(color=case_when(
          All_actions == "Protection" ~ "#6A36A0",
          grepl("TMDL", All_actions, ignore.case = TRUE)  ~ "#EF9E29",
          All_actions == "Watershed Plan/NPS Priority Area" ~ "#0047AB",
          TRUE ~ "#9B5114" # Optional: default case if none of the above conditions are met
        ))}
    
    # TMDL REVISION ADD
    
    if(input$map_type=="Potential TMDL Revisions"){
      au_poly_trim1=au_poly_trim1%>%
        filter()%>%
        mutate(color=case_when(
          All_actions == "Protection" ~ "#6A36A0",
          grepl("TMDL", All_actions, ignore.case = TRUE)  ~ "#EF9E29",
          All_actions == "Watershed Plan/NPS Priority Area" ~ "#0047AB",
          TRUE ~ "#9B5114" # Optional: default case if none of the above conditions are met
        ))}
    
    #Legend dependent on input$map_type
    legend_info <- 
      if (input$map_type == "Action Category") {
        list(
          pal = colorFactor(
            palette = c( "#9B5114","#6A36A0", "#EF9E29", "#0047AB"),
            domain = c( "Protection", "TMDL", 
                        "Watershed Plan/NPS Priority Area", "2 Or more Action Categories")
          ),
          labels = c("Protection", "TMDL", 
                     "Watershed Plan/NPS Priority Area", "2 Or more Action Categories"),
          title = "Action Categories"
        )
      } else {  # Default to Prioritization Counts
        list(
          pal = colorNumeric(
            palette = c("yellow", "red"), 
            domain = c(1, 5)
          ),
          labels = c("Low","High"),
          title = HTML("<div class='legend-tooltip'>AU Priority Count<span class='tooltiptext'>Ignore numeric legend values; focus on the color gradient, where red is higher priority.</span></div>")
        )
      }
    
    # Observe changes in input$map_type and update the map legend accordingly
    if(dim(au_poly_trim1)[1]>0){
      view=sf::st_bbox(au_poly_trim1)
      asmnt_map_proxy %>%
        clearGroup(group='Assessment units')%>%
        clearGroup(group='HUC_12')%>%
        addPolygons(data=au_poly_trim1,group="Assessment units",smoothFactor=4,fillOpacity = 0.8, layerId=~polyID, weight=3,fillColor=~color,color="white", options = pathOptions(pane = "au_poly"),
                    label=lapply(au_poly_trim1$lab, HTML)) %>%
        addPolygons(data=target_hucs,group="HUC_12",smoothFactor=4,fillOpacity = 0.5, layerId=target_hucs$OBJECTID, weight=3,fillColor="#2A63A0",color="white", options = pathOptions(pane = "HUC_12"),
                    label=lapply(target_hucs$lab,HTML)) %>%
        fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4]))%>%
        hideGroup("HUC_12")%>%
        hideGroup("Existing Watershed Protection Activities")%>%
      clearControls() %>%
        addLegend(
          position = "topright",
          pal = legend_info$pal,
          values = if (input$map_type == "Action Category") legend_info$labels
          else 1:5,
          labels = legend_info$labels,
          title = legend_info$title,
          opacity = 1
        )
    }else{
      asmnt_map_proxy %>%
        clearGroup(group='Assessment units')%>%
        clearGroup(group='HUC_12')%>%
        hideGroup("HUC_12")%>%
        hideGroup("Existing Watershed Protection Activities")
    }
  }##END OF updateMap()
  
  # Map filter UI
  output$map_rev_filter=renderUI({
    action_choices=unique(results_all1_cleaned$Action_plan_bin)
    basin_choices=sort(unique(results_all1_cleaned$Basin))
    bu_choices = sort(unique(Scenario_summary_BU$`Beneficial Use`))
    fluidRow(
      column(3,bsTooltip(
        id = "map_type",
        title = paste(
          "Prioritization Counts map is based on the number of entries for selected actions (TMDL, Protection, etc.), red being highest priority.",
          "Color intensity is basin-specific and updates with selected Actions, Basins & Beneficial Use.",
          sep = " " ), placement = "right", trigger = "hover",options = list( html = TRUE,container = "body")),
        radioButtons("map_type","Select Map Type",choices=c("Action Category", "Prioritization Counts","Potential TMDL Revisions"),selected="Action Category",width="250px")),#map_rev_filter
      column(4,shinyWidgets::pickerInput('action_filter', 'Action types', action_choices, selected=action_choices, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
      column(2,shinyWidgets::pickerInput('basin_filter', 'Basin types', basin_choices, selected=basin_choices, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
      column(3,shinyWidgets::pickerInput('bu_filter', 'Beneficial Use types', bu_choices, selected=bu_choices, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")))
    )
  })
  
  # Filter map by Action type
  #Filter results by both filters then add
  observe({
    input$action_filter
    input$basin_filter
    input$bu_filter
    input$map_type
    updateMap()
  })
  
  # Map output
  output$assessment_map=leaflet::renderLeaflet({
    view=sf::st_bbox(au_poly_trim)
    review_map = leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE, dragging=TRUE))
    review_map=leaflet::addProviderTiles(review_map, "Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
      addMapPane("au_poly", zIndex = 415) %>%
      addMapPane("HUC_12", zIndex = 414) %>%
      addPolygons(data=combined_wp_poly,group="Existing Watershed Protection Activities",popup = ~label,fillColor ="pink",fillOpacity = .5)%>%
      addCircles(data = centroids, group = "AUID",stroke=F, fill=F, label=~ASSESS_ID,
                 popup = au_poly_trim$ASSESS_ID) %>%
      addCircles(data = centroidsHUC, group = "HUCID",stroke=F, fill=F, label=~HUC_12,
                 popup = huc12_poly$HUC_12)%>%
      fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) %>% # 
      leaflet::addLayersControl(position ="topleft",
                                baseGroups = c("Topo"),overlayGroups = c("Assessment units","HUC_12","Existing Watershed Protection Activities"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex=FALSE))%>%
      addLegend(position = "topright",
               labels = c("HUC 12 Boundaries"),
               colors = c("#6A9DD3"),
               opacity = 1
                 )%>%
      leaflet.extras::addSearchFeatures(
        targetGroups = c('AUID','HUCID'),
        options = leaflet.extras::searchFeaturesOptions(
          zoom=12, openPopup = FALSE, firstTipSubmit = TRUE,
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))%>%
      hideGroup("HUC_12")%>%
      hideGroup("Existing Watershed Protection Activities")
  }) #END OF renderLeaflet
  
  asmnt_map_proxy=leafletProxy('assessment_map')
  # Reactive value to store clicked polygon details
  clicked_polygon <- reactiveValues(details = NULL)
  
  # Observer to capture click events on the map
  observeEvent(input$assessment_map_click, {
    click_data = input$assessment_map_shape_click
    print(click_data)
    if (click_data$group=="Assessment units") {
      data_selected <-au_poly_trim[au_poly_trim$polyID == click_data$id, ]
      clicked_polygon$details <- paste0("AU Name: ", data_selected$AU_NAME, 
                                        ", ASSESS_ID: ", data_selected$ASSESS_ID)
      #When a site is clicked render that data in the table below map
      reactive_objects$selected_data=results_all1_cleaned[results_all1_cleaned$ASSESS_ID==data_selected$ASSESS_ID,]
      selected_hucs=unique(results_all1_cleaned[results_all1_cleaned$ASSESS_ID==data_selected$ASSESS_ID,c("HUC_12","Scen")])
      selected_hucs$uid=paste0(selected_hucs$HUC_12,selected_hucs$Scen)
      reactive_objects$selected_hucs=unique(selected_hucs$uid)
        }
  }) #END OF assessment_map_click
  
  # Render clicked polygon details
  output$clicked_polygon_details <- renderPrint({
    req(clicked_polygon$details)
    clicked_polygon$details
  })
  
  #Render the first table with RPS Selections
  output$rps_results <- renderDT({
    req(reactive_objects$selected_data)
    datatable(reactive_objects$selected_data)
  })
  
  #Render rps
  output$rps_index <- renderDT({
    req(reactive_objects$selected_hucs)
    index_table=All_RPS_Results[All_RPS_Results$uid%in%reactive_objects$selected_hucs,]
    index_table=index_table%>%select(-uid)
    datatable(index_table)
  })
  
}##END OF SERVER

## run app
shinyApp(ui = ui, server = server)
