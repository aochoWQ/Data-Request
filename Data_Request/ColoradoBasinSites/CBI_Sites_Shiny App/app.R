library(shiny)
library(shinyBS)
library(wqTools)
library(irTools)
library(leaflet)

#This rdata has: aup,all_sites_of_interest1,nps_proj_CB
load("CBI_sites_data.Rdata")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    /* Custom CSS to style legend icons as circles */
      .legend i {
        border-radius: 70%; /* Create circle shapes */
        width: 10px; /* Specify width for circle */
        height: 10px; /* Specify height for circle */
      }
      .selectize-input {
        font-size: 12px; /* Adjust the font size as needed */
      }
    "))
  ),
  fileInput("import_sites", "Import site file", accept=".xlsx"),
  fluidRow(leaflet::leafletOutput("map"))
)

# Define server
server <- function(input, output,session) {
  
  
  output$map <- leaflet::renderLeaflet({
    
    review_map=leaflet()%>%
      addProviderTiles("Esri.WorldTopoMap", group = "World topo", options =  providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
      addMapPane("au_poly", zIndex = 415)  %>%
      addMapPane("SITES", zIndex = 419)  %>% 
      addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="3",],group="Cat 3: Insufficient data",fillOpacity = 0.3,weight=2,color="#a6a6a6", options = pathOptions(pane = "au_poly"),
                  popup=~lab
      ) %>% 
      addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="5",],group="Cat 5: Not supporting, TMDL required",fillOpacity = 0.3,weight=2,color="#e41a1c", options = pathOptions(pane = "au_poly"),
                  popup=~lab
      ) %>% 
      addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="4A",],group="Cat 4A: Approved TMDL",fillOpacity = 0.3,weight=2,color="#984ea3", options = pathOptions(pane = "au_poly"),
                  popup=~lab
      ) %>% 
      addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="2",],group="Cat 2: No evidence of impairment",fillOpacity = 0.3,weight=2,color="#255d8a", options = pathOptions(pane = "au_poly"),
                  popup=~lab
      ) %>% 
      addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="1",],group="Cat 1: Fully Supporting",fillOpacity = 0.3,weight=2,color="#118a11", options = pathOptions(pane = "au_poly"),
                  popup=~lab
      ) %>% 
      addCircles(data=all_sites_of_interest1,lat=~IR_Lat,lng=~IR_Long,group="SITES",popup=~lab,color="blue",weight = 3,radius=160,options = pathOptions(pane = "SITES"))%>%
      addCircles(data = centroids, group = "AUID",stroke=F, fill=F, label=~ASSESS_ID,
                 popup = aup$ASSESS_ID) %>%
      addCircles(data = centroids, group = "AUName",stroke=F, fill=F, label=~AU_NAME,
                 popup = aup$AU_NAME)%>%
      addCircles(data = all_sites_of_interest1,lat=~IR_Lat,lng=~IR_Long, group = "MLID",stroke=F, fill=F, label=~IR_MLID)%>%
      addCircles(lat=nps_proj_CB$IR_Lat, lng=nps_proj_CB$IR_Long, group="NPS_projects", weight=3,radius=160, color="green", opacity = 0.8,
                 popup = paste0(
                   "Project ID: ", nps_proj_CB$ProjectID,
                   "<br> Project Title: ", nps_proj_CB$ProjectTitle,
                   "<br> Amount Spent: ", nps_proj_CB$AmountSpent,
                   "<br> Date Awarded: ", nps_proj_CB$DateAwarded,
                   "<br> Date Completed: ", nps_proj_CB$DateCompleted), options = pathOptions(pane = "SITES")) %>%
      addLayersControl(position ="topleft",overlayGroups = c("NPS_projects" ,"SITES","Cat 1: Fully Supporting", "Cat 2: No evidence of impairment", "Cat 3: Insufficient data", "Cat 4A: Approved TMDL", "Cat 5: Not supporting, TMDL required"),
                       options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))%>%
      addLegend("topright", 
                colors=c("#118a11", "#255d8a", "#a6a6a6", "#984ea3", "#e41a1c"), 
                labels = c("Cat 1: Fully Supporting", "Cat 2: No evidence of impairment", "Cat 3: Insufficient data", "Cat 4A: Approved TMDL", "Cat 5: Not supporting, TMDL required"),
                title = "Assessment Category",opacity = 0.6
      )%>%
      wqTools::addMapResetButton()%>%
      leaflet.extras::addSearchFeatures(
        targetGroups = c('AUID','AUName','MLID'),
        options = leaflet.extras::searchFeaturesOptions(
          zoom=12, openPopup = FALSE, firstTipSubmit = TRUE,
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))%>%
      addLegend(position = "bottomright", # Position the new legend
                colors = c("green", "blue"),
                labels = c("NPS Project Markers", "MLID Sites"),
                title = "Marker Types",
                opacity = .5)
    
    
    
  }) #END OF renderLeaflet
  
  review_map_proxy=leafletProxy('map')
  
  
  # #Read any sites that imported 
  # observeEvent(input$import_sites, {
  #   # 1)read excel or csv
  #   # 2) Ensure they have columns MLID, Lat, Long
  #   # 3) Coordinates not null, okay if some but at least some not nullfile
  #   # 4) Convert to numeric
  #   # target_huc=clicked_points$huc12
  #   # req(target_huc)
  #   # geo_dat = geo_data[geo_data$HUC_12 ==target_huc ,]
  #   # 
  #   #Debugging
  #   print(paste("Target HUC:", target_huc))
  #   print(str(geo_dat))
  #   if (!is.null(target_huc)& nrow(geo_dat)>0) {
  #     review_map_proxy %>%
  #     clearGroup(group='highlight') %>%
  #     addPolygons(data=geo_dat, group='highlight',
  #                 options = pathOptions(pane = "highlight"),layerId=geo_dat$OBJECTID, 
  #                 fillColor='green',color="yellow", fillOpacity = 0.8, weight = 5, popup = 
  #                   paste0 ("HUC_12: ",geo_dat$HUC_12, "<br> HUC12 Name: ",geo_dat$HU_12_NAME))
  #   }
  #    })
  
}#END OF SERVER
# Run the app
shinyApp(ui, server)