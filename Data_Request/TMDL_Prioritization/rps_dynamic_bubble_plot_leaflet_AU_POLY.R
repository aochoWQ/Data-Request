if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readxl")) install.packages("readxl")
if (!require("leaflet")) install.packages("leaflet")
if (!require("sf")) install.packages("sf")
if (!require("rmapshaper")) install.packages("rmapshaper")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("rsconnect")) install.packages("rsconnect")
if (!require("plotly")) install.packages("plotly")
library(RColorBrewer)


library(plotly)
# set working directory (replace with the actual path)
setwd("/Users/alanochoa/Documents/GitHub/Data_Request/TMDL_Prioritization/")
# load data
data_long <- read_excel("HUC12_summary.xlsx")

au_poly = wqTools::au_poly
#st_write(au_poly,"RPS Shiny App/au_poly.shp")
# Load your shapefile 
#Reading this trimmed file instead of Watersheds_Area
shapefile_path <- 'RPS Shiny App/HUC12_Shapefile/Watersheds_Area_Trimmed.shp'
geo_data <- st_read(shapefile_path, stringsAsFactors = FALSE)

#It takes a bit to start up I wonder if the large number of polygons could be why??
#Lets trim to just the impaired huc12s
# unique_huc12=unique(data_long$HUC_12)
# geo_data_trimmed=geo_data[geo_data$HUC_12%in%unique_huc12,]
# st_write(geo_data_trimmed, 'RPS Shiny App/HUC12_Shapefile/Watersheds_Area_Trimmed.shp')

# Simplify the geometry for better performance
#geo_data <- ms_simplify(geo_data, keep = 0.01)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .selectize-input {
        font-size: 12px; /* Adjust the font size as needed */
      }
    "))
  ),
  selectInput("basin", "Select Basin", choices = unique(data_long$Basin)),
  selectInput("scenario", "Select Scenario", choices = unique(data_long$Scenario)),
  fluidRow(plotlyOutput("bubble_plot")),
  fluidRow(verbatimTextOutput("clicked_point_details")),
  fluidRow(leaflet::leafletOutput("map"))
)

# Define server
server <- function(input, output) {
  
 
  output$bubble_plot <- renderPlotly({
    
    selected_scenario=input$scenario
    selected_basin=input$basin
    filtered_data=subset(data_long, Scenario == selected_scenario & Basin == selected_basin)
    # make color list
    #most brewer color sets only have max 11 or 12 colors in the set..
    Paired_colors <- brewer.pal(12, "Paired")
    Set3_colors <- brewer.pal(12, "Set3")
    color_palette <- c(Paired_colors, Set3_colors)
    
    
    watershed_indices = match(filtered_data$Watershed, unique(filtered_data$Watershed))
    color_indices = (watershed_indices - 1) %% length(color_palette) + 1
    
    filtered_data = filtered_data %>%
      mutate(Color_ = color_palette[color_indices],
              label = paste0("HUC_12: ", HUC_12,
                  "<br>Watershed Name : ", Watershed,
                  "<br>Ecological Index : ", Ecological_Index,
                  "<br>Stressor Index: ", Stressor_Index))%>%
      arrange(desc(Social_Index))
    
    # Calculate median values for x and y
    median_x <- median(filtered_data$Stressor_Index)
    median_y <- median(filtered_data$Ecological_Index)
    
    #Why could the range of Social_Index be causing weird issues with size?? smaller range 
    scaled_social_index = scales::rescale(filtered_data$Social_Index, to = c(1, 10))
    # Using plot_ly to be able to create an interactive plotly_click object so the selected 
    # HUC12 could be static and the team could copy and paste if needed.
   p=plot_ly(filtered_data, customdata = ~HUC_12, x = ~Stressor_Index, y = ~Ecological_Index,text = ~Watershed ,textposition="top left", 
            textfont = list(size = 8), 
            hovertemplate = ~paste(
              "Watershed: ", Watershed,
              "<br>Stressor Index: ", Stressor_Index,
              "<br>Ecological Index: ", Ecological_Index,
              "<br>Social Index: ", Social_Index,
              "<extra></extra>"  # This removes the default hover text (trace name)
            ),size = scaled_social_index 
            ,type="scatter",mode="markers+text",color = ~Color_, marker = list(opacity = 0.9, 
                                                          sizemode = 'diameter')) %>%
      layout(
        xaxis = list(zeroline = FALSE),  # Removes the x-axis line at zero
        yaxis = list(zeroline = FALSE), 
        title = list(text = input$scenario,font = list(size = 12)),
        xaxis = list(title = "Stressor Index"),
        yaxis = list(title = "Ecological Index"),
        hovermode = 'closest',
        showlegend=FALSE,
      shapes = list(
      list(type = 'line', x0 = median_x, x1 = median_x, y0 = min(filtered_data$Ecological_Index)-10,
           y1 = max(filtered_data$Ecological_Index)+10,line = list(color = "gray", width = 2, opacity = 0.4)),
      list(type = 'line', x0 = min(filtered_data$Stressor_Index)-10, x1 = max(filtered_data$Stressor_Index)+10, 
           y0 = median_y, y1 = median_y,line = list(color = "gray", width = 2, opacity = 0.5)))
    ,
    annotations = list(
      list(x =min(filtered_data$Stressor_Index), y = max(filtered_data$Ecological_Index)+15, text = "1. Protection & Restoration",xanchor = 'right',showarrow=FALSE),
      list(x = max(filtered_data$Stressor_Index), y = max(filtered_data$Ecological_Index)+15, text = "2. Potential priority",xanchor = 'right',showarrow=FALSE),
      list(x = min(filtered_data$Stressor_Index), y = min(filtered_data$Ecological_Index)-15, text = "3. Less critical More data needed",xanchor = 'right',showarrow=FALSE),
      list(x = max(filtered_data$Stressor_Index), y = min(filtered_data$Ecological_Index)-15, text = "4. Areas under significant stress",xanchor = 'right',showarrow=FALSE)
    )
      )

   })# end of renderPlotly
  # Reactive value to store clicked points
  clicked_points <- reactiveValues(details = NULL)
  
  # Capture click events
  observeEvent(event_data("plotly_click"), {
    click_data = event_data("plotly_click")
    if (!is.null(click_data)) {
      data=unique(data_long[data_long$HUC_12 == click_data$customdata,c("HUC_12","Watershed")])
      clicked_points$details = paste0("Watershed: ", data$Watershed," HUC12: ",data$HUC_12 )
      clicked_points$huc12 = click_data$customdata
    }
  })
  
  # Render clicked point details
  output$clicked_point_details <- renderPrint({
    req(clicked_points$details)
    clicked_points$details 
  })
  
   output$map <- leaflet::renderLeaflet({
    # Use input$scenario to filter and update the map
    selected_basin <- input$basin
    selected_scenario <- input$scenario
    
    # Merge with RPI data
    filt_data = unique(data_long[data_long$Scenario == selected_scenario & 
                                   data_long$Basin == selected_basin, c("HUC_12", "Basin","RPI_Score")])
    filtered_geo_data <- merge(geo_data,filt_data)
    #Basin not in geo_data merge list of HUC_12 basin dataframe first
    filtered_geo_data <- subset(filtered_geo_data, Basin == selected_basin)
    
    
    view=sf::st_bbox(filtered_geo_data)
    
    #Looks like colorRamp works best if values are between 0-1, RPI needs to be normalized. Still not sure if 
    #this is the proper way to use colorRamp
    colorFunction = colorRamp(c("white", "#00008B"))
    normalizedScores = filtered_geo_data$RPI_Score / 100
    colors_list = sapply(normalizedScores, function(score) {
    rgb(colorFunction(score), maxColorValue = 255)
      })

    review_map = leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE, dragging=TRUE))
    review_map=leaflet::addProviderTiles(review_map, "Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
      addMapPane("au_poly", zIndex = 413) %>%
      addMapPane("huc_12s", zIndex = 415) %>%
      addMapPane("highlight", zIndex = 420) %>%
     addPolygons(data= filtered_geo_data, group="huc_12s",fillOpacity=0.9,layerId=filtered_geo_data$OBJECTID,
                 weight=3,fillColor =~colors_list,options= pathOptions(pane = "huc_12s"),
                 popup = paste0 ("HUC_12: ",filtered_geo_data$HUC_12, "<br> HUC12 Name: ",filtered_geo_data$HU_12_NAME)
                 )%>%
      addPolygons( data=au_poly,group="Assessment units",fillOpacity = 0.1, layerId=au_poly$polyID,
                   weight=3,color="orange", options = pathOptions(pane = "au_poly"),
                   popup=paste0(
                     "AU name: ", au_poly$AU_NAME,
                     "<br> AU ID: ", au_poly$ASSESS_ID,
                     "<br> AU type: ", au_poly$AU_Type)
      )%>%
      fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) %>% # Saw this in another app Elise built to zoom in on Basin of interest.
      leaflet::addLayersControl(position ="topleft",
                                baseGroups = c("Topo"),overlayGroups = c("Assessment units", "huc_12s","highlight"),
                                options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex=FALSE)) %>%
      leaflet::addLegend(position = 'topright',
                         colors = c('blue','orange',"green"),
                         labels = c('HUC_12s', 'Assessment Units','Selected HUC12'))
   
   }) #END OF renderLeaflet

   review_map_proxy=leafletProxy('map')
   
   observeEvent(clicked_points$huc12, {
     target_huc=clicked_points$huc12
     req(target_huc)
     geo_dat = geo_data[geo_data$HUC_12 ==target_huc ,]
     
     #Debugging
     print(paste("Target HUC:", target_huc))
     print(str(geo_dat))
     if (!is.null(target_huc)& nrow(geo_dat)>0) {
       review_map_proxy %>%
       clearGroup(group='highlight') %>%
       addPolygons(data=geo_dat, group='highlight',
                   options = pathOptions(pane = "highlight"),layerId=geo_dat$OBJECTID, 
                   fillColor='green',color="yellow", fillOpacity = 0.8, weight = 5, popup = 
                     paste0 ("HUC_12: ",geo_dat$HUC_12, "<br> HUC12 Name: ",geo_dat$HU_12_NAME))
     }
      })
   
}#END OF SERVER
# Run the app
shinyApp(ui, server)

