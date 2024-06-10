library(shiny);     library(plyr);           library(leaflet);   library(reshape2)
library(tidyr);     library(rpivotTable);    library(dplyr);     library(jsonlite)
library(rgdal);     library(RJSONIO);        library(tibble);    library(stringr)
library(sp);        library(maps);           library(maptools);  library(geojsonio)
library(ggplot2);   library(shinydashboard); library(rjson);     library(DT)
library(xlsx);      library(readxl)

###
# install.packages("maptools")
# Warning in install.packages :
# Package ‘maptools’ is not available for this version of R

setwd("/Users/alanochoa/Documents/GitHub/Data_Request/San Juan")

######
# Create radio button option to see selected criteria or sample values. 
# Add code from 'leaflet_mapping issue.R' to display criteria on chloropleth map
# Figure out how to display sampled values as a function of time
# View sample values for all Authorities option to get the best plume over time visual
# maybe add stream layers and GKM location marker
# finally create Markdown tutorial/guide for shiny app
######inst

## created by Brian Avant
## The purpose of this tool is to screen GKM related datasets containing metals concentrations in the water column against water quality standards for specific areas.

## Read in screening critiera and sample data
#setwd("L:/PRIV/AnimasRiver/ARP_ANALYTICS/Hazard Quotient Processing_Screening Tool/Water Quality Screening Tool_copy 062019/wq_screen-master") #L Drive

########

## 09/2021: Updated criteria comparison/screening calculators in summer/fall 2020 - Nina Morales (see NM comments for updates/changes to original code)
# Primary error source: there was a mismatch between the names of the input datasetcolumns and the column names referred to in the code. 
# Reference original code version for future updates (accessible through Github - see previous README.md file). 

## 09/2021: Current functionality
#Criteria comparison and screening calculators are functional. 
#Did not fix map criteria return function – maps load, but no criteria is returned when selected
#Did not update the ScreenWQX tab
#New output file names (SampleComparisonToCriteria.csv = samplemarkers_screen, SampleExceedCount.csv = output_screen) disable the demo tab that relies on Reload1_file.csv and samplelatlondifferences.csv
  #This Demo tab works in the original app code, for future reference 
  #This can be updated in the future by changing all functions that rely on Reload1_file.csv and samplelatlondifferences.csv, useful to reference original app code to restore this 
#New output file names (SampleComparisonToCriteria.csv = samplemarkers_screen, SampleExceedCount.csv = output_screen) disable the reload prior screen tab that relies on Reload1_file.csv and samplelatlondifferences.csv
  #This can be updated in the future by changing all functions that rely on Reload1_file.csv and samplelatlondifferences.csv, useful to reference original app code to restore this

## NM - References: I have tried to cite links that assisted in debugging the program in this file and the draft/scratch work files. 


#NM 8/27, update this to new criteria file 
WQCritSS <- read_excel("WQ Criteria and Sample Templates_basinugl.xlsx", sheet = "WQCriteriaTot")
WQCritHardness <- read_excel("WQ Criteria and Sample Templates_basinugl.xlsx", sheet = "WQCriteriawHardness")

## Reformat WQ Screening Criteria
#gather() is basically the same as pivot_longer() 
#pivot_longer(cols = -c(Designated_Use, ScreenType, Name, Spatial_Type, Sample_Type),
#names_to = "variable",values_to = "value")
WQCritSS_clean <- WQCritSS %>%
    gather(variable, value, -c(Designated_Use,ScreenType,Name,Spatial_Type,Sample_Type))%>%#
    filter(complete.cases(.))

namevector <- c("maSlope","mbIntercept", "alphaBeta", "conversionFactor", "alpha", "beta")
WQCritSS_clean[,namevector] <- NA
WQCritAll <- bind_rows(WQCritSS_clean,WQCritHardness)

## Create Output data.frames
rows <- nrow(WQCritSS_clean)

#################### Load GEOJSONs and Merge Criteria Data #####################
statesJSON <- readOGR(dsn="./Spatial Layers/states.geojson", layer = "states", verbose = FALSE) #selected_states
# statesJSON <- readOGR("./Spatial Layers/states.geojson", "OGRGeoJSON", verbose = FALSE) #selected_
states <- map(statesJSON, fill=TRUE, col="transparent", plot=FALSE)
StateIDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
states_sp <- map2SpatialPolygons(states, IDs=StateIDs,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))
tribesJSON <- readOGR(dsn="./Spatial Layers/tribes.geojson", layer = "tribes", verbose = FALSE)
# tribesJSON <- readOGR("./Spatial Layers/tribes.geojson", "OGRGeoJSON", verbose = FALSE)
tribesmap <- map(tribesJSON, fill=TRUE, col="transparent", plot=FALSE)
TribesIDs <- sapply(strsplit(tribesmap$names, ":"), function(x) x[1])
tribes_sp <- map2SpatialPolygons(tribesmap, IDs=TribesIDs,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))
regionsJSON <- readOGR(dsn="./Spatial Layers/EPA_regions.geojson", layer = "EPA_regions", verbose = FALSE)
# regionsJSON <- readOGR("./Spatial Layers/EPA_regions.geojson", "OGRGeoJSON", verbose = FALSE)
regions <- map(regionsJSON, fill=TRUE, col="transparent", plot=FALSE)
RegionsIDs <- sapply(strsplit(regions$names, ":"), function(x) x[1])
regions_sp <- map2SpatialPolygons(regions, IDs=RegionsIDs,
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))
### latlong Conversion Function #######################################################
latlong2state <- function(pointsDF) {
    ## Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    ## Use 'over' to get _indices_ of the Polygons object containing each point 
    states_indices <- over(pointsSP, states_sp)
    ## Return the state names of the Polygons object containing each point
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    stateNames[states_indices]
}
latlong2tribe <- function(pointsDF) {
    ## Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    ## Use 'over' to get _indices_ of the Polygons object containing each point 
    tribes_indices <- over(pointsSP, tribes_sp)
    ## Return the state names of the Polygons object containing each point
    tribeNames <- sapply(tribes_sp@polygons, function(x) x@ID)
    tribeNames[tribes_indices]
}
latlong2region <- function(pointsDF) {
    ## Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    ## Use 'over' to get _indices_ of the Polygons object containing each point 
    regions_indices <- over(pointsSP, regions_sp)
    ## Return the state names of the Polygons object containing each point
    regionNames <- sapply(regions_sp@polygons, function(x) x@ID)
    regionNames[regions_indices]
}
####################################################################################
m=0
n=0
g=0

ui <- fluidPage(
  navbarPage("WQ Screen",id="nav",
             tabPanel("Input Samples",
                      titlePanel("Water Quality Screening Tool"),
                      navlistPanel("Input Options",
                                   tabPanel("Import Samples",
                                            fluidRow(column(8,h3("Import samples from text file and screen using appropriate criteria"),
                                                            wellPanel(fileInput(inputId = "Samples", label = h3("Import Sample File")),
                                                                      radioButtons(inputId = "Spatialdist", label =h4("Location Input Type"),
                                                                                   c("By Name"="Name","By Lat Lon"="LatLon")),
                                                                      checkboxInput(inputId = "Categories", 
                                                                                    label = "Group by Spatial or Temporal Categories",
                                                                                    value = FALSE),
                                                                      #NM 9/17 - removing this option to return all screened data for comparison. 
                                                                      # checkboxInput(inputId = "checked", 
                                                                      #               label = "Include contaminants that were screened but did not exceed criteria",
                                                                      #               value = FALSE),
                                                                      actionButton(inputId = "Click", label = "Screen Samples"))))),
                                   tabPanel("GKM Demo",
                                            fluidRow(column(8,h3("View screened samples from the Gold King Mine release"),
                                                            actionButton(inputId = "Demo", label = "Run Demo")))),
                                   tabPanel("Screen WQX Data",
                                            fluidRow(column(8,h3("Feature Coming Soon! Download WQX water quality historical data and screen it based on apppropriate criteria"),
                                                            wellPanel(dateRangeInput(inputId = "WQXDates", 
                                                                                     label = h4("Date Range for WQX Data")), 
                                                                      selectInput(inputId = "selectstate", label = h4("Select State"),
                                                                                  choices = statesJSON$Name, 
                                                                                  selected = statesJSON$Name[1]),
                                                                      selectInput(inputId = "selectcontaminant", label = h4("Select Contaminant"),
                                                                                  choices = list("Aluminum" = 1, "Arsenic" = 2, "Lead" = 3), 
                                                                                  selected = 1),
                                                                      selectInput(inputId = "selecttype", label = h4("Select Type"),
                                                                                  choices = list("Total" = 1, "Dissolved" = 2), 
                                                                                  selected = 1),
                                                                      selectInput(inputId = "selectunits", label = h4("Select Units"),
                                                                                  choices = list("mg/l", "ug/l")),
                                                                      actionButton(inputId = "ClickWQX", label = "Get Data"))))),
                                   tabPanel("Reload Prior Screen",
                                            fluidRow(column(8,h3("Reload input files from previous WQ Screen run"),fileInput(inputId = "Reload1", label = "Load Reload1_file.csv"),
                                                            fileInput(inputId = "Reload2", label = "Load Samplelatlondiferences.csv"),
                                                            actionButton(inputId = "reClick", label = "Load Screened Samples")))),
                                   fluidRow(column(9, textOutput(outputId="screenprogress"),
                                                   textOutput(outputId="metal")
                                   )))),
             tabPanel("Tables",
                      h2('Water Quality Screen Results'),
                      downloadButton('screendownload', 'Download Screen Results (.csv)'),
                      br(),
                      dataTableOutput("Results"),
                      br(),
                      h2('Individual Criteria Comparison'),
                      downloadButton('criteriadownload', 'Download Criteria Comparison Results (.csv)'),
                      br(),
                      dataTableOutput("Screen")),
             
              tabPanel("Interactive Map", id="Map", 
                      div(class="outer",
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          leafletOutput("map",width="100%",height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        h2("Screening Metrics"),
                                        htmlOutput("Bound_selector"),
                                        wellPanel(htmlOutput("Metal_selector"),
                                                  htmlOutput("Type_selector"),
                                                  htmlOutput("Criteria_selector"),
                                                  actionButton(inputId = "Clickmap", label = "Get Criteria")),
                                        conditionalPanel( condition = "output.Samplenrows",
                                                          checkboxInput("ImportedSamples", "Imported Samples")),
                                        conditionalPanel( condition = "output.WQXnrows",
                                                          checkboxInput("WQXSamples", "WQX/STORET Samples")),
                                        
                                        uiOutput("animationSlider")#,
                                        
                          ))
             ),   

             tabPanel("Figures",
                      h2('Pivot Tables'),
                      rpivotTableOutput("Pivot1")
             )
  )
)

server <- function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)
    ############################## set up for interactive map ############################
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -98.35, lat = 39.5,  zoom = 4)
    })
    output$Bound_selector <- renderUI({
        selectInput(
            inputId = "Authority", 
            label = "Water Quality Criteria Authority",
            choices = unique(as.character(WQCritAll$Spatial_Type)),
            selected = "States")
    })
    output$Metal_selector <- renderUI({
        available1 <- WQCritAll[WQCritAll$Spatial_Type == input$Authority,"variable"]
        selectInput(
            inputId = "Contaminant", 
            label = "Contaminant",
            choices = unique(available1),
            selected = unique(available1)[1])
    })
    output$Type_selector <- renderUI({
        available2 <- WQCritAll[WQCritAll$Spatial_Type == input$Authority & WQCritAll$variable == input$Contaminant,"Sample_Type"]
        selectInput(
            inputId = "Sampletype", 
            label = "Sample Type",
            choices = unique(available2),
            selected = unique(available2)[1])
    })
    output$Criteria_selector <- renderUI({
        available3 <- WQCritAll[WQCritAll$Spatial_Type == input$Authority & WQCritAll$variable == input$Contaminant & WQCritAll$Sample_Type == input$Sampletype,"ScreenType"]
        selectInput(
            inputId = "Criteria", 
            label = "Criteria",
            choices = unique(available3),
            selected = unique(available3)[1])
    })
    Authority_Layer <- reactive({
        req(input$Authority)
        if (input$Authority == "Tribes"){
            tribesJSON
        } else if (input$Authority == "States"){
            statesJSON
        } else if (input$Authority == "Regions"){
            regionsJSON
        } else {}
    })
    observe({
        selected_layer <- Authority_Layer()
        
        if(is.null(selected_layer)) {
            print("Nothing selected")
            leafletProxy("map") #%>% clearMarkers()
        }
        else{
            leafletProxy("map",data = selected_layer) %>%
                clearShapes() %>%
                addPolygons(color = "#A9A9A9",
                            fillColor = "#d3d3d3", 
                            stroke = TRUE, 
                            smoothFactor = 0.5,
                            opacity = 1.0, fillOpacity = 0.5, weight = 1) #%>%
            #setView(lng = -98.35, lat = 39.5,  zoom = 4) 
        }
    })
    ################################################################################# 
    filedata <- reactive({
      req(input$Samples)
      infile <- input$Samples
      #read.table(infile$datapath,sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
      read.csv(infile$datapath, header = TRUE, sep = ",",stringsAsFactors=FALSE, encoding = "UTF-8") #https://stackoverflow.com/questions/4806823/how-to-detect-the-right-encoding-for-read-csv and https://stackoverflow.com/questions/16681812/how-to-read-csv-data-with-unknown-encoding-in-r
      
    })
    Reload1data <- reactive({
      req(input$Reload1)
      infile <- input$Reload1
      read.csv(infile$datapath,sep=",",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
    })
    Reload2data <- reactive({
      req(input$Reload2)
      infile <- input$Reload2
      read.csv(infile$datapath,sep=",",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
    })
    
    observeEvent(input$Click, {
      df <- filedata()
      colnames(df)[1]  <- "Date_Time" # NM 8/30 renamed because encoding wasn't working
      if (input$Spatialdist == "LatLon") { #lat lon version
        ## Sample Sites
        samplemarkers <- select(df, c(Lon,Lat,Site_ID))
        # write.csv(df,"samplemarkers.csv", row.names=FALSE)
        ## Collect relevant spatial boundaries from sample lat lon
        samplecoords <- select(df, c(Lon,Lat))
        Spatial_Boundstate <- str_to_title(latlong2state(samplecoords)) 
        Spatial_Boundregion <- str_to_title(latlong2region(samplecoords))
        Spatial_Boundtribe <- str_to_title(latlong2tribe(samplecoords))
        
        ## add States column to sample data and remove NAs
        ObsSpatial_BoundsStatena <- add_column(df, Spatial_Boundstate, .after = 1) 
        ObsSpatial_BoundsState <- complete.cases(ObsSpatial_BoundsStatena[,2])
        ObsAllSpatial_BoundsState <- ObsSpatial_BoundsStatena[ObsSpatial_BoundsState, ]
        States_Layer <- add_column(ObsAllSpatial_BoundsState, Sp_Layer = "States", .after = 2) 
        colnames(States_Layer)[2] <- "Name"
        
        ## add EPA Region column to sample data and remove NAs
        ObsSpatial_BoundsRegionna <- add_column(df, Spatial_Boundregion, .after = 1) 
        ObsSpatial_BoundsRegion <- complete.cases(ObsSpatial_BoundsRegionna[,2])
        ObsAllSpatial_BoundsRegion <- ObsSpatial_BoundsRegionna[ObsSpatial_BoundsRegion, ]
        Regions_Layer <- add_column(ObsAllSpatial_BoundsRegion, Sp_Layer = "Regions", .after = 2) 
        colnames(Regions_Layer)[2] <- "Name"
        
        ## add Tribe column to sample data and remove NAs
        ObsSpatial_BoundsTribena <- add_column(df, Spatial_Boundtribe, .after = 1)
        ObsSpatial_BoundsTribe <- complete.cases(ObsSpatial_BoundsTribena[,2])
        ObsAllSpatial_BoundsTribe <- ObsSpatial_BoundsTribena[ObsSpatial_BoundsTribe, ]
        Tribes_Layer <- add_column(ObsAllSpatial_BoundsTribe, Sp_Layer = "Tribes", .after = 2) 
        colnames(Tribes_Layer)[2] <- "Name"
        
        ## append all sample boundaries to one df
        ObsAllSpatial_Bounds <- rbind(States_Layer,Regions_Layer,Tribes_Layer)
        
      } else { # instead of lat lon user provides columns declaring spatial boundaries
        #NM 8/30 - test data
        #df <- read.csv("WINN MAR 2019 Tool Input Data new.csv", skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
        #df <- read.csv("WQ Screening Input Data Sampling Event 1.csv", skip =0, header = TRUE, na.strings = "NA",stringsAsFactors=FALSE, encoding = "UTF-8")
        df2 <- add_column(df, Sp_Layer = "State", Lat = NA, Lon = NA, .after = 2) # adjust to 2 if not using distance from GKM
        Tribes_col <- df2[complete.cases(df2$Tribe),]
        if (nrow(Tribes_col) > 0) {Tribes_col$Sp_Layer <- "Tribes"}
        Tribes_col2 <- df2[complete.cases(df2$Secondary_Tribe),]
        if (nrow(Tribes_col2) > 0) {Tribes_col2$Sp_Layer <- "Tribes"}
        Regions_col <- df2[complete.cases(df2$Region),]
        if (nrow(Regions_col) > 0) {Regions_col$Sp_Layer <- "Regions"}
        names(Tribes_col)[names(Tribes_col)=="Tribe"] <- "Name"
        names(Tribes_col2)[names(Tribes_col2)=="Secondary_Tribe"] <- "Name"
        #names(Regions_col)[names(Regions_col)=="Regions"] <- "Name" #NM 8/30 change from "Regions" to "Region"
        names(Regions_col)[names(Regions_col)=="Region"] <- "Name" #NM 8/30 change from "Regions" to "Region"
        names(df2)[names(df2)=="State"] <- "Name"
        # NM 8/30 
        # a <- df2[, -which(names(df2) %in% c("Tribe","Secondary_Tribe","Region"))]
        # b <- Regions_col[, -which(names(Regions_col) %in% c("State","Tribe","Secondary_Tribe"))]
        # c <- Tribes_col[, -which(names(Tribes_col) %in% c("State","Secondary_Tribe","Region"))]
        # d <- Tribes_col2[, -which(names(Tribes_col2) %in% c("State","Tribe","Region"))]
        # rbind(a,b,c,d)
        # names(a) == names(b)
        # names (a) == names(c)
        # 
        ObsAllSpatial_Bounds <- rbind(df2[, -which(names(df2) %in% c("Tribe","Secondary_Tribe","Region"))],
                                      Regions_col[, -which(names(Regions_col) %in% c("State","Tribe","Secondary_Tribe"))],
                                      Tribes_col[, -which(names(Tribes_col) %in% c("State","Secondary_Tribe","Region"))],
                                      Tribes_col2[, -which(names(Tribes_col2) %in% c("State","Tribe","Region"))])
     
        ObsAllSpatial_Bounds <- filter(ObsAllSpatial_Bounds, Name != "")
      }

      ## Cap hardness values based on specific criteria
      obsCapped <- within(ObsAllSpatial_Bounds, Hardness[Hardness>400] <- 400) #Maximum hardness of 400 mg/L for most criteria in the region
      index <- 1 + which(colnames(obsCapped)=="Hardness" )
      samples_long <- gather(obsCapped, "variable", "conc", index:ncol(obsCapped))

      if (input$Categories==TRUE)  {
          GroupCategories <- colnames(samples_long) [(which(colnames(samples_long)=="Lon")+1):(which(colnames(samples_long)=="Hardness")-1)]
          ScreenCategories <- c(GroupCategories, "variable")
          samples_long <- samples_long %>% mutate(Sample_Type = ifelse(Name=="New Mexico" & Sample_Type=="Total" & variable=="Aluminum",
                                                                       "Total Recoverable", 
                                                                       Sample_Type))
          UniqueObs <- unique(samples_long[ScreenCategories])
          OutputCategories <- c("Designated_Use","ScreenType",GroupCategories,"Metal","Times_Exceeded","Number_Screened")
          
          output_screen <- data.frame(matrix(ncol = length(OutputCategories), nrow = rows),
                                      stringsAsFactors=FALSE)
          names(output_screen) <- OutputCategories
          output_screen[,OutputCategories] <- lapply(output_screen[,OutputCategories],as.character)
          output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
          output_screen$Number_Screened <- as.numeric(output_screen$Number_Screened)
          output_screen$Times_Exceeded[is.na(output_screen$Times_Exceeded)] <- 0
          output_screen$Number_Screened[is.na(output_screen$Number_Screened)] <- 0
          output_screen[is.na(output_screen)] <- ""
      } else {
          samples_long <- samples_long %>% mutate(Sample_Type = ifelse(Name=="New Mexico" & Sample_Type=="Total" & variable=="Aluminum",
                                                                       "Total Recoverable", 
                                                                       Sample_Type))
          UniqueObs <- unique(samples_long[c("Name","Sample_Type", "variable")])
          output_screen <- data.frame(Designated_Use = character(rows),
                                      ScreenType = character(rows),
                                      Name = character(rows),
                                      Sample_Type = character(rows),
                                      Metal = character(rows),
                                      Times_Exceeded = numeric(rows),
                                      Number_Screened = numeric(rows),
                                      stringsAsFactors=FALSE)
          #print("hi")
      }
      
      if (input$Categories==TRUE)  {
          SamplemarkerlayerCategories <- c("Date_Time","Site_ID", "Designated_Use","Sp_Layer",
                                           GroupCategories,
                                           "ScreenType","Lat","Lon", "CritMetal","Criteria","SampleValue","ObsMetal") 
          Samplemarkerlayer <- data.frame(matrix(ncol = length(SamplemarkerlayerCategories), nrow = rows*10), stringsAsFactors=FALSE)
          colnames(Samplemarkerlayer) <- SamplemarkerlayerCategories
      } else {
          Samplemarkerlayer <- data.frame(Date_Time = character(rows*10),
                                          Site_ID = character(rows*10),
                                          Designated_Use = character(rows*10), 
                                          Sp_Layer = character(rows*10),
                                          ScreenType = character(rows*10), 
                                          Lat = numeric(rows*10),
                                          Lon = numeric(rows*10),
                                          Name = character(rows*10), 
                                          Sample_Type = character(rows*10), 
                                          CritMetal = character(rows*10), 
                                          Criteria = numeric(rows*10), 
                                          SampleValue = numeric(rows*10),
                                          ObsMetal = character(rows*10),
                                          stringsAsFactors=FALSE)
       #   print("hi2")
      }
      ## This is the main function of the tool. For each sample the applicable screening criteria are identified and used to 
      ## determine the number of times a WQ criteria has been exceeded for a specific screen.
    # i=5
      for (i in 1:nrow(UniqueObs)) { #loops through each sample by unique combinations of region, conc type(row), and metal
          print(UniqueObs[i,])
          
          nCategories <- as.data.frame(UniqueObs[,-c(1,ncol(UniqueObs)-1,ncol(UniqueObs))], stringsAsFactors=FALSE)
#NM 6/22 change capitalization of Name in UniqueObs$Name
          #NM 8/30, change it back to lowercase
          screen <- filter(WQCritAll, Name==UniqueObs$Name[i], #iteratively queries WQ criteria based on sample data (sample & metal)
          #screen <- filter(WQCritAll, Name==UniqueObs$NAME[i], #iteratively queries WQ criteria based on sample data (sample & metal)
                           variable==UniqueObs$variable[i],
                           Sample_Type==UniqueObs$Sample_Type[i]) 
          
          if (length(screen$value) > 0){
              if (input$Categories==TRUE)  { # Converts designated columns into Categories
                  filtercolumns <- which((names(samples_long) %in% names(UniqueObs[i,]))==TRUE)
                  filt1 <- NULL
                  filt2 <- NULL
                  filtervar <- NULL
                  for (l in 1:length(filtercolumns)){ # generates variable with string to pass to filter_
                      filt1[l] <- names(samples_long[filtercolumns[l]])
                      filt2[l] <-UniqueObs[i,l]
                      filtervar[l] <-paste(filt1[l],"==","'",filt2[l],"'", sep="")
                  }
                  filtervar_collapse <- paste(filtervar, collapse = " & ")
                  tempSamples <- filter_(samples_long, filtervar_collapse)
                  # tempSamples <- samples_long %>% filter(UQ(rlang::sym(filt1[1]))==filt2[1]) %>%
                  #     filter(UQ(rlang::sym(filt1[2]))==filt2[2]) %>%
                  #     filter(UQ(rlang::sym(filt1[3]))==filt2[3]) %>%
                  #     filter(UQ(rlang::sym(filt1[4]))==filt2[4]) %>%
                  #     filter(UQ(rlang::sym(filt1[5]))==filt2[5])
                    
              } else {
                  tempSamples <- filter(samples_long, Name==UniqueObs$Name[i], Sample_Type==UniqueObs$Sample_Type[i], variable == UniqueObs$variable[i]) #subset observed data by unique combination
              }
              
              if (UniqueObs$Name[i]=="New Mexico" & 
                  UniqueObs$Sample_Type[i]=="Total" & 
                  UniqueObs$variable[i]=="Aluminum") { #New Mexico hardness limit for total Al = 220 mg/L
                  tempSamples <- tempSamples %>% within(Hardness[Hardness>220] <- 220) %>%
                      mutate(Sample_Type = "Total Recoverable",
                             conc = conc*0.31)
              }
             
              for (b in 1:length(screen$ScreenType)) { #loop through matching screens 
                  if (!is.na(screen$maSlope[b]==TRUE)) { #find screens that need to be calculated based on hardness
                      if (input$Categories==TRUE)  {
                          aquaticScreenCategories <- c("Date_Time","Site_ID", "Designated_Use","Sp_Layer",
                                                       GroupCategories,
                                                       "ScreenType","Lat","Lon", "CritMetal","Criteria","SampleValue","ObsMetal") 
                          aquatic_screen <- data.frame(matrix(ncol = length(aquaticScreenCategories), nrow = nrow(tempSamples)), stringsAsFactors=FALSE)
                          colnames(aquatic_screen) <- aquaticScreenCategories
                          g=1
                      } else {
                          aquatic_screen <- data.frame(Date_Time = character(nrow(tempSamples)),
                                                       Site_ID = character(nrow(tempSamples)),
                                                       Designated_Use = character(nrow(tempSamples)),
                                                       Sp_Layer = character(nrow(tempSamples)),
                                                       ScreenType = character(nrow(tempSamples)),
                                                       Lat = numeric(nrow(tempSamples)),
                                                       Lon = numeric(nrow(tempSamples)),
                                                       Name = character(nrow(tempSamples)),
                                                       Sample_Type = character(nrow(tempSamples)),
                                                       CritMetal = character(nrow(tempSamples)),
                                                       Criteria = numeric(nrow(tempSamples)),
                                                       SampleValue = numeric(nrow(tempSamples)),
                                                       ObsMetal = character(nrow(tempSamples)),
                                                       stringsAsFactors=FALSE)
                          g=1
                      } 
                      if (screen$alphaBeta[b] == 0) { #calculator function 1 y=1
                          for (y in 1:nrow(tempSamples)) { #iterate through each sample 
                              screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(tempSamples$Hardness[y]))+screen$mbIntercept[b])*screen$conversionFactor[b])/1) #calculate criteria
                              if (input$Categories==TRUE)  {
                              aquatic_screenvars1 <- c(tempSamples$Date_Time[y], # Date_Time
                                               tempSamples$Site_ID[y], # Site_ID
                                               screen$Designated_Use[b], # Designated_Use
                                               tempSamples$Sp_Layer[y], # Sp_Layer
                                               screen$Name[b]) # Name
                              aquatic_screenvars2 <- NULL
                              
                              for (x in 1:length(GroupCategories[-c(1,length(GroupCategories))])) { # gets 
                                  aquatic_screenvars2[x] <- if (length(GroupCategories[-length(GroupCategories)])>1) {nCategories[i,x]} else {nCategories[i]}
                              }
                              aquatic_screenvars3 <- c(screen$Sample_Type[b], # Sample_Type
                                                       screen$ScreenType[b], # ScreenType
                                                       tempSamples$Lat[y], # Lat
                                                       tempSamples$Lon[y], # Lon
                                                       screen$variable[b], # CritMetal
                                                       as.numeric(screen$value[b]), # Criteria
                                                       as.numeric(tempSamples$conc[y]), # SampleValue
                                                       tempSamples$variable[y]) # ObsMetal
                              
                              aquatic_screenvarTot <- c(aquatic_screenvars1,aquatic_screenvars2,aquatic_screenvars3)
                              aquatic_screen[g,] <- aquatic_screenvarTot
                              aquatic_screen[, c("Criteria", "SampleValue", "Lat", "Lon")] <- sapply(aquatic_screen[, c("Criteria", "SampleValue", "Lat", "Lon")], as.numeric)
                              g=g+1
                              } else {
                              aquatic_screen[g,] <- c(tempSamples$Date_Time[y],
                                                      tempSamples$Site_ID[y],
                                                      screen$Designated_Use[b],
                                                      tempSamples$Sp_Layer[y],
                                                      screen$ScreenType[b],
                                                      tempSamples$Lat[y],
                                                      tempSamples$Lon[y],
                                                      screen$Name[b],
                                                      screen$Sample_Type[b],
                                                      screen$variable[b],
                                                      as.numeric(screen$value[b]),
                                                      as.numeric(tempSamples$conc[y]),
                                                      tempSamples$variable[y]) #collect criteria and sample value (for screen eval)
                              
                              aquatic_screen[, c("Criteria", "SampleValue", "Lat", "Lon")] <- sapply(aquatic_screen[, c("Criteria", "SampleValue", "Lat", "Lon")], as.numeric)
                              g=g+1
                              }
                              }
                      } else if (screen$alphaBeta[b] == 1) { #calculator function 2 
                          for (z in 1:nrow(tempSamples)) { #iterate through each sample
                              screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(tempSamples$Hardness[z])+screen$mbIntercept[b]))*(screen$alpha[b]-(log(tempSamples$Hardness[z])*screen$beta[b])))/1) #calculate criteria
                              if (input$Categories==TRUE)  {
                                  aquatic_screenvars1 <- c(tempSamples$Date_Time[y], # Date_Time
                                                           tempSamples$Site_ID[y], # Site_ID
                                                           screen$Designated_Use[b], # Designated_Use
                                                           tempSamples$Sp_Layer[y], # Sp_Layer
                                                           screen$Name[b]) # Name
                                  aquatic_screenvars2 <- NULL
                                  
                                  for (x in 1:length(GroupCategories[-c(1,length(GroupCategories))])) { # gets 
                                      aquatic_screenvars2[x] <- if (length(GroupCategories[-length(GroupCategories)])>1) {nCategories[i,x]} else {nCategories[i]}
                                  }
                                  aquatic_screenvars3 <- c(screen$Sample_Type[b], # Sample_Type
                                                           screen$ScreenType[b], # ScreenType
                                                           tempSamples$Lat[y], # Lat
                                                           tempSamples$Lon[y], # Lon
                                                           screen$variable[b], # CritMetal
                                                           as.numeric(screen$value[b]), # Criteria
                                                           as.numeric(tempSamples$conc[y]), # SampleValue
                                                           tempSamples$variable[y]) # ObsMetal
                                  
                                  aquatic_screenvarTot <- c(aquatic_screenvars1,aquatic_screenvars2,aquatic_screenvars3)
                                  aquatic_screen[g,] <- aquatic_screenvarTot
                                  aquatic_screen[, c("Criteria", "SampleValue", "Lat", "Lon")] <- sapply(aquatic_screen[, c("Criteria", "SampleValue", "Lat", "Lon")], as.numeric)
                                  g=g+1
                              } else {
                                  aquatic_screen[g,] <- c(tempSamples$Date_Time[z],
                                                          tempSamples$Site_ID[z],
                                                          screen$Designated_Use[b],
                                                          tempSamples$Sp_Layer[z],
                                                          screen$ScreenType[b],
                                                          tempSamples$Lat[z],
                                                          tempSamples$Lon[z],
                                                          screen$Name[b],
                                                          screen$Sample_Type[b],
                                                          screen$variable[b],
                                                          as.numeric(screen$value[b]),
                                                          as.numeric(tempSamples$conc[z]),
                                                          tempSamples$variable[z]) #collect criteria and sample value (for screen eval)
                                  
                                  aquatic_screen[, c("Criteria", "SampleValue", "Lat", "Lon")] <- sapply(aquatic_screen[, c("Criteria", "SampleValue", "Lat", "Lon")], as.numeric)
                                  g=g+1
                              }
                          }
                         } else {
                          cat("Something went wrong with the hardness calculator.", "The error occured calculating the screening criteria for",screen$Sample_Type[b],
                              screen$variable[b], "using the",screen$ScreenType[b], "screen for",screen$Name[b])
                      }
                      
                      aquatic_screen_cleaned <- filter(aquatic_screen, Criteria >= 0 & SampleValue >= 0 & CritMetal != "") #remove empty rows in data.frame
                      n_screened <- nrow(aquatic_screen_cleaned) #count the number of samples that are screened
                      n_screened[is.null(n_screened)] <- -50
                      if (n_screened > 0) {
                          metal_vector_exceedances <- aquatic_screen_cleaned[which(aquatic_screen_cleaned$SampleValue > aquatic_screen_cleaned$Criteria),] #filter criteria with exceedances
                          metal_exceedance_count <- nrow(metal_vector_exceedances) #count exceedances
                          m=m+1
                          
                          if (input$Categories==TRUE)  {
                              for (f in 1:nrow(aquatic_screen_cleaned)) {
                                  n=n+1
                                  Samplemarkerlayer1 <- c(aquatic_screen_cleaned$Date_Time[f], # Date_Time
                                                       aquatic_screen_cleaned$Site_ID[f], # Site_ID
                                                       aquatic_screen_cleaned$Designated_Use[f], # Designated_Use
                                                       aquatic_screen_cleaned$Sp_Layer[f], # Sp_Layer
                                                       aquatic_screen_cleaned$Name[f]) # Name
                                  Samplemarkerlayer2 <- NULL
                                  
                              for (x in 1:length(GroupCategories[-c(1,length(GroupCategories))])) { # gets 
                                  Samplemarkerlayer2[x] <- if (length(GroupCategories[-length(GroupCategories)])>1) {nCategories[i,x]} else {nCategories[i]}
                              }
                                  Samplemarkerlayer3 <- c(aquatic_screen_cleaned$Sample_Type[f], # Sample_Type
                                                          aquatic_screen_cleaned$ScreenType[f], # ScreenType
                                                          aquatic_screen_cleaned$Lat[f], # Lat
                                                          aquatic_screen_cleaned$Lon[f], # Lon
                                                          aquatic_screen_cleaned$CritMetal[f], # CritMetal
                                                       as.numeric(aquatic_screen_cleaned$Criteria[f]), # Criteria
                                                       as.numeric(aquatic_screen_cleaned$SampleValue[f]), # SampleValue
                                                       aquatic_screen_cleaned$ObsMetal[f]) # ObsMetal
                              
                                  SamplemarkerlayerTot <- c(Samplemarkerlayer1,Samplemarkerlayer2,Samplemarkerlayer3)
                                  Samplemarkerlayer[n,] <- SamplemarkerlayerTot
                              }
                              
                              screenvars1 <- c(screen$Designated_Use[b],
                                               screen$ScreenType[b],
                                               screen$Name[b])
                              screenvars2 <- NULL
                              
                              for (x in 1:length(GroupCategories[-c(1,length(GroupCategories))])) {
                                  screenvars2[x] <- if (length(GroupCategories[-length(GroupCategories)])>1) {nCategories[i,x]} else {nCategories[i]}
                              }
                              screenvars3 <- c(screen$Sample_Type[b],
                                               screen$variable[b],
                                               metal_exceedance_count,
                                               n_screened)
                              
                              screenvarTot <- c(screenvars1,screenvars2,screenvars3)
                              output_screen[m,] <- screenvarTot
                          } else {
                              for (f in 1:nrow(aquatic_screen_cleaned)) {
                                  n=n+1
                                  Samplemarkerlayer[n,] <- aquatic_screen_cleaned[f,]
                              }
                              output_screen[m,] <- c(screen$Designated_Use[b],
                                                     screen$ScreenType[b],
                                                     screen$Name[b],
                                                     screen$Sample_Type[b],
                                                     screen$variable[b],
                                                     metal_exceedance_count,
                                                     n_screened)
                          }
                      }
                  } else {
                      if (!all(is.na(tempSamples$conc))) { #distinguishes between a non-detect sample and no sample
                          metal_vector_nonas <- tempSamples[!is.na(tempSamples$conc),]#remove NAs
                          num_metal_samples <- nrow(metal_vector_nonas) #count the number of samples that are screened
                          if(is.null(num_metal_samples)){num_metal_samples <- -50}
                          if (num_metal_samples > 0) {
                              metal_vector_exceedances <- metal_vector_nonas[which(metal_vector_nonas$conc>screen$value[b]),] #filter criteria with exceedances
                              metal_exceedance_count <- nrow(metal_vector_exceedances) #count exceedances
                              m=m+1
                              
                              if (input$Categories==TRUE)  {
                                  for (f in 1:num_metal_samples) {
                                      n=n+1
                                      Samplemarkerlayer1 <- c(metal_vector_nonas$Date_Time[f], # Date_Time
                                                              metal_vector_nonas$Site_ID[f], # Site_ID
                                                              screen$Designated_Use[b], # Designated_Use
                                                              metal_vector_nonas$Sp_Layer[f], # Sp_Layer
                                                              metal_vector_nonas$Name[f]) # Name
                                      Samplemarkerlayer2 <- NULL
                                      
                                      for (x in 1:length(GroupCategories[-c(1,length(GroupCategories))])) { # gets 
                                          Samplemarkerlayer2[x] <- if (length(GroupCategories[-length(GroupCategories)])>1) {nCategories[i,x]} else {nCategories[i]}
                                      }
                                      Samplemarkerlayer3 <- c(metal_vector_nonas$Sample_Type[f], # Sample_Type
                                                              screen$ScreenType[b], # ScreenType
                                                              metal_vector_nonas$Lat[f], # Lat
                                                              metal_vector_nonas$Lon[f], # Lon
                                                              screen$variable[b], # CritMetal
                                                              as.numeric(screen$value[b]), # Criteria
                                                              as.numeric(metal_vector_nonas$conc[f]), # SampleValue
                                                              unique(tempSamples$variable)) # ObsMetal
                                      
                                      SamplemarkerlayerTot <- c(Samplemarkerlayer1,Samplemarkerlayer2,Samplemarkerlayer3)
                                      Samplemarkerlayer[n,] <- SamplemarkerlayerTot
                                  }
                             
                                  #nCategories <- (UniqueObs[,c(-1,-ncol(UniqueObs))])
                                  screenvars1 <- c(screen$Designated_Use[b],
                                                   screen$ScreenType[b],
                                                   screen$Name[b])
                                  screenvars2 <- NULL
                                  
                                  for (x in 1:length(GroupCategories[-c(1,length(GroupCategories))])) {
                                  #for (x in 1:length(GroupCategories[-length(GroupCategories)])) {
                                      screenvars2[x] <- if (length(GroupCategories[-length(GroupCategories)])>1) {nCategories[i,x]} else {nCategories[i]}
                                  }
                                  screenvars3 <- c(screen$Sample_Type[b],
                                                   screen$variable[b],
                                                   metal_exceedance_count,
                                                   num_metal_samples)
                                  
                                  screenvarTot <- c(screenvars1,screenvars2,screenvars3)
                                  output_screen[m,] <- screenvarTot
                              } else {
                                  for (t in 1:num_metal_samples) {
                                      n=n+1
                                      Samplemarkerlayer[n,] <-  c(metal_vector_nonas$Date_Time[t], 
                                                                  metal_vector_nonas$Site_ID[t],
                                                                  screen$Designated_Use[b], 
                                                                  metal_vector_nonas$Sp_Layer[t],
                                                                  screen$ScreenType[b], 
                                                                  metal_vector_nonas$Lat[t],
                                                                  metal_vector_nonas$Lon[t],
                                                                  metal_vector_nonas$Name[t], 
                                                                  metal_vector_nonas$Sample_Type[t],
                                                                  screen$variable[b],
                                                                  screen$value[b],
                                                                  metal_vector_nonas$conc[t],
                                                                  unique(tempSamples$variable))   
                                  }
                                  
                                  output_screen[m,] <- c(screen$Designated_Use[b],
                                                         screen$ScreenType[b],
                                                         screen$Name[b],
                                                         screen$Sample_Type[b],
                                                         screen$variable[b],
                                                         metal_exceedance_count,
                                                         num_metal_samples)
                              }
                          }
                      }
                  }
              }
          } else {
              cat(UniqueObs$Sample_Type[i], 
                  UniqueObs$variable, 
                  UniqueObs$Name[i], 
                  file="echoFile.txt", append=TRUE)
          }
      }
      
      output_screen <- filter(output_screen, ScreenType!="")
      output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
      output_screen_Exceeded <- filter(output_screen, Times_Exceeded > 0)
      #write.csv(WQCritAll,file="WQCritAll.csv", row.names=FALSE) NM 8/30 changing name to more descriptive file
      #write.csv(output_screen,file="Reload1_file.csv", row.names=FALSE)
      write.csv(output_screen,file="SampleExceedCount.csv", row.names=FALSE)
      
      if (exists("Samplemarkerlayer")){ 
          samplemarkers_screen <- filter(Samplemarkerlayer, ScreenType!="") %>%
              mutate(SampleValue = as.numeric(SampleValue),
                     Criteria = as.numeric(Criteria),
                     Hazard_Quotient = SampleValue/Criteria,
                     # Difference = SampleValue-Criteria, 
                     Result = ifelse(SampleValue-Criteria < 1,"NotExceeded","Exceeded"),
                     Lat = as.numeric(Lat),
                     Lon = as.numeric(Lon),
                     #NM 8/30 - removed date formatting, it doesn't show up otherwise  #Date format assistance https://www.stat.berkeley.edu/~s133/dates.html
                     #Date_Time = format(as.POSIXct(Date_Time,format = '%m/%d/%Y %H:%M'),format='%m/%d/%Y %H:%M')
                     ) # %H:%M
#         write.csv(samplemarkers_screen,file="Samplelatlondiferences.csv",row.names = FALSE) NM 8/30 - changed name to more descriptive output
          write.csv(samplemarkers_screen,file="SampleComparisonToCriteria.csv",row.names = FALSE)

                }
      
      #NM 9/17 - added this to page of tables
      output$Results = renderDataTable({
        output_screen
        #NM 9/17 - removing this to just return all screen data, not just parameters that have been exceeded
        # if (input$checked==FALSE)  {output_screen
        # } else {
        #   if (input$checked==TRUE) {output_screen_Exceeded}
        # }
      })
      
      #NM 9/17 - added download option in app
      output$screendownload <- downloadHandler(
        filename = function() {
          paste("SampleExceedCount_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(output_screen, file)
        }
      )
      
      #NM 9/17 - added this to page of tables
      output$Screen = renderDataTable({
      samplemarkers_screen
      }, options = list(scrollX = TRUE)) #https://github.com/rstudio/DT/issues/407
      
      #NM 9/17 Added download option for table in app
      output$criteriadownload <- downloadHandler(
        filename = function() {
          paste("SampleComparisonToCriteria_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(samplemarkers_screen, file)
        }
      )
      
      output$Pivot1 = renderRpivotTable({
        #Previous code: 
        #rpivotTable(data=output_screen, rows = c("Designated_Use","Time_Period"),cols = c("Metal","River","Name"), rendererName = "Bar Chart",aggregatorName = "Sum over Sum", vals = c("Times_Exceeded","Number_Screened"))
        
        #NM 8/30 Adjusted to reflect new columns - some of the previous ones are no longer in the dataset
        rpivotTable(data=output_screen, rows = c("Designated_Use","ScreenType"),cols = c("Sample_Type","Metal","Name"), rendererName = "Bar Chart",aggregatorName = "Sum over Sum", vals = c("Times_Exceeded","Number_Screened"))
      })
      output$screenprogress <- renderPrint({
        message("Screen Complete")
        cat("Screen Complete")
      })
      ###################### End of what click does ############################################################
      #Filter data
      datFilt <- reactive({ 
        #if (exists("samplemarkers_screen")){ 
        samplemarkers_screen[samplemarkers_screen$Sp_Layer == input$Authority & 
                               samplemarkers_screen$CritMetal == input$Contaminant & 
                               samplemarkers_screen$Sample_Type == input$Sampletype & 
                               samplemarkers_screen$ScreenType == input$Criteria,]
        #}
      })
      output$Samplenrows <- reactive({
        nrow(datFilt())
        print(nrow(datFilt()))
      })
      observe({
        outputOptions(output, "Samplenrows", suspendWhenHidden = FALSE)  
      })
      samplepal <- colorFactor(c("red","navy"), domain = c("NotExceeded","Exceeded"))
      
      observeEvent(input$Clickmap, {
        QueryCrieria<-reactive({
          # Get a subset of the criteria data based on drop down box selection
          dataSet <- WQCritAll[WQCritAll$Spatial_Type == input$Authority & 
                                 WQCritAll$variable == input$Contaminant & 
                                 WQCritAll$Sample_Type == input$Sampletype & 
                                 WQCritAll$ScreenType == input$Criteria,] #c("Spatial_Bound","value")
          # Copy our GIS data
          joinedDataset<-Authority_Layer()
          # Join the two datasets together
          joinedDataset@data <- merge(dataSet,joinedDataset@data)
          joinedDataset
        })
        #observe({
        if(is.null(datFilt())) {
          print("Nothing selected")
          #leafletProxy("map") %>% clearMarkers()
        }
        else{
          if (input$ImportedSamples==FALSE)  {
            leafletProxy("map") %>% clearMarkers()
          } else {
            if (input$ImportedSamples==TRUE) {

              leafletProxy("map",data=datFilt()) %>%
                clearMarkers() %>%
                addCircleMarkers(~Lon, ~Lat,
                                 #layerId = "ImportedSamples",
                                 radius = ~ifelse(Result == "NotExceeded", 3, ifelse(Difference >5,5,3+Difference)), #abs(Difference)
                                 color = ~samplepal(Result),
                                 popup = ~paste("<strong>Sample Date: </strong>", 
                                                as.POSIXlt(Date_Time,format= '%m/%d/%Y %H:%M',
                                                           tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                          "%Y/%m/%d %H:%M:%OS",
                                                                          "%Y-%m-%d %H:%M",
                                                                          "%Y/%m/%d %H:%M",
                                                                          "%Y-%m-%d",
                                                                          "%Y/%m/%d")), 
                                                "<br><strong>Sample Site: </strong>",
                                                as.character(Site_ID),
                                                "<br><strong>Contaminant: </strong>",
                                                as.character(Sample_Type)," ",
                                                as.character(CritMetal),
                                                "<br><strong>Criteria Value (mg/L): </strong>",
                                                round(as.numeric(Criteria),digits = 4),
                                                "<br><strong>Sample Value (mg/L): </strong>",
                                                round(as.numeric(SampleValue),digits = 4)),
                                 stroke = FALSE, 
                                 fillOpacity = 0.5,
                                 #clusterOptions=markerClusterOptions(),
                                 group = "Imported Samples") %>%
                    fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat))
                   
              #addLayersControl(
              #overlayGroups = "Imported Samples",
              #options = layersControlOptions(collapsed = FALSE))
            } }
        }
        
      })
      #})
      
      #observe({
      # if(is.null(QueryCrieria())) {
      #   print("No Citeria Selected")
      #   #leafletProxy("map") %>% clearMarkers()
      # }
      # else{
      #   sp_selection <- QueryCrieria()
      #   
      #   sp_dataframe <- sp_selection@data
      #   #pal <- colorQuantile("YlGn", joinedDataset$value, n = 5) 
      #   binpal <- colorBin("Blues", sp_selection$value, 6, pretty = FALSE)
      #   #legendvalues <- sp_selection$value
      #   
      #   
      #   
      #   if(is.null(QueryCrieria())) {
      #     print("Nothing selected")
      #     leafletProxy("map") #%>% clearMarkers()
      #   }
      #   else{
      #     leafletProxy("map",data = QueryCrieria()) %>%
      #       clearShapes() %>%
      #       addPolygons(color = "#A9A9A9",
      #                   fillColor = ~binpal(sp_selection$value), 
      #                   stroke = TRUE, smoothFactor = 0.2,
      #                   opacity = 1.0, fillOpacity = 0.5, weight = 1,
      #                   popup = Criteria_popup) %>%
      #       setView(lng = -98.35, lat = 39.5,  zoom = 4) 
      #     #addLegend("bottomleft",pal = binpal, values = ~sp_dataframe$values, opacity = 1)
      #   }
      # }
      #})
      
    })
    observeEvent(input$reClick, {
      output_screen <- Reload1data()
      output_screen <- filter(output_screen, ScreenType!="")
      output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
      output_screen_Exceeded <- filter(output_screen, Times_Exceeded > 0)
      
      samplemarkers_screen <- Reload2data()
      
      output$Results = renderDataTable({
        if (input$checked==FALSE)  {output_screen
        } else {
          if (input$checked==TRUE) {output_screen_Exceeded}
        }
      })
      output$Pivot1 = renderRpivotTable({
        rpivotTable(data=output_screen, rows = c("Designated_Use","Time_Period"),cols = c("Metal","River","Name"), rendererName = "Bar Chart",aggregatorName = "Sum over Sum", vals = c("Times_Exceeded","Number_Screened"))
      })
      output$screenprogress <- renderPrint({
        message("Screen Complete")
        cat("Screen Complete")
        #message(samplemarkers)
      })
      
      ################## End  of what click does ############
      #Filter data
      datFilt <- reactive({ 
        #if (exists("samplemarkers_screen")){ 
        samplemarkers_screen[samplemarkers_screen$Sp_Layer == input$Authority & 
                               samplemarkers_screen$CritMetal == input$Contaminant & 
                               samplemarkers_screen$Sample_Type == input$Sampletype & 
                               samplemarkers_screen$ScreenType == input$Criteria,]
        #}
      })
      output$Samplenrows <- reactive({
        nrow(datFilt())
        print(nrow(datFilt()))
      })
      observe({
        outputOptions(output, "Samplenrows", suspendWhenHidden = FALSE)  
      })
      
      samplepal <- colorFactor(c("red","navy"), domain = c("NotExceeded","Exceeded"))
      
      observeEvent(input$Clickmap, {
        QueryCrieria<-reactive({
          # Get a subset of the criteria data based on drop down box selection
          dataSet <- WQCritAll[WQCritAll$Spatial_Type == input$Authority & 
                                 WQCritAll$variable == input$Contaminant & 
                                 WQCritAll$Sample_Type == input$Sampletype & 
                                 WQCritAll$ScreenType == input$Criteria,] #c("Spatial_Bound","value")
          # Copy our GIS data
          joinedDataset<-Authority_Layer()
          # Join the two datasets together
          joinedDataset@data <- merge(dataSet,joinedDataset@data)
          joinedDataset
        })
        #observe({
        if(is.null(datFilt())) {
          print("Nothing selected")
          #leafletProxy("map") %>% clearMarkers()
        }
        else{
          if (input$ImportedSamples==FALSE)  {
            leafletProxy("map") %>% clearMarkers()
          } else {
            if (input$ImportedSamples==TRUE) {
              leafletProxy("map",data=datFilt()) %>%
                clearMarkers() %>%
                addCircleMarkers(~Lon, ~Lat,
                                 #layerId = "ImportedSamples",
                                 radius = ~ifelse(Result == "NotExceeded", 3, ifelse(Difference >5,5,3+Difference)), #abs(Difference)
                                 color = ~samplepal(Result),
                                 popup = ~paste("<strong>Sample Date: </strong>", 
                                                as.POSIXlt(Date_Time,format= '%m/%d/%Y %H:%M',
                                                           tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                          "%Y/%m/%d %H:%M:%OS",
                                                                          "%Y-%m-%d %H:%M",
                                                                          "%Y/%m/%d %H:%M",
                                                                          "%Y-%m-%d",
                                                                          "%Y/%m/%d")), 
                                                "<br><strong>Sample Site: </strong>",
                                                as.character(Site_ID),
                                                "<br><strong>Contaminant: </strong>",
                                                as.character(Sample_Type)," ",
                                                as.character(CritMetal),
                                                "<br><strong>Criteria Value (mg/L): </strong>",
                                                round(as.numeric(Criteria),digits = 4),
                                                "<br><strong>Sample Value (mg/L): </strong>",
                                                round(as.numeric(SampleValue),digits = 4)),
                                 stroke = FALSE, 
                                 fillOpacity = 0.5,
                                 #clusterOptions=markerClusterOptions(),
                                 group = "Imported Samples") %>%
                fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat))
            } }
        }
      })
      #})
      
    })
    observeEvent(input$Demo, {
      # Work_DemoDir "L:/PRIV/AnimasRiver/ARP_ANALYTICS/Hazard Quotient Processing_Screening Tool/Water Quality Screening Tool_copy 062019/wq_screen-master/GKM Demo Files"
      # home_DemoDir "L:/PRIV/AnimasRiver/ARP_ANALYTICS/Hazard Quotient Processing_Screening Tool/Water Quality Screening Tool_copy 062019/wq_screen-master/GKM Demo Files"
      # output_screen <- read.csv("L:/PRIV/AnimasRiver/ARP_ANALYTICS/Hazard Quotient Processing_Screening Tool/Water Quality Screening Tool_copy 062019/wq_screen-master/GKM Demo Files/Reload1_file.csv",sep=",", 
      #                           header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)

#NM 6/22 changing file 
      output_screen <- read.csv("Reload1_file.csv",sep=",", 
                                header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
      output_screen <- filter(output_screen, ScreenType!="")
      output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
      output_screen_Exceeded <- filter(output_screen, Times_Exceeded > 0)
      # 
      # samplemarkers_screen <- read.csv("L:/PRIV/AnimasRiver/ARP_ANALYTICS/Hazard Quotient Processing_Screening Tool/Water Quality Screening Tool_copy 062019/wq_screen-master/GKM Demo Files/Samplelatlondiferences.csv",sep=",", 
      #                                  header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
#NM 6/22 changing file 
       samplemarkers_screen <- read.csv("Samplelatlondiferences.csv",sep=",", 
                                        header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
      #NM 8/30 - changing file name to reflect output
      # samplemarkers_screen <- read.csv("SampleComparisonToCriteria.csv",sep=",", 
      #                                  header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
      
      output$Results = renderDataTable({
        if (input$checked==FALSE)  {output_screen
        } else {
          if (input$checked==TRUE) {output_screen_Exceeded}
        }
      })
      output$Pivot1 = renderRpivotTable({
        rpivotTable(data=output_screen, rows = c("Designated_Use","Time_Period"),cols = c("Metal","River","Name"), rendererName = "Bar Chart",aggregatorName = "Sum over Sum", vals = c("Times_Exceeded","Number_Screened"))
      })
      output$screenprogress <- renderPrint({
        message("Screen Complete")
        cat("Screen Complete")
        #message(samplemarkers)
      })
      
      ################## End  of what click does ############
      #Filter data
      datFilt <- reactive({ 
        #if (exists("samplemarkers_screen")){ 
        samplemarkers_screen[samplemarkers_screen$Sp_Layer == input$Authority & 
                               samplemarkers_screen$CritMetal == input$Contaminant & 
                               samplemarkers_screen$Sample_Type == input$Sampletype & 
                               samplemarkers_screen$ScreenType == input$Criteria,]
        #}
      })
      
      output$Samplenrows <- reactive({
        nrow(datFilt())
        print(nrow(datFilt()))
      })
      observe({
        outputOptions(output, "Samplenrows", suspendWhenHidden = FALSE)  
      })
      
      samplepal <- colorFactor(c("red","navy"), domain = c("NotExceeded","Exceeded"))
      
      observeEvent(input$Clickmap, {
        QueryCrieria<-reactive({
          # Get a subset of the criteria data based on drop down box selection
          dataSet <- WQCritAll[WQCritAll$Spatial_Type == input$Authority & 
                                 WQCritAll$variable == input$Contaminant & 
                                 WQCritAll$Sample_Type == input$Sampletype & 
                                 WQCritAll$ScreenType == input$Criteria,] #c("Spatial_Bound","value")
          # Copy our GIS data
          joinedDataset<-Authority_Layer()
          # Join the two datasets together
          joinedDataset@data <- merge(dataSet,joinedDataset@data)
          joinedDataset
        })
        
        #observe({
        if(is.null(datFilt())) {
          print("Nothing selected")
        }
        else{
          if (input$ImportedSamples==FALSE)  {
            leafletProxy("map") %>% clearMarkers()
          } else {
            if (input$ImportedSamples==TRUE) {
              leafletProxy("map",data=datFilt()) %>%
                clearMarkers() %>%
                addCircleMarkers(~Lon, ~Lat,
                                 #layerId = "ImportedSamples",
                                 radius = ~ifelse(Result == "NotExceeded", 3, ifelse(Difference >5,5,3+Difference)), #abs(Difference)
                                 color = ~samplepal(Result),
                                 popup = ~paste("<strong>Sample Date: </strong>", 
                                                as.POSIXlt(Date_Time,format= '%m/%d/%Y %H:%M',
                                                           tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                          "%Y/%m/%d %H:%M:%OS",
                                                                          "%Y-%m-%d %H:%M",
                                                                          "%Y/%m/%d %H:%M",
                                                                          "%Y-%m-%d",
                                                                          "%Y/%m/%d")), 
                                                "<br><strong>Sample Site: </strong>",
                                                as.character(Site_ID),
                                                "<br><strong>Contaminant: </strong>",
                                                as.character(Sample_Type)," ",
                                                as.character(CritMetal),
                                                "<br><strong>Criteria Value (mg/L): </strong>",
                                                round(as.numeric(Criteria),digits = 4),
                                                "<br><strong>Sample Value (mg/L): </strong>",
                                                round(as.numeric(SampleValue),digits = 4)),
                                 stroke = FALSE, 
                                 fillOpacity = 0.5,
                                 #clusterOptions=markerClusterOptions(),
                                 group = "Show Imported Samples") %>%
                fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat))
            } }
        }
      })
      val <- max(samplemarkers_screen$Date_Time)
      output$animationSlider <- renderUI({
        sliderInput("animationSlider2", "Date Range", min = as.Date("2015-08-05 00:00"), 
                    max = as.Date(val), value = as.Date("2015-08-05 00:00"), step = 1, 
                    animate = animationOptions(200))
      })
      
      points <- reactive({
        samplemarkers_screen %>% 
          filter(Date_Time==input$animationSlider2)
      })
      
      output$mymap <- renderLeaflet({
        leaflet() %>%
          addMarkers(data = points())#,popup=as.character(points()$a))
      })
      
      
    })
}
shinyApp(ui= ui, server = server)
