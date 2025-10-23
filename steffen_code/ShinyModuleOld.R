##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## DOWNLOAD TRACKING DATA AND PREDICT PLAUSIBLE NEST LOCATIONS  #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
## written by Lara Gross and Steffen Oppel for Red Kites and other species
### generalised for generic Movebank login
### steffen.oppel@vogelwarte.ch
### finalised 21 Sept 2024
### publicly available at https://steffenoppel.shinyapps.io/NestFindApp/


if (!require("pacman")) install.packages("pacman")
# for shiny app
pacman::p_load(tidyverse, #includes tidyr (for using gather(), rearranging data), dplyr, ggplot2
               lemon, #for function facet_rep_wrap()
               sf,
               suncalc,
               RANN,
               dtplyr,
               plotly,
               stringr,
               htmltools,
               lubridate, #time stuff
               viridis, #viridis color scale
               shiny, #shiny app
               shinythemes, #ggmap, #if using maps with download tiles in shiny output
               shinybusy, #for waiting symbol while app loads
               shinyjs, #previous/next button
               shinyWidgets,
               move, #movebank
               leaflet, #map making
               geosphere,
               svDialogs,
               data.table
)

select<-dplyr::select
filter<-dplyr::filter

Sys.setlocale("LC_TIME", "C")  #set English hours for correct x-axis
s <- Sys.time(); attr(s,"tzone") <- "UTC"





select<-dplyr::select
filter<-dplyr::filter

Sys.setlocale("LC_TIME", "C")  #set English hours for correct x-axis
s <- Sys.time(); attr(s,"tzone") <- "UTC"


########### 1 - Pre-shiny Preparation ###########
# code to make actionButtons also work by hitting Enter, left arrow and right arrow
# @Stéphane Laurent, https://stackoverflow.com/questions/56600232/detecting-arrow-key-cursor-key-in-shiny

# code to make actionButtons also work by hitting Enter (after clicking on them once)
jscode <- '$(document).keyup(function(event)) {
  if ((event.keyCode == 13)) {
  $("#button").click();}});'

# code formatting mouseover legend in dygraphs
valueFormatter <- "function(x) {
          var options = {weekday: 'short', year: 'numeric', month: '2-digit', day: '2-digit', hour: '2-digit', minute: '2-digit', 
          hour12: false, timeZone: 'UTC'};
          var dayX = new Date(x);
          return dayX.toLocaleString('en-SE', options);
        }"




############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~PART 2 THE APP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##########

########### 2 - first user interface ###########

ui <- fluidPage(
  
  tags$head(tags$style( #style to make the tabPanel title without border and with white text. I did not manage to remove the grey line next to it.
    HTML(".nav-tabs > li.active > a {
           color: white;
           border-style: none;
           }",
         ".navbar { background-color: red;}"
    ))),
  
  tabsetPanel(id = 'my_tabsetPanel',
              tabPanel('Login',
                       fluidRow(align = "center",
                                br(), br(), br(), br(), br(),
                                textInput("username", "Movebank username"),
                                passwordInput("password", "Movebank password"),
                                textInput("studyID1", "Movebank study NAME"),
                                textInput("studyID2", "(Optional) Second Movebank study NAME"),
                                numericInput(inputId="n.weeks", label="Timeframe (number of recent weeks to download data)", value=4,min=2),
                                numericInput(inputId="nestradius", label="Error radius (in m) of locations around likely nest site", value=50, min=10, max=500),
                                actionButton("go", "Sign in to retrieve data from Movebank", 
                                             class = "btn-info") #or: btn-primary for bright blue
                       )
              )
  ),
  
  add_busy_gif(src = "https://cdn.dribbble.com/users/1433603/screenshots/14483703/media/d895bb77a9139023a7bcca44b22b6bc0.gif",
               timeout = 200, 
               position = 'full-page'), #retrieves GIF online while app is compiling
  
  
)

############### 3 - server ###############

server <- function(input, output, session){
  
  ############################## data manipulation #####################################################
  
  # credentials for movebank
  curl <- reactive({movebankLogin(username=input$username,
                                  password=input$password)})
  
  # download movebank data
  locs <-  eventReactive(input$go, {
    suppressWarnings(    ###NEW
      myDF<-getMovebankLocationData(study=as.character(isolate(input$studyID1)), sensorID="GPS", 
                                    login=curl(), 
                                    timestamp_start=Sys.time()-weeks(input$n.weeks),
                                    timestamp_end=Sys.time())  %>%
        mutate(timestamp=lubridate::round_date(timestamp, "5 mins")) %>%
        select(-tag.local.identifier))
    
    
    
    if(isTruthy(input$studyID2)){
      suppressWarnings(    ###NEW
        myDF<-getMovebankLocationData(study=as.character(isolate(input$studyID2)), sensorID="GPS", 
                                      login=curl(), 
                                      timestamp_start=Sys.time()-weeks(input$n.weeks),
                                      timestamp_end=Sys.time())  %>%
          mutate(timestamp=lubridate::round_date(timestamp, "5 mins")) %>%
          select(-tag.local.identifier)%>%
          bind_rows(myDF))
      
    }
    
    myDF<-myDF[!duplicated(paste0(myDF$timestamp,myDF$individual.local.identifier)),] ## this is to exclude duplicated timestamps (if present)
    locs.df<-as.data.frame(myDF) %>%
      dplyr::filter(!is.na(individual.local.identifier)) %>%
      mutate(bird_id=gsub(r"{\s*\([^\)]+\)}","",as.character(individual.local.identifier))) %>%
      dplyr::select(individual.local.identifier,bird_id,timestamp,location.lat,location.long) %>%
      mutate(bird_id=as.character(bird_id)) %>%
      rename(long_wgs=location.long,lat_wgs=location.lat) %>%
      mutate(x=long_wgs,y=lat_wgs) %>%
      filter(!is.na(x))
    return(locs.df) 
  })
  
  # create list of animals
  bird.list <-  eventReactive(input$go, {
    unique((locs() %>% dplyr::select(individual.local.identifier) %>% 
              mutate(bird_id=gsub(r"{\s*\([^\)]+\)}","",as.character(individual.local.identifier))) %>%
              arrange(bird_id) %>% #sort the bird_id list ascending
              mutate(bird_id=as.factor(bird_id))
    )$bird_id) #select the column bird_id to get some kind of vector list, not a dataframe
  })
  
  # create EPSG for the tracking data
  # AUTO-DETECT best EPSG CODE derived from average lat and long -------------------------
  ## taken from: https://stackoverflow.com/questions/58828828/auto-detect-coordinate-reference-system-based-on-coordinates-in-gpx-file
  
  EPSG <-  eventReactive(input$go, {
    tf.avg.lat <- mean(locs()$lat_wgs)
    tf.avg.lon <- mean(locs()$long_wgs)
    EPSG.num <- 32700-round((45+tf.avg.lat)/90,0)*100+round((183+tf.avg.lon)/6,0)
    return(EPSG.num)
  })
  
  # create spatial tracking data frame for mapping
  trackingdata <-  eventReactive(input$go, {
    locs() %>%
      st_as_sf(coords = c("x", "y"), crs=4326) %>%
      st_transform(EPSG()) %>%
      dplyr::mutate(long_eea = sf::st_coordinates(.)[,1],
                    lat_eea = sf::st_coordinates(.)[,2]) %>%
      select(bird_id,timestamp,long_wgs,lat_wgs,long_eea,lat_eea) %>%
      arrange(bird_id,timestamp) %>%
      dplyr::mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
                    date = as.Date(timestamp, tz = "UTC"),
                    date_id = paste0(date, "_", bird_id),
                    event_id=seq_along(timestamp),
                    year_day = lubridate::yday(timestamp)) %>%
      sf::st_transform(4326) %>%
      mutate(num_time = as.numeric(timestamp, origin=as.POSIXct("2015-01-01", tz="GMT"))) #as workaround for color legend
  })
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ALL OTHER DATA MANIPULATIONS TO RETURN POTENTIAL NEST LOCATION DATA FRAME-------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  avidat_nest <-  eventReactive(input$go, {
    # PREPARE DATA FOR NEST DETECTION WITH PLANAR COORDINATES -------------------------
    # Creating a track
    # 3 mins
    avidat_track_amt <- trackingdata() %>% sf::st_drop_geometry() %>%
      amt::mk_track(
        .x = long_eea,
        .y = lat_eea,
        .t = timestamp,
        id = bird_id,
        date_id,
        event_id,
        crs = EPSG()
      ) %>%
      amt::time_of_day(include.crepuscule = T) %>% # if F, crepuscule is considered as night
      amt::arrange(id, t_)
    
    # creating a night and a day data frame, include crepuscule in day
    avidat_track_night <- avidat_track_amt %>%
      dplyr::filter(tod_ %in% c("night","dusk","dawn")) %>%
      dplyr::select(-tod_)
    
    avidat_track_day <- avidat_track_amt %>%
      dplyr::filter(tod_ %in% c("day")) %>%
      dplyr::select(-tod_)
    
    
    ### identify individuals for which there are no night locations and break the data prep
    if(length(unique(avidat_track_day$id[which(!(avidat_track_day$id %in% avidat_track_night$id))]))>0){
      mismatches <- unique(avidat_track_day$id[which(!(avidat_track_day$id %in% avidat_track_night$id))])
      avidat_track_night <- avidat_track_night %>% dplyr::filter(!(id %in% mismatches))
      avidat_track_day <- avidat_track_day %>% dplyr::filter(!(id %in% mismatches))
      avidat_track_amt <- avidat_track_amt %>% dplyr::filter(!(id %in% mismatches))
      avidat <- trackingdata() %>% dplyr::filter(!(bird_id %in% mismatches))
    }
    
    
    # RECURSIONS DURING NIGHTTIME --------------------------------------------------
    # splitting track into a list with each single id grouped to an element
    avidat_track_night <- as.data.frame(avidat_track_night)
    avidat_track_night$id <- factor(avidat_track_night$id, levels=unique(avidat_track_night$id))  ## required to prevent re-ordering in split
    avidat_track_night_list <- split(avidat_track_night, avidat_track_night$id)
    
    # calculating recursions
    avidat_night_recurse <- lapply(avidat_track_night_list, function(x)
      recurse::getRecursions(x = x[1:4], radius = input$nestradius, timeunits = "hours"))
    
    # allocating recurse information to track data frame (1.5 mins)
    avidat_track_night$revisits <- NA
    avidat_track_night$residence_time <- NA
    for (i in 1:length(avidat_night_recurse)) {
      avidat_track_night[avidat_track_night$id == unique(avidat_track_night$id)[i] ,]$revisits <-
        avidat_night_recurse[[i]]$revisits
      avidat_track_night[avidat_track_night$id == unique(avidat_track_night$id)[i] ,]$residence_time <-
        avidat_night_recurse[[i]]$residenceTime
    }
    
    
    # RECURSIONS DURING DAYTIME ----------------------------------------------------
    # splitting track into a list with each single id grouped to an element
    avidat_track_day <- as.data.frame(avidat_track_day)
    avidat_track_day$id <- factor(avidat_track_day$id, levels=unique(avidat_track_day$id))  ## required to prevent re-ordering in split
    avidat_track_day_list <- split(avidat_track_day, avidat_track_day$id)
    
    # calculating recursions (1.5 mins)
    avidat_day_recurse <- lapply(avidat_track_day_list, function(x)
      recurse::getRecursions(x = x[1:4], radius = input$nestradius, timeunits = "hours"))
    
    # allocating recurse information to track data frame (4 mins)
    avidat_track_day$revisits <- NA
    avidat_track_day$residence_time <- NA
    for (i in 1:length(avidat_day_recurse)) {
      avidat_track_day[avidat_track_day$id == unique(avidat_track_day$id)[i] ,]$revisits <-
        avidat_day_recurse[[i]]$revisits
      avidat_track_day[avidat_track_day$id == unique(avidat_track_day$id)[i] ,]$residence_time <-
        avidat_day_recurse[[i]]$residenceTime
    }
    
    # creating avidat data set for night locations only
    avidat_night <- trackingdata() %>%
      dplyr::filter(event_id %in% avidat_track_night$event_id) %>%
      dplyr::left_join(avidat_track_night[,6:8], by = "event_id")
    
    # creating avidat data set for night day locations only
    avidat_day <- trackingdata() %>%
      dplyr::filter(event_id %in% avidat_track_day$event_id) %>%
      dplyr::left_join(avidat_track_day[,6:8], by = "event_id")
    
    # filtering the location with longest residence time during nighttime
    suppressWarnings({
      avidat_night_max <- avidat_night %>%
        dplyr::group_by(bird_id) %>%
        dplyr::summarise(residence_time_night = max(residence_time),
                         revisits_night = first(revisits[which(residence_time == max(residence_time))]),
                         date_night = first(date[which(residence_time == max(residence_time))]),
                         long_night = first(long_eea[which(residence_time == max(residence_time))]),
                         lat_night = first(lat_eea[which(residence_time == max(residence_time))])
        ) %>%
        dplyr::group_by(bird_id) %>%   ## because there are sometimes duplicates, we need to group again and take the one with max revisits for those where time is equal
        dplyr::summarise(revisits_night = max(revisits_night),
                         residence_time_night = first(residence_time_night[which(revisits_night == max(revisits_night))]),
                         date_night = first(date_night[which(revisits_night == max(revisits_night))]),
                         long_night = first(long_night[which(revisits_night == max(revisits_night))]),
                         lat_night = first(lat_night[which(revisits_night == max(revisits_night))])
        )
    })
    
    # filtering the location with longest residence time during daytime
    suppressWarnings({
      avidat_day_max <- avidat_day %>%
        dplyr::group_by(bird_id) %>%
        dplyr::summarise(residence_time_day = max(residence_time),
                         revisits_day = first(revisits[which(residence_time == max(residence_time))]),
                         date_day = first(date[which(residence_time == max(residence_time))]),
                         long_day = first(long_eea[which(residence_time == max(residence_time))]),
                         lat_day = first(lat_eea[which(residence_time == max(residence_time))])
        ) %>%
        dplyr::group_by(bird_id) %>%
        dplyr::summarise(revisits_day = max(revisits_day),
                         residence_time_day = first(residence_time_day[which(revisits_day == max(revisits_day))]),
                         date_day = first(date_day[which(revisits_day == max(revisits_day))]),
                         long_day = first(long_day[which(revisits_day == max(revisits_day))]),
                         lat_day = first(lat_day[which(revisits_day == max(revisits_day))])
        )
    })
    
    # creating sf objects for distance calculation
    avidat_night_max_sf <- avidat_night_max %>%
      sf::st_as_sf(coords = c("long_night", "lat_night"), crs = EPSG())
    avidat_day_max_sf <- avidat_day_max %>%
      sf::st_as_sf(coords = c("long_day", "lat_day"), crs = EPSG())
    
    # calculating the distance from the day location to the night location
    avidat_max_res_time <- avidat_day_max_sf %>%
      dplyr::mutate(dist_day_to_night = as.numeric(sf::st_distance(avidat_day_max_sf, avidat_night_max_sf, by_element = T))) %>%
      sf::st_drop_geometry() %>%
      dplyr::left_join(avidat_night_max_sf %>% dplyr::select(bird_id,revisits_night,residence_time_night,date_night), by = "bird_id") %>%
      sf::st_drop_geometry()
    
    ##### CALCULATING REVISITS TO POTENTIAL NEST SITE
    avidat_track <- as.data.frame(avidat_track_amt %>% dplyr::select(-tod_))
    avidat_track$id <- factor(avidat_track$id, levels=unique(avidat_track$id))  ## required to prevent re-ordering in split
    avidat_track_list <- split(avidat_track, avidat_track$id)
    avidat_track_amt$id <- factor(avidat_track_amt$id, levels=unique(avidat_track_amt$id))  ## required to prevent re-ordering in split
    avidat_track_amt_list <- split(avidat_track_amt, avidat_track_amt$id)
    
    # calculating recursions (1.5 mins)
    avidat_recurse <- lapply(avidat_track_list, function(x)
      recurse::getRecursions(x = x[1:4], radius = input$nestradius, timeunits = "hours"))
    
    # allocating recurse information to track data frame (4 mins)
    avidat_track$revisits <- NA
    avidat_track$residence_time <- NA
    for (i in 1:length(avidat_recurse)) {
      avidat_track[avidat_track$id == unique(avidat_track$id)[i] ,]$revisits <-
        avidat_recurse[[i]]$revisits
      avidat_track[avidat_track$id == unique(avidat_track$id)[i] ,]$residence_time <-
        avidat_recurse[[i]]$residenceTime
    }
    
    
    ########## IDENTIFY PLAUSIBLE NESTS BY SEQUENTIAL FILTERING AND COUNTING LOCS AROUND EACH POT NEST ------
    
    ## LOOP OVER EACH INDIVIDUAL YEAR
    avidat_pot_nests<-data.frame()
    for (i in unique(avidat_track$id)) {
      
      ### subset the data ####
      workdat<- avidat_track %>% 
        dplyr::rename(bird_id=id) %>%
        dplyr::filter(bird_id==i)
      dim(workdat)
      
      ### calculate nearest neighbour distances
      # identify nearest neighbours and calculate the mean distance to fixed number of nearest neighbours
      nearest <- RANN::nn2(workdat[,1:2],workdat[,1:2],k=min(dim(workdat)[1],25))$nn.dists
      workdat$NN50dist<-apply(nearest,1,median)
      
      ### apply sequential filters of residence time and revisits and nearest neighbour distance
      potnests <- workdat %>% dplyr::arrange(desc(residence_time)) %>% 
        dplyr::filter(residence_time > quantile(residence_time, 0.99)) %>%
        ## need a second filter step here to eliminate locations if they differ too much in time
        dplyr::arrange(desc(revisits)) %>% 
        dplyr::filter(revisits >= quantile(revisits, 0.75)) %>%
        dplyr::arrange(NN50dist) %>% 
        dplyr::filter(NN50dist <= quantile(NN50dist, 0.25))
      dim(potnests)
      
      ### build in failsafe for when number is very low
      if(dim(potnests)[1]==0){
        potnests <- workdat %>% dplyr::arrange(desc(residence_time)) %>%
          dplyr::slice_max(n=1, order_by=residence_time)
      }
      
      ### compare point counts for 3 alternatives
      radnests2 <- RANN::nn2(workdat[,1:2],
                             potnests[,1:2],
                             k=dim(workdat)[1],
                             searchtype="radius", radius = input$nestradius)$nn.idx
      ## count the number of points
      idmat<-apply(radnests2,1,unique)
      if(is.list(idmat)){
        potnests$npoint<-lengths(apply(radnests2,1,unique))
      }else{
        for(l in 1:dim(idmat)[2]){
          potnests$npoint[l]<-length(idmat[,l]>0)
        }
      }
      
      ### retain only the nest with the highest point count
      ## in case of ties use most time, visits, distance and then earliest point
      avidat_pot_nests<-potnests %>% 
        dplyr::slice_max(order_by=npoint, n=1) %>%
        dplyr::slice_max(order_by=residence_time, n=1) %>%
        dplyr::slice_max(order_by=revisits, n=1) %>%
        dplyr::slice_min(order_by=NN50dist, n=1) %>%
        dplyr::slice_min(order_by=t_, n=1) %>%
        dplyr::select(bird_id,event_id,x_,y_,npoint,revisits,residence_time,NN50dist) %>%
        dplyr::rename(id=bird_id,x=x_,y=y_) %>%
        dplyr::bind_rows(avidat_pot_nests)
    }
    
    # RETAIN Predicted nest locations
    display_nest <- avidat_pot_nests %>%
      dplyr::rename(bird_id=id) %>%
      sf::st_as_sf(coords = c("x", "y"), crs = EPSG()) %>%
      sf::st_transform(crs = 4326) %>%
      mutate(long = sf::st_coordinates(.)[,1],
             lat = sf::st_coordinates(.)[,2])
    
    return(display_nest)
  })
  
  
  
  
  
  ############################## UI to display data #####################################################
  
  ### inserts a new tab (next to the "Login" tab) to display the data
  observeEvent(input$go, {
    
    appendTab(inputId = "my_tabsetPanel",
              #theme = shinytheme("slate"), # or darkly
              
              ### Individual bird ###
              tabPanel("Individual bird",
                       
                       sidebarLayout(
                         sidebarPanel(
                           width = 3,
                           
                           selectInput(inputId = "ID.e", label = "Select the Individual", 
                                       choices = bird.list(), multiple = F),
                           column(6, actionButton("prevBtn.e", "<<"), align = "right"),
                           column(6, actionButton("nextBtn.e", ">>"), align = "left"),#style='padding:4px; font-size:80%')
                           
                           br(), hr(),
                           
                           radioButtons(inputId = "PointsToDisplay.e",
                                        label = "Data",
                                        choices = c("last 5 points" = 1,
                                                    "last 10 points" = 2,
                                                    "last 2 days" = 3,
                                                    "last 5 days" = 4,
                                                    "last 10 days" = 5,
                                                    "all data" = 6),
                                        selected = 1),
                           
                           # Download Button
                           downloadButton(outputId = "downloadData",
                                          label = "Download GPKG file",
                                          icon = shiny::icon("download")),
                           
                           br(), br(),   #some empty rows to align sidebarPanel with mainPanel
                         ),
                         
                         mainPanel(
                           height = 1000,  ## does not seem to make a difference
                           
                           # # Display last 5 points
                           # #tableOutput("five.points.e"),
                           # 
                           # hr(),    ###NEW
                           
                           # Plot points on map
                           fillPage(
                             tags$style(type = "text/css", "#map {height: calc(100vh - 8px) !important;}"),
                             uiOutput("map_or_text", height = "100vh"))     ### trying to adjust height of map
                           
                         )
                       )
              )
    )
  })
  
  ### removes the "Login" tab after login is completed
  observeEvent(input$go, {
    removeTab(inputId = "my_tabsetPanel",
              target = "Login")
  }) #this is necessary because for some reason the "Data" tab starts to run the download only after it is activated
  #   (by clicking on that tab). This way, the "Login" tab disappears, so that the "Data" tab is automatically selected
  #   and the download starts. Maybe there is a way to make the tab header transparent or disappear for making the
  #   app prettier.
  
  
  ############################## select individual to display #####################################################
  
  nestPerID.e <- reactive({avidat_nest()[avidat_nest()$bird_id == input$ID.e,] })
  dataPerID.e <- reactive({trackingdata()[trackingdata()$bird_id == input$ID.e,] })
  
  # site updates when clicking on Previous / Next Red Kite
  observeEvent(input$prevBtn.e, {
    listPlacement.e <- which(bird.list() == input$ID.e)
    if (listPlacement.e > 1) { 
      newSelection <- bird.list()[listPlacement.e-1]
      updateSelectInput(session, inputId = "ID.e", selected = newSelection)
    }
  })  
  observeEvent(input$nextBtn.e, {
    listPlacement.e <- which(bird.list() == input$ID.e)
    if (listPlacement.e < length(bird.list())) { 
      newSelection <- bird.list()[listPlacement.e+1]
      updateSelectInput(session, inputId = "ID.e", selected = newSelection)
    }
  })  
  
  # determining subset based on Data to Display 
  dataInd.e <- reactive({
    if(input$PointsToDisplay.e == 1) {utils::tail(dataPerID.e(), n=5)} #last 5 points
    else if(input$PointsToDisplay.e == 2) {utils::tail(dataPerID.e(), n=10)} #last 10 points
    else if(input$PointsToDisplay.e == 3) {subset(dataPerID.e(), timestamp >= as.POSIXct(Sys.Date()-1))} #last 2 days
    else if(input$PointsToDisplay.e == 4) {subset(dataPerID.e(), timestamp >= as.POSIXct(Sys.Date()-4))} #last 5 days
    else if(input$PointsToDisplay.e == 5) {subset(dataPerID.e(), timestamp >= as.POSIXct(Sys.Date()-9))} #last 10 days
    else if(input$PointsToDisplay.e == 6) {dataPerID.e()} #all data
  })
  
  # Plot GPS points on map
  output$map_or_text <- renderUI({    ###NEW
    if(nrow(dataInd.e())>0) {    ###NEW
      renderLeaflet({
        
        ### make colour palette for Date
        pal.date <- colorNumeric(palette = viridis::viridis(200), domain = NULL, reverse=T)
        
        ### legend for Date coloration
        myLabelFormat = function(...,dates=FALSE){ 
          if(dates){ 
            function(type = "numeric", cuts){
              as <- as.POSIXct(cuts, origin="1970-01-01", tz="GMT")
              format(as,"%y-%m-%d %H:%M")
            } 
          }else{
            labelFormat(...)
          }
        }
        
        l1.e <- leaflet(options = leafletOptions(zoomControl = FALSE) #zoom Snap controls padding of points to map border, but then
                        #zoom symbols (+,-) don't work
        ) %>% #changes position of zoom symbol
          htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topright' }).addTo(this)}"
          ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
          addProviderTiles("Esri.WorldImagery", group = "Satellite",
                           options = providerTileOptions(opacity = 0.6, attribution = F)) %>%
          addProviderTiles("CartoDB.Voyager", group = "Roadmap", options = providerTileOptions(attribution = F)) %>%  
          addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>%
          setView(lng=nestPerID.e()$long, lat=nestPerID.e()$lat, zoom=12) %>%
          addCircleMarkers(
            data=dataInd.e(), lng=dataInd.e()$long_wgs, lat=dataInd.e()$lat_wgs,
            radius = 5,
            stroke = TRUE, color = "black", weight = 0.5,
            fillColor = ~pal.date(num_time), fillOpacity = 0.5,
            popup = ~ paste0("bird ID: ", bird_id, "<br>", timestamp)
          ) %>% 
          addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F)) %>%
          
          addCircleMarkers(
            data=nestPerID.e(), lng=nestPerID.e()$long, lat=nestPerID.e()$lat,
            radius = 10,
            stroke = TRUE, color = "red", weight = 0.5,
            fillColor = "red", fillOpacity = 0.7,
            popup = ~ paste0("bird ID: ", bird_id, "<br>", "N visits: ", revisits, "<br>", "Duration (hrs): ", residence_time)
          ) %>% 
          
          addLegend(     # legend for date (viridis scale)
            data = dataInd.e(),
            position = "topleft", 
            pal = pal.date,
            values = ~num_time,
            opacity = 1,
            bins = 4,
            labFormat = myLabelFormat(dates=T),
            title = NULL
          )
      })
    } else HTML(as.character(div(style="color: red;", paste0("No points available in this time frame."))))    ###NEW
  })    ### close conditional loop if no points are available
  
  # Downloadable gpkg of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$studyID1, Sys.Date(),".gpkg", sep = "")
    },
    content = function(file) {
      st_write(avidat_nest(),file)
      
    }
  )
  
}


############### 4 - start ShinyApp ##############

shinyApp(ui = ui, server = server)

