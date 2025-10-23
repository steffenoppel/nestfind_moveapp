library("shiny")
library("move2")
library("sf")
library("dplyr")
library("lubridate")
library("recurse")
library("RANN")
library("leaflet")
library("viridis")

# MoveApps Shiny Module for Nest Location Prediction
# Adapted from NestFindApp by Lara Gross and Steffen Oppel

# helper functions
get_unique_track_ids <- function(move_data) {
  # This function lets us load the track ids in the data set, without crashing Shiny
  return(mt_track_data(move_data)[[mt_track_id_column(move_data)]])
}


get_recursions <- function(move_by_ind, nest_radius) {
  if (nrow(move_by_ind) > 10000) {
    # TODO: confirm this is an appropriate amount to filter, to every 12 hours
    move_by_ind <- mt_filter_per_interval(move_by_ind, unit = "12 hours")
  }
  
  recursions <- recurse::getRecursions(move_by_ind,
                                       radius = nest_radius,
                                       timeunits = "hours")

  updated_move_by_ind <- move_by_ind |>
    mutate(
      revisits = recursions$revisits,
      residence_time = recursions$residenceTime
    )

  return(updated_move_by_ind)
}

get_best_nests_for_ind <- function(move_by_ind, nest_radius) {
  
  # This is the main function that detects nest locations
  # move_by_ind is the move2 object filtered to the current individual
  # this is because we call the function in an lapply, but all individuals data 
  # gets evaluated on load of the app, prior to the user selecting which track 
  # they are interested in
  
  # TODO: confirm the algorithm looks right and is working correctly
  

  coords <- move_by_ind |>
    sf::st_coordinates()

  # Calculate nearest neighbor distances
  nearest <- RANN::nn2(coords, coords, k = min(nrow(move_by_ind), 25))$nn.dists

  move_by_ind$NN50dist <- apply(nearest, 1, median)

  # Apply sequential filters
  plausible_nests <- move_by_ind |>
    arrange(desc(residence_time)) |>
    filter(residence_time > quantile(residence_time, 0.99)) |>
    arrange(desc(revisits)) |>
    filter(revisits >= quantile(revisits, 0.75)) |>
    arrange(NN50dist) |>
    filter(NN50dist <= quantile(NN50dist, 0.25))

  if (nrow(plausible_nests) == 0) {
    plausible_nests <- move_by_ind |>
      arrange(desc(residence_time)) |>
      slice_max(n = 1, order_by = residence_time)
  }

  # Count points within nest radius
  radnests2 <- RANN::nn2(coords,
    coords,
    k = nrow(move_by_ind),
    searchtype = "radius",
    radius = nest_radius
  )$nn.idx

  idmat <- apply(radnests2, 1, unique)
  if (is.list(idmat)) {
    plausible_nests$npoint <- lengths(idmat)
  } else {
    for (l in 1:ncol(idmat)) {
      plausible_nests$npoint[l] <- sum(idmat[, l] > 0)
    }
  }

  # Retain best nest location
  # Note: this is still a move2 object
  best_nests <- plausible_nests |>
    slice_max(order_by = npoint, n = 1) |>
    slice_max(order_by = residence_time, n = 1) |>
    slice_max(order_by = revisits, n = 1) |>
    slice_min(order_by = NN50dist, n = 1) |>
    slice_min(order_by = timestamp, n = 1)

  return(best_nests)
}





shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)

  tagList(
    titlePanel("Nest Location Predictor"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        numericInput(ns("nest_radius"),
          "Error radius (m) around likely nest site",
          value = 50, min = 10, max = 500
        ),
        hr(),
        selectInput(ns("track_id"),
          "Select Track ID",
          choices = NULL,
          multiple = FALSE
        ),
        hr(),
        radioButtons(ns("points_to_display"),
          "Data to display",
          choices = c(
            "Last 5 points" = 1,
            "Last 10 points" = 2,
            "Last 2 days" = 3,
            "Last 5 days" = 4,
            "Last 10 days" = 5,
            "All data" = 6
          ),
          selected = 6
        ),
        downloadButton(ns("downloadData"),
          "Nest predictions",
          icon = icon("download")
        )
      ),
      mainPanel(
        fillPage(
          tags$style(
            type = "text/css",
            paste0("#", ns("map"), " {height: calc(100vh - 8px) !important;}")
          ),
          uiOutput(ns("map_or_text"), height = "100vh")
        )
      )
    )
  )
}


shinyModule <- function(input, output, session, data) {
  
  # Calculate nest predictions
  nest_predictions <- reactive({

    move_by_ind_list <- data |>
      split(mt_track_id(data))

    # Run nest prediction for all individuals
    best_nests_list <- lapply(move_by_ind_list, function(move_by_ind) {
      if (nrow(move_by_ind) < 5) next

      move_by_ind_recursed <- get_recursions(move_by_ind, input$nest_radius)
      best_nests <- get_best_nests_for_ind(move_by_ind_recursed, input$nest_radius)
      return(best_nests)
    })

    # Still a move2 object
    all_nests <- do.call(rbind, best_nests_list)

    return(all_nests)
  })

  # Update individual selection
  observe({
    req(data)
    track_id_list <- get_unique_track_ids(data)
    updateSelectInput(session, "track_id", choices = track_id_list, selected = track_id_list[1])
  })


  # Get data for selected individual
  dataPerTrackID <- reactive({
    req(data, input$track_id)
    
    track_id_column <- mt_track_id_column(data)
    ind_move <- data |>
      filter_track_data(
        .track_id = input$track_id
      ) |>
      arrange(desc(timestamp)) |>
      mutate(
        popup_label = paste0("Track ID: ", !!sym(track_id_column), "<br>", timestamp),
        fill_color = as.numeric(timestamp)
      )
    
    filtered_data <-switch(
      input$points_to_display,
      "1" = slice_tail(ind_move, n = 5),
      "2" = slice_tail(ind_move, n = 10),
      "3" = filter(ind_move, timestamp >= Sys.time() - days(2)),
      "4" = filter(ind_move, timestamp >= Sys.time() - days(5)),
      "5" = filter(ind_move, timestamp >= Sys.time() - days(10)),
      "6" = ind_move,
      stop("Invalid choice. Must be 1-6.")
    )
    return(filtered_data)
  })

  nestPerTrackID <- reactive({
    req(nest_predictions(), input$track_id)
    nest_predictions() |>
      filter(!!sym(mt_track_id_column(data)) == input$track_id) |>
      mutate(
        popup_label = paste0(
          "Track ID: ",
          !!sym(mt_track_id_column(data)),
          "<br>N visits: ",
          revisits,
          "<br>Duration (hrs): ", round(residence_time, 1)
        )
      )
  })


  # Render map
  output$map_or_text <- renderUI({
    req(nestPerTrackID(), dataPerTrackID())

    if (nrow(dataPerTrackID()) > 0 && nrow(nestPerTrackID()) > 0) {
      renderLeaflet({
        
        pal.date <- colorNumeric(palette = viridis(200),
                                 domain = NULL,
                                 reverse = TRUE)
        date_label_fn <- function(..., dates = FALSE) {
          if(dates) {
            function(type = "numeric", cuts) {
              as <- as.POSIXct(cuts, origin = "1970-01-01", tz = "GMT")
              format(as, "%y-%m-%d %H:%M")
            }
          } else {
            labelFormat(...)
          }
        }
        
        ind_data <- dataPerTrackID()
        pal <- colorNumeric(palette = "viridis", domain = ind_data$fill_color)

        leaflet(options = leafletOptions(zoomControl = FALSE)) |>
          addProviderTiles("Esri.WorldImagery",
            group = "Satellite",
            options = providerTileOptions(opacity = 0.6, attribution = FALSE)
          ) |>
          addProviderTiles("CartoDB.Voyager",
            group = "Roadmap",
            options = providerTileOptions(attribution = FALSE)
          ) |>
          addLayersControl(baseGroups = c("Satellite", "Roadmap")) |>
          addCircleMarkers(
            data = ind_data,
            radius = 5,
            stroke = TRUE,
            weight = 0.5,
            color = "black",
            fillColor = ~ pal(fill_color),
            fillOpacity = 0.5,
            popup = ~popup_label
          ) |>
          addCircleMarkers(
            data = nestPerTrackID(),
            radius = 10,
            stroke = TRUE,
            color = "red",
            weight = 0.5,
            fillColor = "red",
            fillOpacity = 0.7,
            popup = ~popup_label
          ) |>
          addScaleBar(
            position = "bottomright",
            options = scaleBarOptions(imperial = FALSE)
          ) |>
          addLegend(
            data = ind_data,
            position = "topleft",
            pal = pal.date,
            values = ~fill_color,
            opacity = 1,
            bins = 4,
            labFormat = date_label_fn(dates = TRUE),
            title = NULL
          )
      })
    } else {
      HTML(as.character(div(
        style = "color: red;",
        "No points or nest predictions available for this individual."
      )))
    }
  })

  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("nest_predictions_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      st_write(nest_predictions(), file)
    }
  )

  # Return original data
  # TODO: you will need to update this if you want to be able to pass the nest predictions onto the next APP
  return(reactive({
    data
  }))
}