
source("utilities/utils.R")

mapUi <- function(id, label = "ports_location"){
  ns <- NS(id)
  segment(
    leafletOutput(ns("ports_map"),
                  height = "505px")
  )
}


mapServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      # Render the initial leaflet map
      output$ports_map <- renderLeaflet({
        leaflet() %>%
          addTiles()
      })
      
      observe({
        # Setting up the pop up labels for the leaflet map
        labs <- lapply(seq(nrow(data())), function(i) {
          if(data()[i, "data_point"] == "start" && !is.na(data()[i, "distance"]) && data()[i, "distance"] != 0){
            paste0(
              tags$p(
                tags$span(
                  tags$b(tags$u("FROM:")), tags$br(),
                  "Vessel name: ", data()[i, "ship_name"], tags$br(),
                  "Longitude: ", data()[i, "lon"], tags$br(),
                  "Latitude: ", data()[i, "lat"], tags$br()
                )
              )
            )
          } else if(data()[i, "data_point"] == "end"){
            paste0(
              tags$p(
                tags$span(
                  tags$b(tags$u("TO:")), tags$br(),
                  "Vessel name: ", data()[i, "ship_name"], tags$br(),
                  "Longitude: ", data()[i, "lon"], tags$br(),
                  "Latitude: ", data()[i, "lat"], tags$br(),
                  "Distance: ", data()[i + 1, "distance"],"m"
                )
              )
            )
          } else {
            paste0(
              tags$p(
                tags$span(
                  tags$b(tags$u("PARKED")), tags$br(),
                  "Vessel name: ", data()[i, "ship_name"], tags$br(),
                  "Longitude: ", data()[i, "lon"], tags$br(),
                  "Latitude: ", data()[i, "lat"], tags$br(),
                )
              )
            )
          }
        })

      #Rendering the leaflet map
      leafletProxy("ports_map", data = data()) %>%
        clearMarkers() %>%
        clearAntpath() %>% 
        removeControl("legend") %>% 
        clearControls() %>% 
        clearPopups() %>%
        addMarkers(
          lng = ~lon, lat = ~lat, icon = leaflet_icons(data()),
          popup = lapply(labs, htmltools::HTML)
        ) %>%
        addAntpath(
          lng = ~lon, lat = ~lat, color = ~pal(ship_type), weight = 2,
          options = antpathOptions(
            reverse = TRUE
          )
          ) %>% 
        fitBounds(lng1 = max(data()$lon), lat1 = max(data()$lat),
                  lng2 = min(data()$lon), lat2 = min(data()$lat)
        ) %>%
        addLegend( position = "bottomright",
                   pal = pal,
                   values = ~ship_type)
      })
    }
  )
}