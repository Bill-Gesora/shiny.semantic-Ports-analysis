library(data.table)
library(tidyverse)
library(geosphere)
library(leaflet)
library(leaflet.extras2)
library(shiny)
library(shiny.semantic)
library(shiny.router)

source("utilities/utils.R")
source("utilities/analytics_logo.R")
source("module_filters.R")
source("module_map.R")

main_layout <- grid_template(
  default = list(
    areas = rbind(
      c("dashboard_header"),
      c("dashboard_panel")
    ),
    rows_height = c("70px", "auto"),
    cols_width = c("100%")
  ),
  mobile = list(
    areas = rbind(
      c("dashboard_header"),
      c("dashboard_panel")
    ),
    rows_height = c("600px", "auto"),
    cols_width = c("100%")
  )
)

dashboard_header_layout <- grid_template(
  default = list(
    areas = rbind(
      c("logo_area", "dashboard_filters")
    ),
    rows_height = c("100%"),
    cols_width = c("20%", "80%")
  ),
  mobile = list(
    areas = rbind(
      c("logo_area"),
      c("dashboard_filters")
    ),
    rows_height = c("100px", "400px"),
    cols_width = c("100%")
  )
)

dashboard_panel_layout <- grid_template(
  default = list(
    areas = rbind(
      c("dashboard_kpi", "dashboard_kpi"),
      c("dashboard_ship_panel", "dashboard_leaflet_map")
    ),
    rows_height = c("140px", "530px"),
    cols_width = c("21%", "79%")
  ),
  mobile = list(
    areas = rbind(
      c("dashboard_kpi"),
      c("dashboard_ship_panel"),
      c("dashboard_leaflet_map")
    ),
    rows_height = c("400px", "100px", "100px"),
    cols_width = c("100%")
  )
)

dashboard_page <- grid(main_layout,
  dashboard_header = grid(
    dashboard_header_layout,
    logo_area = div(
      analytics_logo
    ),
    dashboard_filters = filtersUi("map_filters")
  ),
  dashboard_panel = grid(
    dashboard_panel_layout,
    dashboard_kpi = div(
      class = "ui four column stackable grid",
      div(
        class = "column",
        uiOutput("num_observations")
      ),
      div(
        class = "column",
        uiOutput("total_distance_travelled")
      ),
      div(
        class = "column",
        uiOutput("last_observed_date")
      ),
      div(
        class = "column",
        uiOutput("last_observed_port")
      )
    ),
    dashboard_ship_panel = div(
      uiOutput("ship_details_pane")
    ),
    dashboard_leaflet_map = mapUi("port_map_leaflet"),
  )
)


router <- make_router(
  route("index", dashboard_page)
)

ui <- semanticPage(
  title = "Port Analytics",
  tags$head(
    tags$link(rel = "stylesheet", href = "css/style.css", type = "text/css")
  ),
  router$ui,
  tags$footer("Created by Bill for Appsilon",
    align = "center",
    style = "position:fixed;
              bottom:0; right:0; left:0;
              background:teal;
              color: white;
              padding:5px;
              box-sizing:border-box;
              z-index: 1000;
              text-align: center"
  )
)

server <- function(input, output, session) {
  # Router page
  router$server(input, output, session)
  # Assign the module call a variable
  filters_module <- filtersServer("map_filters", reactive(analyzed_data))

  mapServer("port_map_leaflet", reactive(filters_module$data))

  # Observations per ship
  output$num_observations <- renderUI({
    selected_ship <- filters_module$data$ship_id

    observations_value <- observations %>%
      filter(ship_id %in% selected_ship) %>%
      select(count) %>%
      sum()

    custom_kpi_box(
      header = "Total observations",
      value = format(observations_value,
        format = "d",
        big.mark = ","
      ),
      label = "",
      icon = icon("big binoculars"),
      description = "Number of data points collected"
    )
  })

  # Total distance traveled
  output$total_distance_travelled <- renderUI({
    selected_ship <- filters_module$data$ship_id

    distance_travelled <- distance_travelled %>%
      filter(ship_id %in% selected_ship) %>%
      select(distance) %>%
      sum(na.rm = TRUE)

    custom_kpi_box(
      header = "Distance travelled",
      value = paste0(format(round(distance_travelled, 2),
        format = "f",
        big.mark = ",",
        nsmall = 0
      )),
      label = "",
      icon = icon("big map"),
      description = "Total distance travelled in meters"
    )
  })

  # Last observerd date
  output$last_observed_date <- renderUI({
    selected_ship <- filters_module$data$ship_id

    last_date <- final_observations %>%
      ungroup() %>%
      filter(ship_id %in% selected_ship) %>%
      select(observation_date)

    div(
      class = "ui raised link card",
      style = "cursor:default",
      div(
        class = "right aligned content",
        div(class = "center aligned header", "Last observed on..."),
        div(
          class = "left aligned",
          icon("big calendar alternate"),
          div(
            class = "ui right aligned tiny horizontal statistic",
            div(class = "value", last_date),
            div(class = "label", "")
          )
        ), hr(),
        div(class = "right floated meta", "Last observation date")
      )
    )
  })


  # Last location
  output$last_observed_port <- renderUI({
    selected_ship <- filters_module$data$ship_id

    port_assigned <- final_observations %>%
      ungroup() %>%
      filter(ship_id %in% selected_ship) %>%
      select(port_assigned)

    div(
      class = "ui raised link card",
      style = "cursor:default",
      div(
        class = "right aligned content",
        div(class = "center aligned header", "Last seen at.."),
        div(
          class = "left aligned",
          icon("big map marker alternate"),
          div(
            class = "ui right aligned tiny horizontal statistic",
            div(class = "value", port_assigned),
            div(class = "label", "")
          )
        ), hr(),
        div(class = "right floated meta", "Last observation port")
      )
    )
  })

  output$ship_details_pane <- renderUI({
    ship_details(filters_module$data)
  })
}

shinyApp(ui, server)
