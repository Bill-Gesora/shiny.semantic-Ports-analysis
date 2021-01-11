# Setting the general options for R ----
# Remove scientific notations
options(scipen = 999)

# When data is null skip filter
conditional_filter <- function(condition, success) {
  if (condition) success else TRUE
}


# Functions used in leafltet ----
# Color palltet for different ships
pal <- colorFactor(
  palette = c("#FFC000", "#FF0000", "#00B8DE",
              "#F868EE", "#617CA9", "#003579" ,
              "#B11366", "#017E5C", "#71C100"),
  levels = c("Unspecified", "Cargo", "Tanker",
             "Tug", "Fishing", "Passenger",
             "Pleasure", "Navigation", "High Special")
)

# Icons used for the leaflet ship types
leaflet_icons <- function(data) {
  icons(
    iconUrl = case_when(
      data$ship_type == "Cargo" ~ "images/Cargo.svg",
      data$ship_type == "Tanker" ~ "images/Tanker.svg",
      data$ship_type == "Unspecified" ~ "images/Unspecified.svg",
      data$ship_type == "Tug" ~ "images/Tug.svg",
      data$ship_type == "Fishing" ~ "images/Fishing.svg",
      data$ship_type == "Passenger" ~ "/images/Passenger.svg",
      data$ship_type == "Pleasure" ~ "images/Pleasure.svg",
      data$ship_type == "Navigation" ~ "images/Navigation.svg",
      data$ship_type == "High Special" ~ "images/High Special.svg"
      ),
    iconWidth = 30,
    iconHeight = 30
    )
}

# Ship card functions
ship_details <- function(data) {
  div(class = "ui raised card",
      div(class = "image",
          img(id = "ship_image",
              src = unique(data$image_url),
          )
      ),
      div(class = "content",
          div(class = "header",
              unique(data$ship_name)),
          div(class = "meta",
              tags$span(
                tags$i(class = paste0(unique(data$flag)," flag")),
                unique(data$country)
              )
          ),
          hr(style = "height:3px; background-color:grey;"),
          div(class = "ui equal width grid",
              div(class = "column", "Ship ID:"),
              div(class = "column", unique(data$ship_id))
          ),hr(),
          div(class = "ui equal width grid",
              div(class = "column", "Ship type:"),
              div(class = "column", unique(data$ship_type))
          ),hr(),
          div(class = "ui equal width grid",
              div(class = "column", "Dead weight:"),
              div(class = "column", mean(data$dead_weight))
          ),hr(),
          div(class = "ui equal width grid",
              div(class = "column", "Ship length:"),
              div(class = "column", unique(data$ship_length))
          ),hr(),
          div(class = "ui equal width grid",
              div(class = "column", "Ship width:"),
              div(class = "column", unique(data$ship_width))
          )
      )
  )
}

# KPIs
custom_kpi_box <- function(value, label, header, description, icon) {
 div(class = "ui raised link card",
     style = "cursor:default",
   div(class = "right aligned content",
       div(class = "center aligned header", header),
       div( class = "left aligned",
         icon,
         div(class = "ui right aligned small horizontal statistic",
             div(class = "value", value),
             div(class = "label", label)
             )
       ), hr(),
       div(class = "right floated meta", description)
       )
 )
  }


