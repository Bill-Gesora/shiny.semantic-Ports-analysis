
source("utils.R")

filtersUi <- function(id, label = "ports_location"){
  ns <- NS(id)
  
  # Configure dropdown begin values
  begin_type <- analyzed_data %>% 
    filter(ship_type == "Cargo")
  
  begin_name <- analyzed_data %>% 
    filter(ship_type == "Cargo")
  
  filter_dropdowns <- div(class = "ui stackable width grid",
                            div(class = "two wide right aligned column",
                                tags$h3("Vessel type:")
                            ),
                            div(class = "four wide column",
                                selectInput(
                                  inputId = ns("ship_type_filter"),
                                  label = NULL,
                                  choices = unique(analyzed_data$ship_type),
                                  selected = unique(begin_type$ship_type),
                                  type = "fluid selection dropdown"
                                )
                                ),
                          div(class = "two wide right aligned column",
                              tags$h3("Vessel name:")
                          ),
                          div(class = "four wide column",
                              selectInput(
                                inputId = ns("ship_name_filter"),
                                label = NULL,
                                choices =  unique(begin_name$ship_name),
                                selected = NULL,
                                type = "fluid search selection"
                              )
                          ),
                          div(class = "four wide column",
                              tags$h3(
                                id = "report_status_date",
                                tags$em(
                                paste0("...status as at ",max(ships_data$observation_date))
                                )
                              )
                          )
                          )

  tagList(
    filter_dropdowns
  )
  
}

filtersServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      # Variable for module data returned
      return_filter_data <- reactiveValues()
      
      observe({
        req(input$ship_type_filter)
        # Filtering for the updated value
        if(!is.null(input$ship_type_filter)) {
          filter_data <- data() %>% 
            filter(ship_type %in% input$ship_type_filter) 
          
          updateSelectInput(session,
                            inputId = "ship_name_filter",
                            choices = unique(filter_data$ship_name),
                            selected = NULL,
                            label = NULL
          ) 
        } 
      })
      
      observe({
        req(input$ship_type_filter)
        
      if (!is.null(input$ship_name_filter)) {
          isolate(input$ship_type_filter)
      } 
        
      })
          
        
      filtered_data <- reactive({

        data() %>%
          dplyr::filter(
            conditional_filter(!is.null(input$ship_type_filter),
                              ship_type %in% isolate(input$ship_type_filter)),
            conditional_filter(!is.null(input$ship_name_filter),
                               ship_name %in% isolate(input$ship_name_filter))
          )
      })
      
      observe({
        validate(
          need(nrow(filtered_data())  > 0, "File still loading...")
        )
      return_filter_data$data <- filtered_data()
      })
      
      return(return_filter_data)
      
    }
  )
  
}