library(DT)
library(shiny)
library(shiny.semantic)

ui <- semanticPage(

  #ship_details(my_test)

# custom_kpi_box <- function(value, label, header, description, icon) {
 div(class = "ui raised link card",
     style = "cursor:default",
   div(class = "right aligned content",
       div(class = "left aligned header", "header"),
       div( class = "left aligned",
         icon,
         div(class = "ui right aligned small horizontal statistic",
             div(class = "value", "value"),
             div(class = "label", "label")
             )
       ), hr(),
       div(class = "right floated meta", "description")
       )
 ),
 
 div(class = "ui raised link card",
     style = "cursor:default",
     div(class = "right aligned content",
         div(class = "left aligned header", "header"),
         div( class = "left aligned",
              icon,
              div(class = "ui right aligned horizontal statistic",
                  div(class = "value", "2016-12-16"),
                  div(class = "label", "label")
              )
         ), hr(),
         div(class = "right floated meta", "description")
     )
 ),
  # }
# ,
 
# custom_kpi_box(value = 300,
#                label = "My UI",
#                header = "KPI1",
#                icon = icon("huge edit"),
#                desc = "try"),

 # value <- 100
 # description = "ABC"
 # icon <- icon("huge plane icon")

 
 tags$i(class = "huge plane icon")

)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)