## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  library(shiny)
#  
#  ui <- fluidPage(
#  
#     # Application title
#     titlePanel("mtcars"),
#  
#     sidebarLayout(
#        sidebarPanel(
#           sliderInput("mpg", "mpg Limit",
#                       min = 11, max = 33, value = 20)
#        ),
#  
#        mainPanel(
#           tableOutput("mtcars_kable")
#        )
#     )
#  )
#  
#  server <- function(input, output) {
#    library(dplyr)
#    library(kableExtra)
#     output$mtcars_kable <- function() {
#       req(input$mpg)
#       mtcars %>%
#         mutate(car = rownames(.)) %>%
#         select(car, everything()) %>%
#         filter(mpg <= input$mpg) %>%
#         knitr::kable("html") %>%
#         kable_styling("striped", full_width = F) %>%
#         add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6))
#     }
#  }
#  
#  # Run the application
#  shinyApp(ui = ui, server = server)
#  
#  

## ---- eval=FALSE--------------------------------------------------------------
#  shiny::runGist("https://gist.github.com/haozhu233/9e675e1a8a1bb4744f9ebc9246a2366b")

