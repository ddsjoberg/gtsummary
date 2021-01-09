library(shiny)
library(sass)
library(colourpicker)

ui <- fluidPage(
  headerPanel("Sass Size Example"),
  uiOutput("sass"),

  sidebarPanel(
    sliderInput("width", "Image Percent of Screen",
                min = 1, max = 100, value = 100),
    colourInput("color", "Background Color", value = "#6498d2",
                showColour = "text")
  ),

  mainPanel(
    plotOutput("distPlot"),
    br()
  ),

  column(6, verbatimTextOutput("scssTxt"), verbatimTextOutput("sassFile")),
  column(6, verbatimTextOutput("cssTxt"))
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(500))
  })

  variables <- reactive({
    list(
      color = input$color,
      width = input$width
    )
  })

  sass_input <- reactive({
    list(
      variables(),
      sass_file("sass-size.scss")
    )
  })
  compiled_css <- reactive({
    sass(sass_input())
  })

  output$sass <- renderUI({
    tags$head(tags$style(compiled_css()))
  })

  output$scssTxt <- renderText({
    paste0("/* Sass */\n", as_sass(sass_input()))
  })
  output$sassFile <- renderText({
    paste0(
      "/* sass-size.scss */\n\n",
      paste0(readLines("sass-size.scss"), collapse = "\n")
    )
  })

  output$cssTxt <- renderText({
    paste0("/* Compiled CSS */\n", compiled_css())
  })
}

shinyApp(ui, server)
