library(shiny)
library(rgl)

options(rgl.useNULL = TRUE)

ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("red",
               rglwidgetOutput('thewidget1')),
      tabPanel("green",
               rglwidgetOutput('thewidget2'))
    ))
)

server <- function(input, output, session) {
  
  x <- rnorm(100)
  y <- 2*rnorm(100)
  z <- 10*rnorm(100)
  open3d()
  plot3d(x, y, z, col = "red")
  scene1 <- scene3d()
  plot3d(z, y, x, col = "green")
  scene2 <- scene3d()
  close3d()
  
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  output$thewidget1 <- renderRglwidget(
    rglwidget(scene1)
  )
  
  output$thewidget2 <- renderRglwidget(
    rglwidget(scene2)
  )

}

if (interactive())
  shinyApp(ui = ui, server = server)