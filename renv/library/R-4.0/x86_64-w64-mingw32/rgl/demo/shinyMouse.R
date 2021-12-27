# Use the mouse to select points
# Original version written by Yohann Demont

library(rgl)
library(shiny)

ui <- fluidPage(
  sidebarLayout(
    mainPanel(tabsetPanel(id = "navbar",
                          selected = "3D",
                          tabPanel(title = "2D",
                                   plotOutput("plot_2D", brush = brushOpts(id = "plot_2D_brush",
                                                                           resetOnNew = TRUE,
                                                                           direction = "xy")),
                                   verbatimTextOutput("brush_info_2D")),
                          tabPanel(title = "3D",
                                   uiOutput("plot_3D_mousemode"),
                                   rglwidgetOutput("plot_3D"),
                                   verbatimTextOutput("brush_info_3D"),
                                   verbatimTextOutput("selected"))
    )),
    sidebarPanel(selectInput("plot_x", label = "x feature", choices = colnames(iris)[-5], selected = colnames(iris)[1]),
                 selectInput("plot_y", label = "y feature", choices = colnames(iris)[-5], selected = colnames(iris)[2]),
                 selectInput("plot_z", label = "z feature", choices = colnames(iris)[-5], selected = colnames(iris)[3]),
                 actionButton(inputId = "reset_brush", label = "reset brush"))
  ))

server <- function(input, output, session) {
  # 2D
  output$plot_2D <- renderPlot({
    plot(x = iris[, input$plot_x],
         y = iris[, input$plot_y],
         col = as.integer(iris[, "Species"]))
  })
  output$brush_info_2D <- renderPrint(str(input$plot_2D_brush))
  
  # 3D
  sharedData <- NULL
  output$brush_info_3D <- renderPrint(print(input$rgl_3D_brush, verbose = TRUE))
  
  # How to use selectionFunction3d ?
  output$selected <- renderPrint({
    if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(NULL)
    cat("Selections from crosstalk:\n")
    # Need as.logical because selection() might return NULL
    print(which(as.logical(sharedData$selection())))
    cat("Selections using function:\n")
    f <- selectionFunction3d(input$rgl_3D_brush)
    which(f(iris[, c(input$plot_x, input$plot_y, input$plot_z)]))
  })
  
  output$plot_3D_mousemode <-
    renderUI({
      rglMouse( default = "trackball",
                stayActive = FALSE,
                choices = c("trackball", "selecting"),
                sceneId = "plot_3D")
    })
  open3d(useNULL = TRUE)
  output$plot_3D <- renderRglwidget({
    clear3d()
    dat <- iris[, c(input$plot_x, input$plot_y, input$plot_z, "Species")]
    dat$id <-as.character(seq_len(nrow(iris)))
    plot3d(x = dat[, 1:3], type = "s", size = 1, col = as.integer(iris[, "Species"]), aspect = TRUE)
    sharedData <<- rglShared(id = text3d(dat[, 1:3], text = dat[, "id"], adj = -0.5),
                             group = "SharedData_plot_3D_ids",
                             deselectedFade = 0,
                             selectedIgnoreNone = FALSE)
    shinyResetBrush(session, "rgl_3D_brush")
    rglwidget(shared = sharedData,
              shinyBrush = "rgl_3D_brush")
  })
  observeEvent(input$reset_brush, {
    session$resetBrush("plot_2D_brush")
    shinyResetBrush(session, "rgl_3D_brush")
  })
}

shinyApp(ui, server)
