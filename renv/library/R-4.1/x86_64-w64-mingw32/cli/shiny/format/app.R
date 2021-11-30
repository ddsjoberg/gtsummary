# modified from https://shiny.rstudio.com/articles/progress.html

# This app has a custom format string

library(cli)

# !!! You don't need these in real code.
# cli.progress_show_after makes sure that we see the progress bar from
# the beginning, not only after a delay.
options(cli.progress_show_after = 0)

# !!! You don't need these in real code.
# This also requests logging the progress bar to the standard output,
# in the console.
options(cli.progress_handlers_only = c("shiny", "logger"))

server <- function(input, output) {
  output$plot <- renderPlot({
    input$goPlot # Re-run when button is clicked
    
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    # Number of times we'll go through the loop
    n <- 10

    cli_progress_bar(total = n, "Rendering plot", format = "Starting part {i}")
    for (i in 1:n) {
      # Each time through the loop, add another row of data. This is
      # a stand-in for a long-running computation.
      dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))

      cli_progress_update()

      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.5)
    }

    plot(dat$x, dat$y)
  })
}

ui <- shinyUI(basicPage(
  plotOutput('plot', width = "300px", height = "300px"),
  actionButton('goPlot', 'Go plot')
))

shinyApp(ui = ui, server = server)
