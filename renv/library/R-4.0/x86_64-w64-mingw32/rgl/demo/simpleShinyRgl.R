# A simple Shiny demo written by Dieter Menne

options(rgl.useNULL = TRUE)
library(shiny)
library(rgl)

app <- shinyApp(
	ui = bootstrapPage(
		checkboxInput("rescale", "Rescale"),
		rglwidgetOutput("rglPlot")
	),
	server = function(input, output) {
		output$rglPlot <- renderRglwidget({
			try(close3d(), silent = TRUE)
			if (input$rescale) aspect3d(1,1,10) else aspect3d(1,1,1)

			spheres3d(rnorm(100), rnorm(100), rnorm(100,sd = 0.1), col = "red",
				  radius = 0.1)
			axes3d()
			rglwidget()
		})
	})
runApp(app)
