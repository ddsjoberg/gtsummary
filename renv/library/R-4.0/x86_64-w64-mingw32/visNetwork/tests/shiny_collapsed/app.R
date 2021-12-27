library(shiny)
library(visNetwork)

ui <- shinyUI(fluidPage(
  visNetworkOutput("net")
))

server <- shinyServer(function(input, output) {
  
  output$net <- renderVisNetwork({
    set.seed(125)
    nodes <- data.frame(id = 1:15, label = paste("Label", 1:15), title = paste("Label", 1:15),
                        group = sample(LETTERS[1:3], 15, replace = TRUE))
    
    edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
                        to = trunc(runif(15)*(15-1))+1)
    
    # using your own css
    # visNetwork(nodes, edges) %>% 
    #   visInteraction(tooltipStyle = "position: fixed;visibility:hidden;text-decoration: underline;")
    
    # default css + text-decoration
    visNetwork(nodes, edges) %>% 
      visInteraction(tooltipStyle = "position: fixed;visibility:hidden;padding: 5px;font-family: verdana;font-size:14px;
                     font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;
                     border-radius: 3px;border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                     max-width:400px;word-break: break-all;text-decoration: underline;")
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

