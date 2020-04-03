#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("Graph")
    )
)

# Create a function where the output calls the actual plot I created. Then go
# ahead and add specifications for sizing.
server <- function(input, output) {
    output$Graph <- renderImage({
        list(src = "image1.jpeg",
             contentType = 'jpeg',
             width = 950,
             height = 600)
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)


