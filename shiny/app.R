#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reprex)

# Define UI for application that draws a histogram
ui <- fluidPage(

    sliderInput("n", "Select Graphs", 1,5,1),
    plotOutput("plot")
)


server <- function(input, output, session) {
    output$plot <- renderImage({
        # When input$n is 1, filename is ./shiny/image1.jpeg
        filename <- normalizePath(file.path(paste('image', input$n, '.jpeg', sep='')))
        
        # Return a list containing the filename
        list(src = filename,
             width = 700,
             height = 500)
    }, deleteFile = FALSE)
}

shinyApp(ui, server)

