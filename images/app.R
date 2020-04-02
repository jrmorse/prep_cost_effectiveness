#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that will show my plot that I created in 1G
if (interactive()) {
    
ui <- fluidPage(
   
        sliderInput("n", "Number of observations",1, 10,5),
        imageOutput("image1"),
        imageOutput("image2"),
        imageOutput("image3")
    )


# Create a function where the output calls the actual plot I created. Then go
# ahead and add specifications for sizing.
server <- function(input, output, session) {
    output$plot3 <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path('./shiny_graphs',
                                            paste('image', input$n, '.jpeg', sep='')))
        
        # Return a list containing the filename
        list(src = filename)
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
}

