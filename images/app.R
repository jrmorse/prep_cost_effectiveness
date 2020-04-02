#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

if (interactive()) {
    options(device.ask.default = FALSE)
    
    ui <- fluidPage(
        sliderInput("n", "Select Graph", 1, 5, 1),
        plotOutput("plot3")
    )

server <- function(input, output, session) {
    output$plot3 <- renderImage({
        
        filename <- normalizePath(file.path('./images',
                                            paste('image', input$n, '.jpeg', sep='')))
        
        # Return a list containing the filename
        list(src = filename,
             width = 700,
             height = 500)
    }, deleteFile = FALSE)
}

shinyApp(ui, server)
}
