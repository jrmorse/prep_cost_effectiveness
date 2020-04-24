#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Here I load the libraries

library(shiny)
library(tidyverse)
library(usmap)
library(shinythemes)
library(ggcorrplot)
library(htmltools)
library(vembedr)

# Need to load in the appropriate data

#
ui <- fluidPage(
    # Here I set the theme and a title for my app.
    
    theme = shinytheme("lumen"),
    
    navbarPage(tags$b("HIV in the United States & The Promise of PrEP"),
               
               # Create the first tab, which will be the first page that people will see when they open up the ShinyApp.
               
               tabPanel("Context",
                        
                        # Load in the first image for the front page.
                        
                        imageOutput("image", width = "100%", height = "100%"),
                        
                        # Title of the page along with a subtitle. Likely will need to tweek this.
                        
                        h1(tags$b("HIV in the United States & The Promise of PrEP"), align = "center"),
                        p(tags$em("Inequity in Prevalence and Prevention"), align = "center"),
                        
                        # Presents an arrow so that readers know the next step is to scroll.
                        
                        imageOutput("arrow", width = "100%", height = "100%"),
                        br(), 
                        
                        # Cleaning the aesthetic of the page by adding a fluid row.
                        
                        fluidRow(column(2), column(8, 
                                          
                                                   
                                                   p("Since the HIV/AIDS epidemic first began in 1981, amazing, albeit slow, progress
                                                   has been made in the treatment and management of the disease in the United
                                                   States. Public Health campaigns combined with powerful pharmaceutical
                                                   innovations have played a pivotal role in changing the perception of the
                                                   diagnosis of HIV from a death sentence to that of a chronic condition. In 2012,
                                                   the United States witnessed the approval of a new drug that was able to prevent
                                                   the onset of HIV for at risk individuals. With, Pre-Exposure Prophylaxis (PrEP)
                                                   medication, high risk individuals have been able to live without contracting
                                                   HIV, thus saving millions of dollars each year for the U.S. healthcare system.
                                                   Accompanying this progress, however, is a spike in the prevalence of many other
                                                   sexually transmitted illnesses (STIs). With this increase in STI prevalence,
                                                   financial resources must be directed towards treatment. In my study, I aim to
                                                   link the advent of PrEP in the United States to this increased observation of
                                                   STIs, before discerning the cost efficiency of PrEP and its’ impact on the
                                                   United States’ healthcare system."), br(),
                                                   
                                                   # Here I load a graph to show the decrease in HIV diagnosis.
                                                  
                                                   plotOutput("hivTotal", width = "100%", height = "100%"), br(),
                                                   
                                                   p("With this project, I have aggregrated data from diverse sources including 
                                                   the CDC, Rollins School of Public Health with Gilead Sciences, and academic
                                                     research presented by the National Center for Biotechnology Information."),
                                                   
                                                   p("Additionally, I have pulled in data regarding the estimated costs for all STIs
                                                   which include the direct medical costs of HIV. With this information, we can see
                                                   if the decreased numbers of HIV has saved the U.S. healthcare money, or if the
                                                   extra resources devoted towards STI treatment has surpassed any potential
                                                   savings. This type of analysis  will make it possible to consider if PrEP has
                                                   been a financially effective treatment in the eyes of the U.S. health system."),
                                                   br()
                        )
                        )
               ),
                                                   # This should be the beginning of the second tab.
                                                   
                                                   tabPanel("HIV",
                                                            sidebarLayout(
                                                              sidebarPanel(
                                                                # Set an icon for the webpage along with wording
                                                                
                                                                HTML('<script> document.title = "HIV in the United States & The Promise of PrEP"; </script>'),
                                                                tags$head(tags$link(rel="shortcut icon", href="https://upload.wikimedia.org/wikipedia/commons/e/e6/World_Aids_Day_Ribbon.png")),
                                                                
                                                                # Add in short description about what this plot is.
                                                                selectInput("sexInput", "Sex", c(
                                                                    "Male",
                                                                    "Female"), multiple = TRUE)
                                                              ,
                                                              
                                                                p(tags$em("Select a transmission category to view nation totals on a yearly basis")),
                                                                
                                                                # Include the option for users to select what transmission category they would like to see.
                                                                
                                                                selectInput("transmissionInput", "Transmission Category", c("Heterosexual contact",
                                                                                                                               "Injection drug use",
                                                                                                                               "Male-to-male sexual contact",
                                                                                                                               "Male-to-male sexual contact and injection drug use",
                                                                                                                               "Other"), multiple = TRUE)
                                                                ),
                                                              mainPanel(
                                                                # Here I add in a fluid row. I'm not sure if I'll change this so that it matches the aesthetic of the first page.
                                                                  
                                                                  fluidRow(column(2), column(8,
                                                                                             # Here I load in the plot
                                                                                             plotOutput("plot")
                                                                                             )
                                                                           )
                                                                  )
                                                              ),
                                                            
                                                            # Here I add the second chart to this page.
                                                            
                                                                sidebarPanel(
                                                                    selectInput("raceInput", "Race", c("Black/African American",
                                                                                                       "White",
                                                                                                       "Hispanic/Latino",
                                                                                                       "Asian"), multiple = TRUE)
                                                                                ),
                                                                    mainPanel(
                                                                        fluidRow(column(2), column(8, 
                                                                        plotOutput("race")
                                                                    )
                                                                    
                                                                )
                                                   )
               ),
               
               # Creating the third tab
               
               tabPanel("PrEP",
                        
                        #Setting a cover photo for the page.
                        
                        imageOutput("medicine2", width = "100%", height = "100%"),
                        br(),
                        
                        # Here I add in a static plot to show rates of prep usage.
                        fluidRow(column(2), column(8,
                        plotOutput("prepTotal", width = "100%", height = "100%"), br()
                        )
                        )
               )
    )
)

server <- function(input, output, session) {
    # Here is where I load the title image for my presentation. I got this image at:
    # "https://www.jnj.com/innovation/6-surprising-things-you-may-not-know-about-hiv-aids-today"
    
    output$image <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './graphics/title.png',
             height = 450,
             width = 800, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    # Here I load in the image of an arrow that I found at: 
    # "http://icon-library.com/icon/scroll-down-icon-png-11.html"
    
    output$arrow <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './graphics/downarrow2.png',
             height = 80,
             width = 90, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    output$hivTotal <- renderPlot({
        hiv_aids_year %>% 
        ggplot(aes(x = year, y = total_cases)) +
            geom_point() +
            geom_text(aes(x = year, y = total_cases, label = total_cases),vjust = -1, nudge_y = .5) +
            labs(title="HIV Diagnoses by Year", subtitle = "2007-2017") +
            theme_classic() +
            scale_x_continuous(
                name = "Year",
                breaks = seq(2007,2017,1),
                label = c("2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017")) +
            scale_y_continuous(
                name = "New Diagnoses",
                limits=c(35000, 50000))
    },
    height = 400,
    width = 700
    )
    
    # Create a plot of nationwide users
     output$plot <- renderPlot({
       filtered <-
         hiv_aids_transmission %>% 
         filter(transmission_category %in% input$transmissionInput,
                sex %in% input$sexInput)
       
       ggplot(data = filtered, aes(x = year, y = total_cases)) +
         geom_col() +
         theme_classic() +
         labs(title="HIV Diagnosis by Sex and Category", subtitle = "2007-2017") +
           scale_x_continuous(
               name = "Year",
               breaks = seq(2007,2017,1),
               label = c("2007", "2008", "2009", "2010", "2011", "2012", "2013","2014", "2015", "2016"," 2017")) +
           scale_y_continuous(
               name = "New Diagnoses")
      
     }) 
     
     # Create plot of HIV diagnosis by race
     
     output$race <- renderPlot({
         filtered2<-
             hiv_aids_maletomalesexualcontact %>% 
             filter(race_ethnicity %in% input$raceInput)
         ggplot(filtered2, aes(x = year, y = total_cases, fill = race_ethnicity)) +
             geom_col(position = "dodge") +
            facet_wrap(~ race_ethnicity) +
             labs(title="HIV Diagnosis Male-to-male Sexual Contact", subtitle = "2007-2017") +
             theme_classic() +
             scale_x_continuous(
                 name = "Year",
                 breaks = seq(2007,2017,2),
                 label = c("2007", "2009", "2011", "2013", "2015", "2017")) +
             scale_y_continuous(
                 name = "New Diagnoses"
             )
     })
     
     output$medicine2 <- renderImage({
         
         # Return a list containing the filename and alt text
         
         list(src = './graphics/medicine2.png',
              height = 450,
              width = 800, style="display: block; margin-left: auto; margin-right: auto;")
     }, deleteFile = FALSE
     )
     
     output$prepTotal <- renderPlot({
         prep_gender_state %>% 
         ggplot(aes(x = year, y = pr_ep_users, fill = sex)) +
             geom_col(position = "dodge") +
             labs(title="PrEP Users by Gender", subtitle = "2012-2018",
                  x = "Year",
                  y = "Total Users",
                  fill = "Sex") +
             theme_classic() +
             scale_x_continuous(
                 breaks = seq(2012,2018,1),
                 label = c("2012", "2013", "2014", "2015", "2016","2017","2018")) +
             scale_y_continuous()
     },
     height = 400,
     width = 700
     )         
}

shinyApp(ui = ui, server = server)



