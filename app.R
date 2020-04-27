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
library(shinythemes)
library(readxl)
library(janitor)
library(gt)
library(rvest)
library(reprex)
library(fivethirtyeight)
library(stringr)
library(usmap)
library(shinythemes)
library(ggcorrplot)
library(htmltools)
library(vembedr)
library(wesanderson)
library(infer)
library(scales)
library(tidyverse)

# Need to load in the appropriate data


complete_costs <- read_csv("csv/complete_costs.csv")
prep_race <- read_csv("csv/prep_race.csv")
prep_gender <- read_csv("csv/prep_gender.csv")
prep_race_sample <- read_csv("csv/prep_race_sample.csv")
hiv_aids_all <- read_csv("csv/hiv_aids_all.csv")
hiv_aids_year <- read_csv("csv/hiv_aids_year.csv")
hiv_aids_maletomalesexualcontact <- read_csv("csv/hiv_aids_maletomalesexualcontact.csv")
chlamydia_gonorrea_total<- read_csv("csv/chlamydia_gonorrea_total.csv")

#
ui <- fluidPage(
    # Here I set the theme and a title for my app.
    
    theme = shinytheme("lumen"),
    
    navbarPage(tags$b("HIV in the United States & The Promise of PrEP"),
               
               # Create the first tab, which will be the first page that people will see when they open up the ShinyApp.
               
               tabPanel("Context",
                        
                        # Load in the first image for the front page.
                        
                        imageOutput("image", width = "100%", height = "100%"), br(),
                        
                        # Title of the page along with a subtitle. Likely will need to tweek this.
                        
                        h1(tags$b("HIV in the United States & The Promise of PrEP"), align = "center"),
                        p(tags$em("Inequity in Prevalence and Prevention"), align = "center"), br(),
                        
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
                                                   HIV, thus saving millions of dollars each year for the U.S. healthcare system."), br(),
                                                  
                                                   p(" Throughout this application, we will take a look at the state of HIV today.
                                                   We will learn where we have been successful at combatting the disease and where
                                                   we still have work to do."),
                                                   br(), 
                                                   
                                                   h3(tags$b("At a glance...")),
                                                   
                                                   p("The first piece of data to consider is the general downward trend in HIV diagnoses
                                                     as is shown below:"), br(),
                                                   
                                                   # Here I load a graph to show the decrease in HIV diagnosis.
                                                  
                                                   plotOutput("hivTotal", width = "100%", height = "100%"), br(),
                                                   
                                                   p("As you will see in the coming sections of this app, this observed downward trend may 
                                                     not tell the whole story..."), br(),
                                                   
                                                   h3(tags$b("Why should you care?")),
                                                   
                                                   p("Our policies may not be working in the way in which we would hope.
                                                   The prevelance of the disease is not equitable, and certain groups are left carrying the brunt of
                                                   the physical, financial, and emotional burden. From an ethical standpoint, this should serve
                                                   as a call to action."),br(),

                                                   p("Additionally, the United States is spending billions of dollars on treatment for HIV each year, and
                                                     losing even more through loss of productivity. If we can lower the rates of HIV diagnoses, we could
                                                     realize massive savings which could be directed towards other soc")
                                                   )
                                 )
                        ),
                                                   # This should be the beginning of the second tab.
                                                   
                                                   tabPanel("HIV",
                                                            
                                                            # Here I load the cover photo.
                                                            
                                                            imageOutput("population", width = "100%", height = "100%"), br(),
                                                            
                                                            # Here is the title for the HIV page
                                                            
                                                            h1(tags$b("Who is being diagnosed?"), align = "center"),
                                                            
                                                            hr(),
                                                            
                                                            fluidRow(column(2), column(8,
                                                                                       h3(tags$b("General Population")), br(),
                                                                                       
                                                                                       p("With this interactive graph, we can start taking a closer look at who is being
                                                                                       diagnosed with HIV. Go ahead and select one or multiple sexes along with several
                                                                                       transmission categories in order to see how rates have changed throughout the
                                                                                         year."), br(),
                                                                                       
                                                                                       p("You will notice that when you have all sexes and transmission categories
selected, the chart mirrors what you saw on the introductory page. However, when
you start segmenting out sex and condition, there's a different story."), br(),
                                                                                       
                                                            sidebarLayout(
                                                              
                                                              sidebarPanel(
                                                                # Set an icon for the webpage along with wording
                                                                
                                                                HTML('<script> document.title = "HIV in the United States & The Promise of PrEP"; </script>'),
                                                                tags$head(tags$link(rel="shortcut icon", href="https://upload.wikimedia.org/wikipedia/commons/e/e6/World_Aids_Day_Ribbon.png")),
                                                                
                                                                # A short description of the options.
                                                                                           
                                                                p(tags$em("Select either Male, Female, or both sexes to see how rates of diagnoses vary by sex.")),
                                                                
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
                             
                                                                                             # Here I load in the plot
                                                                                             plotOutput("plot"), br(),
                                                              )
                                                            )
                                                            )
                                                            ),
                                                            fluidRow(column(2), column(8,
                                                                                       
                                                            p("What I hope one gathers from this graph is that a decrease in HIV diagnoses has been enjoyed
equitably across the country. Most of the decline in diagnoses has been for
women. Male to male sexual contact, the transmission category responsible for
the most new diagnoses, has remained relatively constant throughout the decade.
Let's take a closer look at this transmission category.")
                                                            )
                                                            ),
                                                            
                                                            # Here I add the second chart to this page.
                                                            
                                                            fluidRow(column(2), column(8,
                                                                                       h3(tags$b("Transmission Category")), br(),
                                                                                       sidebarPanel(
                                                                  
                                                                  # A short description of the options.
                                                                  
                                                                  p(tags$em("Select one or multiple races from the options below")),
                                                                  
                                                                  # I decided to set the default value as Asian here in order to avoid the error message that one value is needed.
                                                                  
                                                                  selectInput("raceInput", "Race", c("Asian",
                                                                                                       "Black/African American",
                                                                                                       "Hispanic/Latino",
                                                                                                       "White"), selected = "Asian", multiple = TRUE)
                                                                                ),
                                                                    mainPanel(
                               
                                                                        plotOutput("race")
                                                                    )
                                                                    
                                                                )
                                                   )
               ),
               
               # Creating the third tab
               
               tabPanel("PrEP",
                        
                        #Setting a cover photo for the page.
                        
                        imageOutput("medicine2", width = "100%", height = "100%"),
                        
                        # Title for the page
                        
                        h1(tags$b("Who is using PrEP?"), align = "center"), br(),
                        
                        hr(),
                        
                        # Here I add in a static plot to show rates of prep usage. I also add the fluid row.
                        
                        fluidRow(column(2), column(8,
                                                   
                                                   h3(tags$b("What is it")),
                                                   
                                                   p("PrEP is a method used by high-risked individuals to prevent HIV. Currently,
there are two brand named medications that are used for PrEP: Truvada and
Descovy. These medications work by preventing HIV from spreading throughout your
body after exposure. The pills must be taken daily, and, when used correctly,
the CDC estimates that they are up to 99% effective."),
                                                   
                                                   h3(tags$b("Users")),
                                                   
                                                   p("The key takeaways are that usage of PrEP has skyrocketted, and that the majority of the users are men. In fact, in 2018 only about 6.6% of the 132,333 users were women."),
                                                   br(),
                                                   
                                                   plotOutput("prepTotal", width = "100%", height = "100%"), br(),
                                                  
                                                    p("Curious to know how PrEP usage varies accross demographics? Imagine you could take a sample of a size of your chosing. Enter the number of individuals you would like in your sample in order to see the racial breakdown of users. You will note that the userbase is overwhelmingly comprised of white men. In 2016 it was estimated that 67% of the users were White men, 12% Black men, 13% Hispanic or Latino men, and 4% Asian men."),br(),
                                                   
                                                   # Here I add in a button that I will use to generate a histogram showing the race of male users.
                                                   
                                                   numericInput("n", "Sample Size", 50),
                                                   actionButton("go", "Go"),
                                                   plotOutput("x")
                                                   )
                                 )
                        ),
               
               # Here I make the page that will discuss the costs of such STIs and HIV.
               
               tabPanel("Costs",
                        
                        # Setting cover photo for the page
                        
                        imageOutput("costs", width = "100%", height = "100%"),
                        br(),
                        
                        h1(tags$b("Associated Costs"), align = "center"), 
                        
                        hr(), 
                        
                        fluidRow(column(2), column(8,
                                                   
                                                   h3(tags$b("HIV")),
                                                   
                                                   p("With all of these diagnoses, one may wonder how much it costs society. While
there are several frameworks once can use to consider the cost of HIV, for our
purposes, the most relevant is that of indirect and direct costs."), br(),
                                                   p("Direct cost
refers to the money spent on the individual for specific illness related care.
Indirect cost is a bit more abstract. Rather than being a simple value such as
the cost to fill a perscription, indirect costs relate to lost productivity.
Another way to think of indirect costs is as the cost to society."),
                                                   br(),
                                                   
                                                   sidebarPanel(
                                                     
                                                     # A short description of the options.
                                                     
                                                     p(tags$em("Select a year from the list below to view the total associated costs of HIV.")),
                                                     
                                                     selectInput("yearInput", "Year", c("2008",
                                                                                        "2009",
                                                                                        "2010",
                                                                                        "2011",
                                                                                        "2012",
                                                                                        "2013",
                                                                                        "2014",
                                                                                        "2015",
                                                                                        "2016",
                                                                                        "2017"), multiple = FALSE)
                                                     ),
                                                   mainPanel(
                                                     # I add this in because if not there is an error that one or more values is needed for faceting.
                                                     
                                                     br(), gt_output('table'), br(), br(), br()
                                                     )
                                                   )
                                 ),
                        fluidRow(column(2), column(8,
                                                   
                                                   p("While these numbers are staggering. You'll notice that the associated costs are
trending downwards. This is what we would expect to see as new diagnoses has
been generally decreasing since 2008. It is important to remember, however, that
these costs are not decreasing for everyone."),br(),
                                                   p("As we've seen, for men who have sex with men, these diagnoses are relatively
static. What's more, these men who are not white are facing an increasing
financial burden over the last 10 years. Take a look at the direct costs for different racial
                                                     demographics of men who have sex with men below."),
                                                   
                                                   br(),
                                                   
                                                   sidebarPanel(
                                                     
                                                     # A short description of the options.
                                                     
                                                     p(tags$em("Select a demographic from the list below to view the total associated costs of HIV for your selected group.")),
                                                    
                                                     selectInput("race2Input", "Race", c("Black/African American",
                                                                                                       "White",
                                                                                                       "Hispanic/Latino",
                                                                                                       "Asian"), multiple = FALSE)
                                                   ),
                                                   mainPanel(
                                                     # I add this in because if not there is an error that one or more values is needed for faceting.
                                                     
                                                     br(), gt_output('table2'), br(), br()
                                                   )
                                                   
                                 )
                        ),
                        fluidRow(column(2), column(8,
                                                   h3(tags$b("Other STIs")),
                                                   
                                                   p("Interested to see how these costs compare to other STIs? In the table below, I've compiled costs of two of the most prevalant STIs in the United States: Chlamydia and Gonorrhea."),br(),
                                                   
                                                   sidebarPanel(
                                                     
                                                     # A short description of the options.
                                                     
                                                     p(tags$em("Select one of the two STIs from the options below")),
                                                     
                                                     selectInput("stiInput", "STI", c("Chlamydia",
                                                                                         "Gonorrhea"), multiple = FALSE)
                                                   ),
                                                   mainPanel(
                                                    
                                                     br(), gt_output('table3'), br(), br()
                                                   ),
                                                   
                                                   p("A couple of things should jump out here... The first being that the costs
incurred by women are generally much higher than that of men. Rather than
resulting from a massive increase in number of cases for women, this is largely
the result of STI treatment being much more expensive. For HIV, the associated
costs are the same accross sex."), br(),
                                                   p("Another takeaway is that the values are much lower in this table than in the
table of HIV costs. This is to say that the costs of these other STIs do not
come close to the costs incurred to both the individual and to society with HIV.
With this in mind, finding a method for curbing the prevalence of HIV should be
a top priority for those financially invested players such as policy makers and
other officials."), br(),
                                                   
                                                   
                        )
                        )
                        ),
                        
               
               # This is my about page. I set up the same columns with fluid rows so that it matches other pages.
               tabPanel("About",
                        
                        fluidRow(column(2), column(8,
                                                   includeMarkdown("aboutproject.Rmd")
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
    
    output$population <- renderImage({
      # Return a list containing the filename and alt text
      list(src = './graphics/population.png',
           height = 450,
           width = 800, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    # Loading the static graph to show general downward trend in HIV diagnoses
    
    output$hivTotal <- renderPlot({
        hiv_aids_year %>% 
        ggplot(aes(x = year, y = total_cases)) +
            geom_point(color = "lightskyblue") +
            geom_text(aes(x = year, y = total_cases, label = total_cases),color = "deepskyblue3", vjust = -1, nudge_y = .5) +
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
    
    # Create a plot of HIV diagnoses by sex/ transmission category
    
     output$plot <- renderPlot({
       filtered <-
         hiv_aids_transmission %>% 
         filter(transmission_category %in% input$transmissionInput,
                sex %in% input$sexInput)
       
       ggplot(data = filtered, aes(x = year, y = total_cases, fill = sex)) +
         geom_col() +
         theme_classic() +
         scale_fill_manual(values = wes_palette("Darjeeling2", n = 2)) +
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
         
         ggplot(filtered2, aes(x = year, y = total_cases)) +
             geom_col(fill = wes_palette("IsleofDogs1", n = 1)) +
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
     
     # Loading the pages title image
     
     output$medicine2 <- renderImage({
         
         # Return a list containing the filename and alt text
         
         list(src = './graphics/medicine2.png',
              height = 450,
              width = 800, style="display: block; margin-left: auto; margin-right: auto;")
     }, deleteFile = FALSE
     )
     
    # Here I am creating the plotthat shows how PrEP has been used by genders on an annual basis.
     
     output$prepTotal <- renderPlot({
       ggplot(data = prep_gender, aes(x = year, y = total_cases, fill = sex)) +
             geom_col(position = "dodge") +
             labs(title="PrEP Users by Gender", subtitle = "2012-2018",
                  x = "Year",
                  y = "Total Users",
                  fill = "Sex") +
         theme_classic() +
         scale_fill_manual(values = wes_palette("Darjeeling2", n = 2)) +
         scale_x_continuous(
           breaks = seq(2012,2018,1),
           label = c("2012", "2013", "2014", "2015", "2016","2017","2018"))
       
     },
     height = 400,
     width = 700
     )     
     
     # This is for the reactive button on the PrEP page
     
     reactive <- eventReactive(input$go, {
       input$n
     })
   
     # This is a reactive plot that fills the sample size based off the value entered by the user. 
     # The point of this is to show how many white people take it vs. other racial groups.
     
     output$x <- renderPlot({
       prep_race_sample %>% 
         rep_sample_n(size = reactive(), replace = TRUE, reps = 1) %>% 
         ggplot(aes(x = race, fill = race)) +
         geom_bar() +
         theme_classic() + 
         scale_fill_manual(values = wes_palette("Darjeeling2", n = 5)) +
         labs(
           title = "Male PrEP Users",
           subtitle = "Racial Charecteristics of Sample",
           x = "Race",
           y = "Count"
         )
       
     })
     
     output$costs <- renderImage({
       # Return a list containing the filename and alt text
       list(src = './graphics/healthcaremoney.jpg',
            height = 450,
            width = 800, style="display: block; margin-left: auto; margin-right: auto;")
     }, deleteFile = FALSE
     )
     
     # First table of the fourth page. All associated costs for all diagnoses.
     
     output$table <- render_gt({
       
       filtered3 <-
         hiv_aids_year %>% 
         filter(year == input$yearInput)
       
       filtered3 %>% 
         mutate(direct_costs = total_cases * 259997.01,
                indirect_costs = total_cases * 1089414.34,
                total_costs = direct_costs + indirect_costs) %>%
         select(direct_costs, indirect_costs, total_costs) %>% 
        gt() %>% 
        tab_header(
          title = "HIV Associated Costs by Year",
          subtitle= "Prices are in 2020 USD") %>% 
         cols_label(
           direct_costs = "Direct Costs",
           indirect_costs = "Indirect Costs",
           total_costs = "Total Costs") %>% 
         fmt_currency(., 1:3) %>% 
         fmt_number(.,1:3, decimals = 0)
     })
     
     # Second table of the fourth page. Associated costs by race for msm.
     
     output$table2 <- render_gt({
       
       filtered4 <-
         hiv_aids_all %>% 
         filter(race_ethnicity == input$race2Input,
                transmission_category == "Male-to-male sexual contact")
       
       filtered4 %>% 
         group_by(year) %>%
         summarize(total_cases = sum(cases)) %>% 
         mutate(direct_costs = total_cases * 259997.01,
                indirect_costs = total_cases * 1089414.34,
                total_costs = direct_costs + indirect_costs) %>%
         select(year, direct_costs, indirect_costs, total_costs) %>%
         ungroup(year) %>% 
         gt() %>% 
         tab_header(
           title = "HIV Associated Costs by Year",
           subtitle= "Prices are in 2020 USD") %>% 
         cols_label(
           year = "Year",
           direct_costs = "Direct Costs",
           indirect_costs = "Indirect Costs",
           total_costs = "Total Costs") %>% 
         data_color(2:4, "Reds") %>% 
         fmt_currency(., 2:4) %>% 
         fmt_number(.,2:4, decimals = 0)
       
     })
    
     output$table3 <- render_gt({
       filtered5 <-
         chlamydia_gonorrea_total %>% 
         filter(indicator == input$stiInput) 
       
       filtered5 %>% 
         group_by(indicator, year, sex) %>%
       ungroup(indicator, sex, year) %>% 
      select(year, sex, direct_costs, indirect_costs, total_costs) %>%
       gt() %>% 
         tab_header(
           title = "STI Associated Costs by Year",
           subtitle= "Prices are in 2020 USD") %>% 
         cols_label(
           year = "Year",
           sex = "Sex",
           direct_costs = "Direct Costs",
           indirect_costs = "Indirect Costs",
           total_costs = "Total Costs") %>% 
         data_color(3:5, "Reds") %>% 
         fmt_currency(., 3:5) %>% 
         fmt_number(.,3:5, decimals = 0)
     })
     # Here I load in the about page of my project. It is an HTML document.
     
     output$about <- renderUI({
       HTML(markdown::markdownToHTML(knit('aboutproject.html', quiet = TRUE)))
     })
}

shinyApp(ui = ui, server = server)



