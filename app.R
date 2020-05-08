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
library(broom)

# Need to load in the appropriate data


complete_costs <- read_csv("csv/complete_costs.csv")
prep_race <- read_csv("csv/prep_race.csv")
prep_gender <- read_csv("csv/prep_gender.csv")
prep_race_sample <- read_csv("csv/prep_race_sample.csv")
hiv_aids_all <- read_csv("csv/hiv_aids_all.csv")
hiv_aids_year <- read_csv("csv/hiv_aids_year.csv")
hiv_aids_maletomalesexualcontact <- read_csv("csv/hiv_aids_maletomalesexualcontact.csv")
chlamydia_gonorrea_total<- read_csv("csv/chlamydia_gonorrea_total.csv")
prep_state <- read_csv("csv/prep_state.csv")
regression <- read_csv("csv/regression.csv")

# Here I begin the UI for my app.

ui <- fluidPage(
  
    # Setting the theme and a title for my app.
    
    theme = shinytheme("lumen"),
    
    navbarPage(tags$b("HIV in the United States & The Potential of PrEP"),
               
               # Create the first tab, which will be the first page that people will see when they open up the ShinyApp.
               
               tabPanel("Context",
                        
                        # Load in the first image for the front page.
                        
                        imageOutput("image", width = "100%", height = "100%"), br(),
                        
                        # Title of the page along with a subtitle.
                        
                        h1(tags$b("HIV in the United States & The Potential of PrEP"), align = "center"),
                        p(tags$em("Inequity in Prevalence and Prevention"), align = "center"), br(),
                        
                        # Presents an arrow so that readers know the next step is to scroll.
                        
                        imageOutput("arrow", width = "100%", height = "100%"),
                        br(), 
                        
                        # Cleaning the aesthetic of the page by adding a fluid row.
                        
                        fluidRow(column(2), column(8, 
                                          
                                                   p("Since the HIV/AIDS epidemic first began in 1981, amazing, albeit slow, progress
                                                   has been made in the treatment and management of the disease in the United
                                                   States. Public Health campaigns combined with powerful pharmaceutical
                                                   innovations have played a pivotal role in decreasing the total number of new
                                                   diagnoses each year and in changing the perception of an HIV diagnoses from that 
                                                   of a death sentence to that of a chronic condition."),br(),
                                                   
                                                   p("One of the most important steps forward came in 2012, when the United States
                                                   witnessed the approval of a new drug that was able to prevent
                                                   the onset of HIV for at risk individuals. With Pre-Exposure Prophylaxis (PrEP)
                                                   medication these individuals have been able to live without contracting
                                                   HIV and have saved billions of dollars for the U.S. healthcare system."), br(),
                                                  
                                                   p("In this application, we will begin by taking a look at the state of HIV today.
                                                   We will then learn where society has been successful at combatting the disease and where
                                                   we still have much work to do."),
                                                   br(), 
                                                   
                                                   h3(tags$b("At a glance...")),
                                                   
                                                   p("The first piece of data to consider is the general downward trend in HIV diagnoses
                                                     as is shown below:"), br(),
                                                   
                                                   # Here I load a graph to show the decrease in HIV diagnosis.
                                                  
                                                   plotOutput("hivTotal", width = "100%", height = "100%"), br(),
                                                   
                                                   p("As you will see in the coming sections of this application, this observed downward trend may 
                                                     not tell the whole story..."), br(),
                                                   
                                                   h3(tags$b("Why should you care?")),
                                                   
                                                   h5(tags$b("Health Inequity")),
                                                   
                                                   p("Our policies may not be working in the way in which we would hope.
                                                   The prevelance of the disease is not equitable, and certain groups are left carrying the brunt of
                                                   the physical, financial, and emotional burden. From an ethical standpoint, this should serve
                                                   as a call to action."),br(),
                                                   
                                                   h5(tags$b("Financial Losses")),

                                                   p("Additionally, the United States is spending billions of dollars on treatment for HIV each year-- and
                                                     losing even more through loss of productivity. If we can lower the rates of HIV diagnoses, we can
                                                     realize massive savings which can be directed towards distinct social programs.")
                                                   )
                                 )
                        ),
                                                   # This is the beginning of the second tab.
                                                   
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
                                                                                         years."), br(),
                                                                                         
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
                                                                    "Female"), multiple = TRUE),
                                                              
                                                                p(tags$em("Select a transmission category to view the nation's total new diagnoses on a yearly basis")),
                                                                
                                                                # Include the option for users to select what transmission category they would like to see.
                                                                
                                                                selectInput("transmissionInput", "Transmission Category", c("Heterosexual contact",
                                                                                                                               "Injection drug use",
                                                                                                                               "Male-to-male sexual contact",
                                                                                                                               "Male-to-male sexual contact and injection drug use",
                                                                                                                               "Other"), multiple = TRUE)
                                                               
                                                                ),
                                                              mainPanel(
                                                                
                                                                # Here I load in the plot
                                                                
                                                                plotOutput("plot"), br(),
                                                                )
                                                              )
                                                            )
                                                            ),
                                                            
                                                            fluidRow(column(2), column(8,
                                                                                       
                                                            p("The purpose of this graph is to show that a decrease in HIV diagnoses has not
                                                            been universally enjoyed across all groups in the United States. The majority
                                                            of the decline in anual diagnoses has been for women. Male to male sexual contact,
                                                            the transmission category responsible for the most new diagnoses, has remained 
                                                            relatively constant throughout the decade."), br(),
                                                            
                                                            p("Let's take a closer look at this transmission category..."), br()
                                                            )
                                                            ),
                                                            
                                                            # Here I add the second chart to this page.
                                                            
                                                            fluidRow(column(2), column(8,
                                                                                       h3(tags$b("Transmission Category: Male to Male Sexual Contact")), 
                                                                                       
                                                                                       h5(tags$b("Total Diagnoses")), br(),
                                                                                       
                                                                                       p("Please select multiple races to vizualize the
                                                                                         new diagnoses of HIV for the four racial groups who carry the largest total
                                                                                         number of diagnoses on an annual basis."), br(),
                                                                                       
                                                                                       sidebarPanel(
                                                                  
                                                                  # A short description of the options.
                                                                  
                                                                  p(tags$em("Select races from the options below")),
                                                                  
                                                                  # I decided to set the default value as Asian here in order to avoid the error message that one value is needed.
                                                                  
                                                                  selectInput("race2Input", "Race", c("Asian",
                                                                                                       "Black/African American",
                                                                                                       "Hispanic/Latino",
                                                                                                       "White"), selected = "Asian", multiple = TRUE)
                                                                                ),
                                                                    mainPanel(
                                                                                                    plotOutput("race2"), br(),br())
                                                                  )
                                                                  ),
                                                                  
                                                                  fluidRow(column(2), column(8,
                                                                                             h5(tags$b("Race as an Explanatory Variable")), br(),
                                                                                             
                                                                                             p("Here you will see a similar graphic to what is shown above. Rather than viewing merely
                                                                                                 the total cases per year, however, we are focusing on the line of best fit."), br(),
                                                                                             
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
                                                                                                 
                                                                                                 plotOutput("race"), br()
                                                                                                 )
                                                                                             )
                                                                           ),
                                                                                               
                                                                                                 fluidRow(column(2), column(8,  
                                                                                                 p("The line of best fit for white men is perhaps the most striking as it is the only line with a negative
                                                                                               slope. While all other selected racial groups are trending up, white men are exhibiting a
                                                                                               decrease in annual diagnoses."), br()
                                                                                                 )
                                                                                                 )
                                                            ),
               
               # Creating the third tab
               
               tabPanel("PrEP",
                        
                        #Setting a cover photo for the page.
                        
                        imageOutput("medicine2", width = "100%", height = "100%"), br(),
                        
                        # Title for the page
                        
                        h1(tags$b("Who is using PrEP?"), align = "center"),
                        
                        hr(),
                        
                        # Here I add in a static plot to show rates of prep usage. I also add the fluid row.
                        
                        fluidRow(column(2), column(8,
                                                   
                                                   h3(tags$b("What is it")),
                                                   
                                                   p("PrEP is a method used by high-risked individuals to prevent HIV. As of October 2019,
                                                   there are two brand named medications that are used for PrEP: Truvada and
                                                   Descovy. These medications work by preventing HIV from spreading throughout one's
                                                   body after exposure. The pills must be taken daily, and, when used correctly,
                                                     the CDC estimates that they are up to 99% effective."),
                                                   
                                                   h3(tags$b("Users")),
                                                   
                                                   h5(tags$b("Nation-wide")),
                                                   
                                                   p("The key takeaways are that usage of PrEP has skyrocketted, and that the majority of
                                                   the users are men. In fact, in 2018 only about 6.6% of the 132,333 users were women."),
                                                   br()
                                                   )
                                 ),
                        
                    
                        fluidRow(column(2), column(8,
                                                   plotOutput("prepTotal", width = "100%", height = "100%"), align = "center", br()
                                                   )
                                 ),
                        fluidRow(column(2), column(8,
                                                   
                                                   h5(tags$b("State-wide")),
                                                  
                                                   p("If we look at regional data, you'll notice that it is states with large urban
                                                   populations that have the highest PrEP usage. States like California, New York,
                                                   Florida, and Texas, all have relatively high total numbers of users. As numbers of PrEP
                                                   users grow, it appears that users remain relatively concentrated in these states."), br(),
                                                   
                                                   p("Considering rates of PrEP usage by 100,000 individuals rather than by total numbers
                                                   would be another effective way of understanding regional differences."), br(),
                                                   
                                                   sidebarPanel(
                                                     
                                                     sliderInput("mapyear", tags$b("Choose a year:"),
                                                                 min = 2012,
                                                                 max = 2017,
                                                                 value = 2012,
                                                                 step = 1,
                                                                 sep = ""
                                                                 )
                                                     ),
                                                   fluidRow(column(2), column(8,
                                                   mainPanel(
                                                     
                                                     # Create the map
                                                     
                                                     plotOutput("maphiv")
                                                     )
                                                   )
                                                   ),
                                                   
                                                   h5(tags$b("Race-wide")),
                                                   
                                                    p("Curious to know how PrEP usage varies accross demographics?
                                                    Imagine you could take a sample of a size of your chosing. Enter 
                                                    the number of individuals you would like in your sample in order to 
                                                    see a racial breakdown of users."), br(),
                                                   
                                                   p("You will note that the userbase is overwhelmingly comprised of white men. In
                                                   2016 it was estimated that 67% of the users were white men, 12% black men,
                                                     13% hispanic or latino men, and 4% asian men."),
                                                   
                                                   p(tags$em("Demographic data of users was only available for the year 2016.")), br(),
                                                   
                                                   # Here I add in a button that I will use to generate a histogram showing the race of male users.
                                                   
                                                   numericInput("n", "Sample Size", 1000),
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
                                                   
                                                   p("With all of these diagnoses, one may wonder how much HIV costs society.
                                                   While there are several frameworks one can use to consider the cost of HIV, for our
                                                     purposes, the most relevant is that of indirect and direct costs."), br(),
                                                   
                                                   p("Direct costs refers to the money spent on the individual for specific illness related care.
                                                   Indirect cost is a bit more abstract. Rather than being a simple value such as
                                                   the cost to fill a perscription, indirect costs relate to lost productivity.
                                                     Another way one may think of indirect costs is as the cost to society."),
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
                                                     
                                                     br(), gt_output('table'), br(), br(), br()
                                                     )
                                                   )
                                 ),
                        
                        fluidRow(column(2), column(8,
                                                   
                                                   p("While these numbers are staggering. You'll notice that the associated costs are
                                                   trending downwards. This is what we would expect to see as the cost of care has
                                                   remained constant year over year while the total diagnoses have been generally decreasing
                                                   since 2008."),br(),
                                                   
                                                   p("It is important to remember, however, that these costs are not decreasing for everyone."),br(),
                                                   
                                                   p("As we've seen, for men who have sex with men, these diagnoses are relatively
                                                   static. What's more, non-white men are facing an increasing
                                                   financial burden over the last 10 years. Take a look at the costs for different racial
                                                     demographics of men who have sex with men"), br(),
                                                   
                                                   sidebarPanel(
                                                     
                                                     # A short description of the options.
                                                     
                                                     p(tags$em("Select a demographic from the list below to view the total associated costs of HIV for your selected group.")),
                                                    
                                                     selectInput("race_costsInput", "Race", c("Black/African American",
                                                                                                       "White",
                                                                                                       "Hispanic/Latino",
                                                                                                       "Asian"), multiple = FALSE)
                                                   ),
                                                   
                                                   mainPanel(
                                                     
                                                     br(), gt_output('table2'), br(), br()
                                                   )
                                                   
                                 )
                        ),
                        
                        fluidRow(column(2), column(8,
                                                   h3(tags$b("Other STIs")),
                                                   
                                                   p("Interested to see how these costs compare to other STIs? In the table below,
                                                     I've compiled costs of two of the most prevalant STIs in the United States: Chlamydia and Gonorrhea."),br(),
                                                   
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
                                                   the result of STI treatment being much more expensive than for men. 
                                                   For HIV, the associated costs are the same for individuals regardless of sex."), br(),
                                                   
                                                   p("Another takeaway is that the costs are much lower in this table than in the
                                                   table of HIV costs. This is to say that even in the most expensive year shown,
                                                   the costs of these other STIs do not come close to the costs incurred to both
                                                     the individual and to society from HIV."), br()
                                            
                                                   )
                                 )
                        ),
               
               tabPanel("Conclusion",
                        
                        imageOutput("conclusion", width = "100%", height = "100%"), br(),
                        
                        h1(tags$b("Conclusion"), align = "center"), 
                        hr(), 
                        
                        fluidRow(column(2), column(8,
                                                   p("From the data considered in this analysis, men who have sex with men should be
                                                     at the focal point of future interventions. This group makes up the overwhelming
                                                     majority of new cases, and they not been privy to the same gains that other
                                                     groups have enjoyed over the last decade."), br(),
                                                   
                                                   p("Given its' efficacy, PrEP should be an exciting opportunity to decrease diagnoses 
                                                   amongst men who have sex with men. The evidence presented in this application shows
                                                   that currently white men are the majority of the drug's user base. They are also the only
                                                   racial group of those infected from male to male sexual contact who are experiencing a 
                                                   decrease in HIV cases on a annual basis."),br(),
                                                   
                                                   p("PrEP usage should not be interpreted as the only driver in the decrease in diagnoses for white men; there
                                                   are plenty of other factors such as socieoeconomic status, access to health insurance, and
                                                   community attitudes that all play a role in driving this trend. PrEP usage can, however, be recognized
                                                   for its' clear association with a decrease in HIV diagnoses year over year."), br(),
                                                   
                                                   p("With this in mind, those looking to contain healthcare costs and to curb new diagnoses of HIV should consider programs
                                                   aimed at expanding access to PrEP for diverse racial groups.")
                                                   )
                                 )
                        ),
                        
               
               # This is my about page. I set up the same columns with fluid rows so that it matches other pages.
               tabPanel("About",
                        
                        fluidRow(column(2), column(8,
                                                   h1(tags$b("About"), align = "left"),
                                                   hr(),
                                                   h5(tags$b("Personal Information")),
                                                   
                                                   p("Hi there,"),
                                                   p("Thanks very much for taking the time to visit this website. I hope you learned
                                                   something new or perhaps confirmed previously held suspicions."),
                                                   p("This webapp is the final project for the Harvard Gov1005 course, and it was submitted
                                                   in May of 2020. Please feel free to reach out with any comments or questions. I would be happy
                                                     to discuss the specifics of my methodology, and I welcome the opportunity
                                                     to incorporate recomendations. The best way to reach me is via email."),
                                                   
                                                   p("Email: jmorse@hsph.harvard.edu"),
                                                   tags$a(href = "https://github.com/jrmorse/prep_cost_effectiveness", "Github Repo"),br(),
                                                   tags$a(href = "https://github.com/jrmorse", "Github Profile"), br(),hr(),
                                                   
                                                   h5(tags$b("Data")),
                                                   
                                                   p("With this project, I have aggregrated data from diverse sources including
                                                   the CDC, Rollins School of Public Health with Gilead Sciences, and various academic
                                                   papers presented by the National Center for Biotechnology Information."),
                                                   
                                                   p("All of my statistics regarding HIV and STI rates were taken directly from the CDC.
                                                     Cost estimations of diseases were taken from academic journals as were the racial
                                                     estimations for PrEP users. As PrEP is relatively new, there are no official statistics
                                                     on the actual demography of its' users. Furthermore, the only data available regarding
                                                     demography of users is from 2016. This is a limitation of my work."), hr(),
                                                   
                                                   h5(tags$b("The Process")),
                                                   
                                                   p("The first work I did was to scrape academic journals in order to gather cost estimates
                                                     for HIV and STIs. The information here is from a highly cited paper that estimates both
                                                     direct and indirect costs of various illnesses. The referenced paper can be found here:
                                                     https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2426671/. This data is from 2006, so I
                                                     adjusted the value of inflation to convey 2020 USD."),
                                                   
                                                   p("After handling the costs, I set about loading statistics on the rates of PrEP usage
                                                   The data I pulled for PrEP usage was published by Gilead
                                                   with Rollins School of Public health and can be found here: https://aidsvu.org/ as well as in
                                                     my repo. Once I loaded and cleaned this data, I scraped another academic journal in order
                                                     to gauge the estimated racial demographics of users. The paper I used can be accessed here:
                                                     https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6193685/."),
                                                   
                                                   p("Once this information was in R, I followed a similar process to load statistics to account
                                                     for HIV and STI rates mostly beginning in 2012. These datasets came directly from a CDC platform
                                                     called AtlasPlus. This website can be found here: https://www.cdc.gov/nchhstp/atlas/index.htm
                                                     as well as in my repo."),
                                                   
                                                   p("With all this data cleaned and matched appropriately, I was able to begin the process
                                                     of creating graphs and interactive tools on the Shiny app."), hr(),
                                                   
                                                   h5(tags$b("Limitations")),
                                                   
                                                   p("My original vision for this project was to consider the role of PrEP in driving down HIV
                                                     diagnoses and driving up increased rates of STIs in the United States. I wanted to
                                                     analyze if the savings from HIV averted cases was greater than the costs incurred from
                                                     the large increase in STI diagnoses."),
                                                   
                                                   p("Unfortunately, the datasets were not complete enough to accurately
                                                     link PrEP users to new STI cases. STI data published by the CDC does not account for type of
                                                     transmission in the way that HIV data does. As this is the case, I did not feel that making such
                                                     an estimate was appropriate. From my research, however, it appears that the costs associated
                                                     with each HIV diagnoses far outweigh the costs of STIs that may be incurred by PrEP users.
                                                     In other words, this medication may be cost effective."),
                                                   
                                                   p("It is important to mention that I have not included the price of taking PrEP.
                                                   As of 2019, private insurance companies, and even Medicaid,
                                                     must cover the medication for their beneficiaries.
                                                     This is not to say that the medication is cheap for such organizations.
                                                     For those without insurance, out-of-pocket costs are estimated to hover around $13,000 per year.
                                                     This effectively puts PrEP far out of reach for the millions of uninsured folks in the United
                                                     States."),br()
                                                   
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
         filtered_race<-
             hiv_aids_maletomalesexualcontact %>% 
             filter(race_ethnicity %in% input$raceInput)
        ggplot(filtered_race, aes(x = year, y = total_cases)) +
          geom_point(fill = wes_palette("Darjeeling2", n = 1)) + 
          geom_smooth(method = "lm", fill = wes_palette("Zissou1", n = 1)) +
          facet_wrap(~ race_ethnicity) + 
          labs(title="HIV Diagnosis Male-to-male Sexual Contact", subtitle = "2007-2017") +
              theme_classic() +
              scale_x_continuous(
                 name = "Year",
                 breaks = seq(2007,2017,2),
                 label = c("2007", "2009", "2011", "2013", "2015", "2017")) +
             scale_y_continuous(
                 name = "New Diagnoses")
     })
     
        output$race2 <- renderPlot({
          filtered_race2<-
            hiv_aids_maletomalesexualcontact %>% 
            filter(race_ethnicity %in% input$race2Input)   
         ggplot(filtered_race2, aes(x = year, y = total_cases)) +
             geom_col(fill = wes_palette("IsleofDogs1", n = 1)) +
            facet_wrap(~ race_ethnicity) +
             labs(title="HIV Diagnosis Male-to-male Sexual Contact", subtitle = "2007-2017") +
             theme_classic() +
             scale_x_continuous(
                 name = "Year",
                 breaks = seq(2007,2017,2),
                 label = c("2007", "2009", "2011", "2013", "2015", "2017")) +
             scale_y_continuous(
                 name = "New Diagnoses")
     })
  
     
     # Loading the pages title image
     
     output$medicine2 <- renderImage({
         
         # Return a list containing the filename and alt text
         
         list(src = './graphics/medicine2.png',
              height = 450,
              width = 800, style="display: block; margin-left: auto; margin-right: auto;")
     }, deleteFile = FALSE
     )
     
    # Here I am creating the plot that shows how PrEP has been used by sexes on an annual basis.
     
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
         filter(race_ethnicity == input$race_costsInput,
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
     
     
     output$conclusion <- renderImage({
       
       # Return a list containing the filename and alt text
       
       list(src = './graphics/conclusion.png',
            height = 450,
            width = 800, style="display: block; margin-left: auto; margin-right: auto;")
     }, deleteFile = FALSE
     )
     
     output$maphiv <- renderPlot({
       
       if(input$mapyear== " ") {
         return()
       }
       
       # filter the dataset based on year
       
       filter6 <- prep_state %>% 
         filter(year == input$mapyear)

     plot_usmap(data = filter6, values = "total_cases", regions = "state", size = 0.05) + 
       theme(panel.background = element_rect(color = "white", fill = "white")) +
       scale_fill_continuous(low = "white", high = "lightcoral", name = "Total Users") +
       labs(title = paste("Map of PrEP Usage in ", input$mapyear, sep = "")) + 
       theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 20))
     
     },
     
     height = 400,
     width = 700
     )
}

shinyApp(ui = ui, server = server)





