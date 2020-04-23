#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(janitor)
library(memoise)

#rsconnect::showLogs()

storesearch <- read.csv("storefront_searches_2020-02-29_2020-03-07.csv") %>%
    clean_names()

visits <- read.csv("visits_2019-01-01_2019-12-31.csv") %>%
    clean_names()

sales <- read.csv("raw_data/sales_2019-01-01_2019-12-31 (1).csv") %>%
  clean_names()


purchases <- visits %>%
  filter(day == "2019-01-01"| day == "2019-02-01" | day == "2019-03-01" |  day == "2019-04-01" |  day ==
           "2019-05-01" | day == "2019-06-01" | day == "2019-07-01" | day == "2019-08-01" |  
           day == "2019-09-01" |  day == "2019-10-01" |  day == "2019-11-01" |  day == "2019-12-01") %>%
  pivot_longer(cols = c(total_sessions, total_carts, total_checkouts, total_orders_placed, total_conversion), names_to = "action") 





ui <- fluidPage(theme = shinytheme("flatly"), 
  
  navbarPage("Explore the data",
             tabPanel("Are people buying?",
                      #used tabset panel to create a panel within my overall panel 
                      tabsetPanel(
                        tabPanel("Exploring Purchase Behavior",
                                 
                                 #used sidebar panel to separate the sidebar from the main panel                                 
                                 sidebarPanel(
                                   
                                   selectInput("action", "Select an customer action:",
                                               choices = c("Visits site" = "total_sessions",
                                                           "Put item in cart" = "total_carts",
                                                           "Continue to checkout screen" = "total_checkouts",
                                                           "Order placed" = "total_orders_placed",
                                                           "Conversion rate" = "total_conversion")
                                   ))))),

  fluidPage(                         
                           mainPanel(
                               h2("Exploring Sales and Customer Interaction"),
                               plotOutput("plot_1")
                           )),
  fluidPage(
    
    fluidPage(
      # Application title
      titlePanel("Word Cloud"),
      
      sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
       
          hr(),
          sliderInput("freq",
                      "Minimum Frequency:",
                      min = 1,  max = 50, value = 15),
          sliderInput("max",
                      "Maximum Number of Words:",
                      min = 1,  max = 300,  value = 100)
        ),
        
        # Show Word Cloud
        mainPanel(
          plotOutput("wordplot")
        )
      )
    ),
    # Define UI (user interface) for application, which is the
    # formating/way the interface will look to users
    
    #used navbarpage to create a navbar shiny app set up, used tab panel to create
    #the tabs within my shiny app, within the first tab map I called imageoutput
    #referencing map, which I then specified in the server section below, inside the
    #second tab, about I used h4 to set a header and p to specify a paragraph of
    #text that I entered
# NEW TAB NOT SHOWING UP
                        tabPanel("When Is Cash Used?",
                                 tabsetPanel(
                                   tabPanel("Expenditures by Payment Type",
                                    
                                    #used select input within sidebar panel to create payment type choices 
                                    sidebarPanel(
                                      selectInput("payment", "Select a Payment Type",
                                                  choices = c("Cash" = 1,
                                                              "Check" = 2,
                                                              "Credit Card" = 3,
                                                              "Debit Card" = 4,
                                                              "Prepaid/gift/EBT card" = 5,
                                                              "Bank Account" = 6,
                                                              "Online Banking" = 7)
                                      )))),
                                    
                                    #within main panel have plot output which corresponds with function in output section below
                                    
                                    mainPanel(
                                      h2("Transaction Volume is Highest at 1pm"),
                                      plotOutput("plot_5")
                                    ))),
# NEW TAB NOT SHOWING UP               
                tabPanel("About", 
                         titlePanel("About"),
                         h3("Project Background and Motivations"),
                         p("Hello, this is where I talk about my project."),
                         h3("About Me"),
                         p("My name is Taylor Greenberg Goldy and I study Design Engineering in the Graduate School of Design and School of Engineering and Applied Sciences. This is the link for my repo:
https://github.com/taylorgreenberggoldy/final_project.git

The data I am using for this project is looking at the behavior of customers and interactions they have on a shopping website that I have access to.  In this data, I'll be able to see how often people are shopping, what are they shopping for as well as what are they searching on the website for.  Through this study, I can hopefully be able to make suggestions to how to improve the overal UX of the site to let people navigate it more thoroughly.  I am slowly getting access to more of the data and will be able to add more files into this project however for now, these two provide sufficient data to get started.

To pull this data, I'm looking at shopify as well as google analytics that pulls basic measurements off of the website of the e-commerce site.

             You can reach me at taylorgg@mde.harvard.edu"))))

server <- function(input, output) {
    datareact <- reactive({
        visits %>%
            filter(day == "2019-01-01"| day == "2019-02-01" | day == "2019-03-01" |  day == "2019-04-01" |
                       day == "2019-05-01" | day == "2019-06-01" | day == "2019-07-01" | day == "2019-08-01" | 
                       day == "2019-09-01" |  day == "2019-10-01" |  day == "2019-11-01" |  day == "2019-12-01") %>% 
            pivot_longer(cols = c(total_sessions, total_carts, total_checkouts, total_orders_placed, 
                                  total_conversion), names_to = "action")
    })
    
    output$plot_1 <- renderPlot({
        # generate type based on input$plot_type from ui
        purchases %>%
            filter(action == input$action) %>%
            ggplot(aes(x = day, y = value, fill = input$action))+
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Shopping Turnover for Online Shop",
                 y= "Actions", x = "Month")+
            theme(axis.text.x=element_text(angle=45, hjust=1))
    })
    
    # Text of the books downloaded from:
    # A Mid Summer Night's Dream:
    #  http://www.gutenberg.org/cache/epub/2242/pg2242.txt
    # The Merchant of Venice:
    #  http://www.gutenberg.org/cache/epub/2243/pg2243.txt
    # Romeo and Juliet:
    #  http://www.gutenberg.org/cache/epub/1112/pg1112.txt
    

      terms <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        isolate({
          withProgress({
            setProgress(message = "Processing corpus...")
            getTermMatrix(input$original_query)
          })
        })
      })
      
      # Make the wordcloud drawing predictable during a session
      wordcloud_rep <- repeatable(wordcloud)
      
      output$wordplot <- renderPlot({
        #Create a vector containing only the text
        
        text <- storesearch$original_query
        # Create a corpus  
        docs <- Corpus(VectorSource(text))
        
        docs <- docs %>%
          tm_map(removeNumbers) %>%
          tm_map(removePunctuation) %>%
          tm_map(stripWhitespace)
        docs <- tm_map(docs, content_transformer(tolower))
        docs <- tm_map(docs, removeWords, stopwords("english"))
        
        dtm <- TermDocumentMatrix(docs) 
        matrix <- as.matrix(dtm) 
        words <- sort(rowSums(matrix),decreasing=TRUE) 
        df <- data.frame(word = names(words),freq=words)
        
        wordcloud(words = df$word, freq = df$freq, min.freq = 1,
                  max.words=200, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"))
      })
    }
    


# Run the application
shinyApp(ui = ui, server = server)
