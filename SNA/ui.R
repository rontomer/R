

options(scipen = 50)

## load libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(shinydashboard)
library(networkD3)
library(lubridate)

## get data
tweet_tags <- read.csv('tweet_tags.csv')
tweet_mentions <- read.csv('tweet_mentions.csv')
tweets <- read.csv('tweets.csv')

## join datasets
search_tags <- inner_join(tweet_tags,tweet_mentions) %>% 
                inner_join(tweets) %>% 
                group_by(y=year(created_at),m=month(created_at),w=week(created_at)) %>% 
                summarise(num_tweets=n()) %>% 
                data.frame()

## get unique years data
years <- unique(search_tags$y)


dashboardPage(
    dashboardHeader(title = "Social Network Graph",titleWidth = 300),
    dashboardSidebar(
        width=300,
        selectInput('year',label='Choose Year',multiple=F,selectize=T,choices = c('',years),selected = ''),
        uiOutput("month"),
        uiOutput("week"),
        uiOutput("tag"),
        sidebarMenu(
            menuItem("Graph",icon=icon("area-chart"), tabName = "Graph"),
            menuItem("Data",icon=icon("table"), tabName = "Data")
        )
    ),
    dashboardBody(
        tags$head(
            tags$style(
                HTML('/* logo */
                     .skin-blue .main-header .logo {
                     background-color: black;
                     }
                     
                     /* logo when hovered */
                     .skin-blue .main-header .logo:hover {
                     background-color: black;
                     }
                     
                     /* navbar (rest of the header) */
                     .skin-blue .main-header .navbar {
                     background-color: black;
                     }        
                     
                     /* main sidebar */
                     .skin-blue .main-sidebar {
                     background-color: black;
                     }
                     
                     .content-wrapper,.right-side {
                     background-color: black;
                     }'
                    )
                )
            ),
        tabItems(
            tabItem("Graph",
                    fluidRow(
                        forceNetworkOutput("force")
                    )
            ),
            tabItem("Data",
                    numericInput("maxrows", "Rows to show", 10),
                    verbatimTextOutput("rawtable"),
                    downloadButton("downloadCsv", "Download as CSV")
            )
        )
    )
)

