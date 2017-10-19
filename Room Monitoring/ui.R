
## load libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(dygraphs)
library(xts)
library(shinydashboard)


## get min and max dates from data
min_date <- Sys.Date()-60
max_date <- Sys.Date()

## set number of companies/buildings/rooms
companies <- c(1:3)

## generate data
# avg_co2 <- rnorm(60*9,700,150)
# avg_temp <- rnorm(60*9,20,5)
# avg_humidity <- rnorm(60*9,30,10)

# data <- data.frame(company=c(1:3),building=c(1:3),room=c(1:3),avg_co2,avg_temp,avg_humidity)



dashboardPage(
    dashboardHeader(title = "My Dashboard"),
    dashboardSidebar(
        selectInput('company',label='Choose company',choices = c('',companies),selected = ''), 
        uiOutput("building"),
        uiOutput("room"),
        selectInput('adjust_graph',label='Choose graph',choices = c('Room Only','Building Only','Company Only','Room & Building','Room & Company','All'),selected = 'Room Only'), 
        dateRangeInput('dateRange',label = 'Choose date range',start = min_date, end = max_date),
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard"),
            menuItem("Raw data", tabName = "rawdata")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("dashboard",
                    fluidRow(
                        uiOutput("co2"),
                        uiOutput("temp"),
                        uiOutput("humidity")
                    ),
                    fluidRow(
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "CO2 Daily Trend (Avg)",
                            dygraphOutput("co21", width = "100%", height = 300)
                        ),
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Temperature Daily Trend (Avg)",
                            dygraphOutput("temp1", width = "100%", height = 300)
                        ),
                        box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Humidity Daily Trend (Avg)",
                            dygraphOutput("humidity1", width = "100%", height = 300)
                        )        
                    )
            ),
            tabItem("rawdata",
                    numericInput("maxrows", "Rows to show", 25),
                    verbatimTextOutput("rawtable"),
                    downloadButton("downloadCsv", "Download as CSV")
            )
        )
    )
)