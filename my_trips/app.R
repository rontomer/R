






## load libraries
library(dplyr)
library(googlesheets)
library(ggmap)
library(leaflet)
library(googleway)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title='My Trips',titleWidth = 200),
    dashboardSidebar(width = 200),
    dashboardBody(leafletOutput("mymap",height='700px'))
)

server <- function(input, output){ 
    

    
    ## set url to googlesheet and get data
    url <- 'https://docs.google.com/spreadsheets/d/1JNBrDna6Oxi_S2v7JHNhq3AOtkAF6KWsyCS4gsX5TTo/edit#gid=0'
    data <- gs_url(url) %>% gs_read(ws='data') %>% data.frame()
    
    icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = c('red','orange','orange','green','green','green','green'
                        ,'purple','blue','pink','pink','pink','pink','pink','pink')
    )
    
    output$mymap <- renderLeaflet({
        
        labels <- c('Middle East','Pacific','Asia','Central America','North America','Europe')
        sizes <- c(20, 20, 20,20, 20, 20)
        colors <- c('red','orange','green','purple','blue','pink')
        colorAdditions <- paste0(colors, "; width:", sizes, "px; width:", sizes, "px")
        labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
        
        leaf <-
            leaflet(data) %>% 
            addTiles() %>% 
            setView(34.78, 32.08, zoom = 2) %>% 
            addAwesomeMarkers(data$lon
                              ,data$lat
                              ,label = ~place
                              ,labelOptions = labelOptions(noHide = T,textOnly = T,textsize = "15px")
                              ,clusterOptions = markerClusterOptions()
                              ,icon=icons) %>% 
            addLegend(colors = colorAdditions #c('red','orange','green')
                      ,labels = labelAdditions #c('Not Selling','Improve','Selling')
                      ,title='Seller Status &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp'
                      ,position='bottomleft'
            ) %>% 
            addTiles(group = "OSM (default)") %>%
            addProviderTiles(providers$Stamen.TonerLabels, group = "OSM Label") %>%
            addProviderTiles(providers$OpenStreetMap.HOT, group = "OSM Hot") %>% 
            addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
            addLayersControl(
                baseGroups = c("OSM (default)","OSM Label","OSM Hot", "Toner", "Toner Lite"),
                options = layersControlOptions(collapsed = T))

    })
    
}

shinyApp(ui, server)

