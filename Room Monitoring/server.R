
## load libraries
library(shiny)
library(dplyr)
library(reshape2)
library(dygraphs)
library(xts)
library(shinythemes)

function(input, output, session) {

    ## generate data
    dates <- rep(seq(Sys.Date()-60,length.out = 60,by='day'),27)
    avg_co2 <- rnorm(60*27,700,150)
    avg_temp <- rnorm(60*27,20,5)
    avg_humidity <- rnorm(60*27,30,10)
    room <- rep(1:3,each=60)
    building <- rep(1:3,each=180)
    company <- rep(1:3,each=540)
    data <- data.frame(date=dates,company,building,room,avg_co2,avg_temp,avg_humidity)
    
    
    ## encode data to support german
    data$building <- iconv(data$building,"WINDOWS-1252","UTF-8")
    
    output$building <- renderUI({
        selectInput('building'
                    ,label='Choose building'
                    ,choices = as.character(unique(data[data$company==input$company,"building"])))
    })
    
    output$room <- renderUI({
        selectInput('room'
                    ,label='Choose room'
                    ,choices = as.character(unique(data[data$building==input$building,"room"])))
    })
    
    raw_data <- reactive({
        
        tmp <- 
            data %>% 
            filter(date>=as.Date(input$dateRange[1])
                   ,date<=as.Date(input$dateRange[2])
                   ,company==input$company
                   ,building==input$building
                   ,room==input$room
            ) %>% 
            melt(id=c('company','building','room','date')) %>% 
            mutate(date=as.Date(date))
        
    }) 
    
    raw_data_no_room <- reactive({
        
        tmp <- 
            data %>% 
            filter(date>=as.Date(input$dateRange[1])
                   ,date<=as.Date(input$dateRange[2])
                   ,company==input$company
                   ,building==input$building
            ) %>% 
            group_by(date,building,company) %>%
            summarise(avg_co2=mean(avg_co2)
                      ,avg_temp=mean(avg_temp)
                      ,avg_humidity=mean(avg_humidity)) %>%
            data.frame() %>%
            melt(id=c('company','building','date')) %>% 
            mutate(date=as.Date(date))
        
    }) 
    
    raw_data_company_only <- reactive({
        
        tmp <- 
            data %>% 
            filter(date>=as.Date(input$dateRange[1])
                   ,date<=as.Date(input$dateRange[2])
                   ,company==input$company
            ) %>% 
            group_by(date,company) %>%
            summarise(avg_co2=mean(avg_co2)
                      ,avg_temp=mean(avg_temp)
                      ,avg_humidity=mean(avg_humidity)) %>%
            data.frame() %>%
            melt(id=c('company','date')) %>% 
            mutate(date=as.Date(date))
        
    }) 
    
    
    raw_data1 <- reactive({
        
        tmp1 <- 
            data %>% 
            filter(date>=as.Date(input$dateRange[1])
                   ,date<=as.Date(input$dateRange[2])
                   ,company==input$company
                   ,building==input$building
                   ,room==input$room
            ) %>% 
            mutate(date=as.Date(date))
        
    })    
    
    raw_data_summary <- reactive({
        
        tmp2 <- 
            data %>% 
            filter(date>=as.Date(input$dateRange[1])
                   ,date<=as.Date(input$dateRange[2])
                   ,company==input$company
                   ,building==input$building
                   ,room==input$room
            ) %>% 
            group_by(company,building,room) %>% 
            summarise(avg_co2=mean(avg_co2)
                      ,avg_temp=mean(avg_temp)
                      ,avg_humidity=mean(avg_humidity)) %>% 
            data.frame()
        
    })   
    
    output$co2 <- renderUI({
        
        y <- raw_data_summary()
        validate(need(nrow(y) > 0, ""))
        
        val1 <- abs(as.numeric(difftime(input$dateRange[1],input$dateRange[2],units = 'day')))
        val <- raw_data_summary() %>% select(avg_co2) %>% round(1)
        vall <- paste0(val,' ppm')
        
        
        valueBox(
            value = vall,
            subtitle = paste("CO2 (Avg last",val1,"days)"),
            icon = icon("area-chart"),
            color = if (val[1] < 750 ) "green" else "red"
            
        )
    })
    
    output$temp <- renderUI({
        
        y <- raw_data_summary()
        validate(need(nrow(y) > 0, ""))
        
        val1 <- abs(as.numeric(difftime(input$dateRange[1],input$dateRange[2],units = 'day')))
        val <- raw_data_summary() %>% select(avg_temp) %>% round(1)
        vall <- paste0(val,'°C')
        
        valueBox(
            value = vall,
            subtitle = paste("Temperature (Avg last",val1,"days)"),
            icon = icon("area-chart"),
            color = if (val[1] >= 18 & val[1] < 23 ) "green" else "red"
        )
    })
    
    output$humidity <- renderUI({
        
        y <- raw_data_summary()
        validate(need(nrow(y) > 0, ""))
        
        val1 <- abs(as.numeric(difftime(input$dateRange[1],input$dateRange[2],units = 'day')))
        val <- raw_data_summary() %>% select(avg_humidity) %>% round(1)
        vall <- paste0(val,'%')
        
        
        valueBox(
            value = vall,
            subtitle = paste("Humidity (Avg last",val1,"days)"),
            icon = icon("area-chart"),
            color = if (val[1] >= 30 & val[1] < 65) "green" else "red"
        )
    })
    
    
    output$co21 <- renderDygraph({
        
        y <- raw_data()
        validate(need(nrow(y) > 0, ""))
        
        df <- 
            raw_data() %>% 
            filter(variable=='avg_co2') %>%
            mutate(date=as.Date(date,'%Y-%m-%d'))
        
        df1 <- 
            raw_data_no_room() %>% 
            filter(variable=='avg_co2') %>%
            mutate(date=as.Date(date,'%Y-%m-%d'))
        
        df11 <- 
            raw_data_company_only() %>% 
            filter(variable=='avg_co2') %>%
            mutate(date=as.Date(date,'%Y-%m-%d'))
        
        df <- xts(df$value, as.Date(df$date, format='%Y-%m-%d'))
        df1 <- xts(df1$value, as.Date(df1$date, format='%Y-%m-%d'))
        df11 <- xts(df11$value, as.Date(df11$date, format='%Y-%m-%d'))
        
        if(input$adjust_graph=='Room Only'){
            
            df_room <- cbind(room_avg=df)
            
            dygraph(df_room) %>% 
                dyAxis("y", label = "ppm", valueRange = c(0, max(df_room[,1])+400)) %>% 
                dyShading(from = 750, to = max(df_room[,1])+400, axis = "y",color='#F7C6CE') %>% 
                dyShading(from = 0, to = 750, axis = "y",color='#c6efce') %>% 
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Building Only'){
            
            df_building <- cbind(building_avg=df1)
            
            dygraph(df_building) %>% 
                dyAxis("y", label = "ppm", valueRange = c(0, max(df_building[,1])+400)) %>% 
                dyShading(from = 750, to = max(df_building[,1])+400, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 0, to = 750, axis = "y",color='#c6efce') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("building_avg", drawPoints = F, color = "#a114ff",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Company Only'){
            
            df_company <- cbind(company_avg=df11)
            
            dygraph(df_company) %>% 
                dyAxis("y", label = "ppm", valueRange = c(0, max(df_company[,1])+400)) %>% 
                dyShading(from = 750, to = max(df_company[,1])+400, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 0, to = 750, axis = "y",color='#c6efce') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("company_avg", drawPoints = F, color = "#ffa114",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Room & Building'){
            
            df_room_building <- cbind(room_avg=df,building_avg=df1)
            
            dygraph(df_room_building) %>% 
                dyAxis("y", label = "ppm", valueRange = c(0, max(df_room_building[,1:2])+400)) %>% 
                dyShading(from = 750, to = max(df_room_building[,1:2])+400, axis = "y",color='#F7C6CE') %>% 
                dyShading(from = 0, to = 750, axis = "y",color='#c6efce') %>% 
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dySeries("building_avg", drawPoints = F, color = "#a114ff",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Room & Company'){
            
            df_room_company<- cbind(room_avg=df,company_avg=df11)
            
            dygraph(df_room_company) %>% 
                dyAxis("y", label = "ppm", valueRange = c(0, max(df_room_company[,1:2])+400)) %>% 
                dyShading(from = 750, to = max(df_room_company[,1:2])+400, axis = "y",color='#F7C6CE') %>% 
                dyShading(from = 0, to = 750, axis = "y",color='#c6efce') %>% 
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dySeries("company_avg", drawPoints = F, color = "#ffa114",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='All'){
            
            df_room_building_company<- cbind(room_avg=df,building_avg=df1,company_avg=df11)
            
            dygraph(df_room_building_company) %>% 
                dyAxis("y", label = "ppm", valueRange = c(0, max(df_room_building_company[,1:3])+400)) %>% 
                dyShading(from = 750, to = max(df_room_building_company[,1:3])+400, axis = "y",color='#F7C6CE') %>% 
                dyShading(from = 0, to = 750, axis = "y",color='#c6efce') %>% 
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dySeries("building_avg", drawPoints = F, color = "#a114ff",strokeWidth=2) %>%
                dySeries("company_avg", drawPoints = F, color = "#ffa114",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        }
        
        
        
        
    })
    
    
    output$temp1 <- renderDygraph({
        
        y <- raw_data()
        validate(need(nrow(y) > 0, ""))
        
        df <- 
            raw_data() %>% 
            filter(variable=='avg_temp') %>%
            mutate(date=as.Date(date,'%Y-%m-%d'))
        
        df1 <- 
            raw_data_no_room() %>% 
            filter(variable=='avg_temp') %>%
            mutate(date=as.Date(date,'%Y-%m-%d'))
        
        df11 <- 
            raw_data_company_only() %>% 
            filter(variable=='avg_temp') %>%
            mutate(date=as.Date(date,'%Y-%m-%d'))
        
        df <- xts(df$value, as.Date(df$date, format='%Y-%m-%d'))
        df1 <- xts(df1$value, as.Date(df1$date, format='%Y-%m-%d'))
        df11 <- xts(df11$value, as.Date(df11$date, format='%Y-%m-%d'))
        
        if(input$adjust_graph=='Room Only'){
            
            df_room <- cbind(room_avg=df)
            
            dygraph(df_room) %>% 
                dyAxis("y", label = "°C", valueRange = c(0, max(df_room[,1])+20)) %>% 
                dyShading(from = 23, to = max(df_room[,1])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 18, to = 23, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 18, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Building Only'){
            
            df_building <- cbind(building_avg=df1)
            
            dygraph(df_building) %>% 
                dyAxis("y", label = "°C", valueRange = c(0, max(df_building[,1])+20)) %>% 
                dyShading(from = 23, to = max(df_building[,1])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 18, to = 23, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 18, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("building_avg", drawPoints = F, color = "#a114ff",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Company Only'){
            
            df_company <- cbind(company_avg=df11)
            
            dygraph(df_company) %>% 
                dyAxis("y", label = "°C", valueRange = c(0, max(df_company[,1])+20)) %>% 
                dyShading(from = 23, to = max(df_company[,1])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 18, to = 23, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 18, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("company_avg", drawPoints = F, color = "#ffa114",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Room & Building'){
            
            df_room_building <- cbind(room_avg=df,building_avg=df1)
            
            dygraph(df_room_building) %>% 
                dyAxis("y", label = "°C", valueRange = c(0, max(df_room_building[,1:2])+20)) %>% 
                dyShading(from = 23, to = max(df_room_building[,1:2])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 18, to = 23, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 18, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dySeries("building_avg", drawPoints = F, color = "#a114ff",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Room & Company'){
            
            df_room_company<- cbind(room_avg=df,company_avg=df11)
            
            dygraph(df_room_company) %>% 
                dyAxis("y", label = "°C", valueRange = c(0, max(df_room_company[,1:2])+20)) %>% 
                dyShading(from = 23, to = max(df_room_company[,1:2])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 18, to = 23, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 18, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dySeries("company_avg", drawPoints = F, color = "#ffa114",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='All'){
            
            df_room_building_company<- cbind(room_avg=df,building_avg=df1,company_avg=df11)
            
            dygraph(df_room_building_company) %>% 
                dyAxis("y", label = "°C", valueRange = c(0, max(df_room_building_company[,1:3])+20)) %>% 
                dyShading(from = 23, to = max(df_room_building_company[,1:2])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 18, to = 23, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 18, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dySeries("building_avg", drawPoints = F, color = "#a114ff",strokeWidth=2) %>%
                dySeries("company_avg", drawPoints = F, color = "#ffa114",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        }
        
        
        
        
    })
    
    
    
    output$humidity1 <- renderDygraph({
        
        y <- raw_data()
        validate(need(nrow(y) > 0, ""))
        
        df <- 
            raw_data() %>% 
            filter(variable=='avg_humidity') %>%
            mutate(date=as.Date(date,'%Y-%m-%d'))
        
        df1 <- 
            raw_data_no_room() %>% 
            filter(variable=='avg_humidity') %>%
            mutate(date=as.Date(date,'%Y-%m-%d'))
        
        df11 <- 
            raw_data_company_only() %>% 
            filter(variable=='avg_humidity') %>%
            mutate(date=as.Date(date,'%Y-%m-%d'))
        
        df <- xts(df$value, as.Date(df$date, format='%Y-%m-%d'))
        df1 <- xts(df1$value, as.Date(df1$date, format='%Y-%m-%d'))
        df11 <- xts(df11$value, as.Date(df11$date, format='%Y-%m-%d'))
        
        if(input$adjust_graph=='Room Only'){
            
            df_room <- cbind(room_avg=df)
            
            dygraph(df_room) %>% 
                dyAxis("y", label = "%", valueRange = c(0, max(df_room[,1])+20)) %>% 
                dyShading(from = 65, to = max(df_room[,1])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 30, to = 65, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 30, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Building Only'){
            
            df_building <- cbind(building_avg=df1)
            
            dygraph(df_building) %>% 
                dyAxis("y", label = "%", valueRange = c(0, max(df_building[,1])+20)) %>% 
                dyShading(from = 65, to = max(df_building[,1])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 30, to = 65, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 30, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("building_avg", drawPoints = F, color = "#a114ff",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Company Only'){
            
            df_company <- cbind(company_avg=df11)
            
            dygraph(df_company) %>% 
                dyAxis("y", label = "%", valueRange = c(0, max(df_company[,1])+20)) %>% 
                dyShading(from = 65, to = max(df_company[,1])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 30, to = 65, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 30, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("company_avg", drawPoints = F, color = "#ffa114",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Room & Building'){
            
            df_room_building <- cbind(room_avg=df,building_avg=df1)
            
            dygraph(df_room_building) %>% 
                dyAxis("y", label = "%", valueRange = c(0, max(df_room_building[,1:2])+20)) %>% 
                dyShading(from = 65, to = max(df_room_building[,1:2])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 30, to = 65, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 30, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dySeries("building_avg", drawPoints = F, color = "#a114ff",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='Room & Company'){
            
            df_room_company<- cbind(room_avg=df,company_avg=df11)
            
            dygraph(df_room_company) %>% 
                dyAxis("y", label = "%", valueRange = c(0, max(df_room_company[,1:2])+20)) %>% 
                dyShading(from = 65, to = max(df_room_company[,1:2])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 30, to = 65, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 30, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dySeries("company_avg", drawPoints = F, color = "#ffa114",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        } else if(input$adjust_graph=='All'){
            
            df_room_building_company<- cbind(room_avg=df,building_avg=df1,company_avg=df11)
            
            dygraph(df_room_building_company) %>% 
                dyAxis("y", label = "%", valueRange = c(0, max(df_room_building_company[,1:3])+20)) %>% 
                dyShading(from = 65, to = max(df_room_building_company[,1:2])+20, axis = "y",color='#F7C6CE') %>%
                dyShading(from = 30, to = 65, axis = "y",color='#c6efce') %>%
                dyShading(from = 0, to = 30, axis = "y",color='#F7C6CE') %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                dySeries("room_avg", drawPoints = F, color = "#303030",strokeWidth=2) %>% 
                dySeries("building_avg", drawPoints = F, color = "#a114ff",strokeWidth=2) %>%
                dySeries("company_avg", drawPoints = F, color = "#ffa114",strokeWidth=2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(axisLineWidth = 2.5, fillGraph = F, drawGrid = T) 
            
        }
        
        
        
        
    })
    
    
    
    output$downloadCsv <- downloadHandler(
        filename = "data.csv",
        content = function(file) {
            write.csv(raw_data1(), file)
        },
        contentType = "text/csv"
    )
    
    output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(tail(raw_data1(), input$maxrows))
        options(orig)
    })
}

