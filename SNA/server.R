

options(scipen = 50)

## function to get names and gender
getNames <- function(){
    
    names <- read.csv('names.csv',stringsAsFactors = F)
    names <- data.frame(names)
    names$first.name <- iconv(names$first.name,"WINDOWS-1252","UTF-8")
    names <- names %>% group_by(first.name) %>% mutate(ind=row_number()) %>% filter(ind==1) %>% select(-ind) %>% na.omit()
    
    return(names)
    
}


## function to get users
getUsers <- function(){
    
    
    users <- read.csv('users.csv')
    users <- users %>% filter(!name %in% c('','name'))
    users$name <- iconv(users$name,"WINDOWS-1252","UTF-8")
    users$name <- gsub('[[:punct:]]','',users$name)
    
    return(users)
}

## function to get gender from name
getGender <- function(df){
    
    cols <- data.frame(g=c('female','leader','male','unknown')
                       ,id=c(1,2,3,4)
                       ,col=c('#ff00ff','#ffff00','#00ffff','#d3d3d3')
                       ,stringsAsFactors = F
    )
    
    users$name <- iconv(users$name,"WINDOWS-1252","UTF-8")
    users$name <- gsub('[[:punct:]]','',users$name)
    
    df <- left_join(df,users,by=c('target_user_id'='user_id')) %>% mutate(recipent_name=name)
    
    df <- data.frame(user_id=c(df$source_user_id,df$target_user_id),names=c(df$sender_name,df$recipent_name)) %>% unique
    df$names <- iconv(df$names,"WINDOWS-1252","UTF-8")
    
    names$first.name <- tolower(names$first.name)
    df$names <- tolower(df$names)
    df$names <- gsub('[[:punct:]]','',df$names)
    df$full_name <- gsub('[[:punct:]]','',df$names)
    df$names <- gsub(' .*','',df$names)
    
    df <- left_join(df,names,by=c('names'='first.name'))
    df$gender[is.na(df$gender)] <- 'unknown'
    df <- left_join(df,cols,by=c('gender'='g'))
    
    df <- unique(df)
    
    df <- df %>% 
        arrange(user_id,names) %>% 
        group_by(user_id) %>% 
        mutate(ind=row_number()) %>% 
        data.frame() %>% 
        filter(ind==1) %>% 
        select(-ind) %>% 
        mutate(names=ifelse(is.na(names),'unknown',names)
               ,full_name=ifelse(is.na(full_name),'unknown',full_name)
        )
    
    return(df)
}

## load libraries
library(shiny)
library(dplyr)
library(shinythemes)
library(networkD3)

## get names and users 
names <- getNames()
users <- getUsers()



## get data
tweet_tags <- read.csv('tweet_tags.csv')
tweet_mentions <- read.csv('tweet_mentions.csv')
tweets <- read.csv('tweets.csv')
users <- read.csv('users.csv')

## join datasets
tags <- inner_join(tweet_tags,tweet_mentions) %>% 
        inner_join(tweets) %>% 
        group_by(tag=tolower(tag),y=year(created_at),m=month(created_at),w=week(created_at)) %>% 
        summarise(num_tweets=n()) %>% 
        data.frame() %>% 
        filter(num_tweets>=10)



function(input, output, session) {
    
    
    output$month <- renderUI({
        selectInput('month'
                    ,label='Choose Month'
                    ,multiple=F
                    ,choices = sort(unique(tags[tags$y %in% input$year,"m"])))
    })
    
    output$week <- renderUI({
        selectInput('week'
                    ,label='Choose Week'
                    ,multiple=F
                    ,choices = sort(unique(tags[tags$m %in% input$month,"w"])))
    })
    
    output$tag <- renderUI({
        
        tags <- tags %>% 
            filter(y %in% input$year,m %in% input$month,w %in% input$week) %>% 
            group_by(tag) %>% 
            summarise(num_tweets=sum(num_tweets)) %>% 
            mutate(tag1=paste0(tag,' (',num_tweets,')')) %>% 
            arrange(desc(num_tweets))
        
        selectInput('tag'
                    ,label='Choose Search Term'
                    ,choices = as.character(unique(tags$tag1)))
    })
    
    
    data <- reactive({
        
        
        try(
            if(input$year!='' & input$month!='' & input$week!='' & (input$tag!='' | !is.na(input$tag))){
                
                tags <- reactiveValues()
                
                tags$tag <- gsub(' \\(+[0-9]+\\)','',input$tag)
                
                data <- inner_join(tweet_tags,tweet_mentions) %>% inner_join(tweets) 
                data <- left_join(data,users,by=c('source_user_id'='user_id'))
                data <- data %>% 
                        mutate(sender_name=tolower(name.x),recipent_name=name.y,text=tweet_text) %>% 
                        filter(year(created_at) %in% paste(input$year,collapse = ',')
                               ,month(created_at) %in% paste(input$month,collapse = ',')
                               ,week(created_at) %in% paste(input$week,collapse = ',')
                               ,tolower(tag)==tags$tag
                               ,!is.na(sender_name)
                               ,sender_name!=''
                               ) %>% 
                    select(tag,tweet_id,source_user_id,target_user_id,sender_name,recipent_name,text) %>% 
                    data.frame()                                
            }
            ,silent = T)        
        
    })
    
    
    ## output data for 
    output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        
        edgeList <- 
            data() %>% 
            select(source_user_id,target_user_id,sender_name,recipent_name) %>% 
            filter(source_user_id!=target_user_id) %>% 
            group_by(source_user_id) %>% 
            mutate(Weight=n()) %>% 
            data.frame()
        
        user_gender <- getGender(edgeList) %>% select(user_id,gender)
        
        dt <- inner_join(data(),user_gender,by=c('source_user_id'='user_id')) %>% select(tag,sender_name,gender,tweet) %>% mutate(tweet=paste0(substr(tweet,1,100),'...'))
        
        print(tail(dt, input$maxrows))
        options(orig)
    })
    
    ## download file
    output$downloadCsv <- downloadHandler(
        filename = "tweets_data.csv",
        content = function(file) {
            
            edgeList <- 
                data() %>% 
                select(source_user_id,target_user_id,sender_name,recipent_name) %>% 
                filter(source_user_id!=target_user_id) %>% 
                group_by(source_user_id) %>% 
                mutate(Weight=n()) %>% 
                data.frame()
            
            user_gender <- getGender(edgeList) %>% select(user_id,gender)
            
            dt <- inner_join(data(),user_gender,by=c('source_user_id'='user_id')) %>% select(tag,sender_name,gender,source_user_id,target_user_id,tweet_id,tweet)
            
            write.csv(dt, file)
        },
        contentType = "text/csv"
    )
    
    
    
    ########################################################################################################
    ########################################################################################################
    ## graph
    
    output$force <- renderForceNetwork({
        
        y <- data()
        validate(need(nrow(y) > 0, ""))
        
        
        edgeList <- 
            data() %>% 
            select(source_user_id,target_user_id,sender_name,recipent_name) %>% 
            filter(source_user_id!=target_user_id) %>% 
            group_by(source_user_id) %>% 
            mutate(Weight=n()) %>% 
            data.frame()
        
        names_df <- getGender(edgeList) 
        
        edgeList <- edgeList %>% select(-sender_name,-recipent_name)
        colnames(edgeList) <- c("SourceName", "TargetName", "Weight")
        
        # create a graph. use simplyfy to ensure that there are no duplicated edges or self loops
        gD <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=FALSE))
        
        # create a node list object (actually a data frame object) that will contain information about nodes
        nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)), # because networkD3 library requires IDs to start at 0
                               nName = igraph::V(gD)$name)
        
        # map node names from the edge list to node IDs
        getNodeID <- function(x){
            which(x == igraph::V(gD)$name) - 1 # to ensure that IDs start at 0
        }
        #  add them to the edge list
        edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"), 
                                function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                        TargetID = getNodeID(x$TargetName)))
        
        # calculate degree for all nodes
        nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))
        
        # calculate betweenness for all nodes
        betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
        betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
        nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
        rm(betAll, betAll.norm)
        
        # calculate dice similarities between all pairs of nodes
        dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")
        
        F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
        edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"), 
                                function(x) data.frame(F1(x)))
        
        rm(dsAll, F1, getNodeID, gD)
        
        F2 <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = nrow(edgeList), space = "rgb", interpolate = "linear")
        
        colCodes <- F2(length(unique(edgeList$diceSim)))
        edges_col <- sapply(edgeList$diceSim, function(x) colCodes[which(sort(unique(edgeList$diceSim)) == x)])
        
        rm(colCodes, F2)
        
        ############################################################################################
        
        nodeList$nName <- as.numeric(as.character(nodeList$nName))
        edgeList_short <- edgeList %>% select(SourceName,Weight) %>% unique
        
        ## join nodeList with edgeList_short and change nodeBetweenness according to Weight
        nodeList <- left_join(nodeList,edgeList_short,by=c('nName'='SourceName')) %>% 
            mutate(Weight=ifelse(is.na(Weight),1,Weight+1)
                   ,nodeBetweenness=(Weight - min(Weight))/(max(Weight) - min(Weight))
                   ,nodeBetweenness=ifelse(nodeBetweenness==0,0.01,nodeBetweenness)*100
            )
        
        
        
        ## join nodeList with names_df
        nodeList <- left_join(nodeList,names_df,by=c('nName'='user_id')) 
        
        ## change user id to name
        nodeList <- nodeList %>% mutate(nName=ifelse(is.na(full_name),'',full_name))
        
        ## set leader 
        nodeList <- nodeList %>% mutate(id=ifelse(Weight>5,2,id))
        
        ## replace nodeDegree with id
        nodeList$nodeDegree <- as.character(nodeList$id)
        
        ## plot
        D3_network_LM <- networkD3::forceNetwork(Links = edgeList, # data frame that contains info about edges
                                                 Nodes = nodeList, # data frame that contains info about nodes
                                                 Source = "SourceID", # ID of source node 
                                                 Target = "TargetID", # ID of target node
                                                 Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                                 NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                                 Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
                                                 Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
                                                 height = 5000, # Size of the plot (vertical)
                                                 width = 5000,  # Size of the plot (horizontal)
                                                 fontSize = 30, # Font size
                                                 linkDistance = networkD3::JS("function(d) { return 50*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                                 linkWidth = networkD3::JS("function(d) { return d.value/3; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                                 opacity = 0.95, # opacity
                                                 zoom = TRUE, # ability to zoom when click on the node
                                                 opacityNoHover = 0.05, # opacity of labels when static
                                                 linkColour = edges_col, # edge colors
                                                 colourScale = JS('d3.scaleOrdinal().domain(["1","2","3","4"]).range(["pink","orange","blue","grey"]);' )
                                                 
        )
        
    })
    
    
}





