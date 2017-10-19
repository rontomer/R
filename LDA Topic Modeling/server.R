
library(shiny)
library(dplyr)




shinyServer(function(input, output,session) {
    
   
    
    
    
    output$plot <- renderVis({
    # json_plot <- reactive({
    
        
        library(stringr) ## str_split()
        library(tm) 
        library(dplyr)
        
        # if(is.null(input$file1)) stop('Please upload file')
        # if(input$K<1) stop('Numnber of clusters should be bigger than 1')
        
        # inFile <- input$file1
        
        # ratings_data <- read.csv(inFile$datapath)
        ratings_data <- read.csv('my_reviews.csv')
        names(ratings_data) <- 'title'
        
        progress <- Progress$new(session, min=1, max=15)
        on.exit(progress$close())
        
        progress$set(message = 'Model in progress\n',
                     detail = 'This can take up to 2 minutes...')
        
        
        ## format text
        ratings_data$title <- iconv(ratings_data$title,"WINDOWS-1252","UTF-8") ## run this before tolower() inorder to solve Error in tolower() invalid multibyte string
        ratings_data <- tolower(ratings_data$title)
        ratings_data <- gsub('\\d+', '', ratings_data)
        ratings_data <- gsub('[[:punct:]]', '', ratings_data)
        ratings_data <- gsub('[[:cntrl:]]', '', ratings_data)
        ratings_data <- gsub("^[[:space:]]+", "", ratings_data) # remove whitespace at beginning of documents
        ratings_data <- gsub("[[:space:]]+$", "", ratings_data)
        
        
        
        ########################################################################################################################################
        ## LDA
        ########################################################################################################################################
        
        # devtools::install_github("cpsievert/LDAvisData")
        library(LDAvisData)
        library(tm)
        # install.packages("lda")
        library(lda)
        # install.packages("LDAvis")
        library(LDAvis) ## http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
        
        doc.list <- strsplit(ratings_data, "[[:space:]]+")
        term.table <- table(unlist(doc.list))
        term.table <- sort(term.table, decreasing = TRUE)
        
        stop_words <- stopwords("SMART") 
        del <- names(term.table) %in% stop_words | term.table < 5 
        term.table <- term.table[!del]
        term.table <- term.table[nchar(names(term.table))>2]
        term.table <- term.table[nchar(names(term.table))<20]
        vocab <- names(term.table)
        
        get.terms <- function(x) {
            index <- match(x, vocab)
            index <- index[!is.na(index)]
            rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
        }
        documents <- lapply(doc.list, get.terms)
        
        D <- length(documents)  
        W <- length(vocab)  
        doc.length <- sapply(documents, function(x) sum(x[2, ]))  
        N <- sum(doc.length)  
        term.frequency <- as.integer(term.table) 
        
        K <- input$K
        G <- 2500
        alpha <- 50/K
        eta <- 0.1 
        
        
        # Fit the model:
        library(lda)
        set.seed(357)
        fit <- lda.collapsed.gibbs.sampler(documents = documents, 
                                           K = K, 
                                           vocab = vocab, 
                                           num.iterations = G, 
                                           alpha = alpha, 
                                           eta = eta, 
                                           initial = NULL, 
                                           burnin = 0,
                                           compute.log.likelihood = TRUE)
        
        
        theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x))) ## docs topics probs 
        phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))) ## terms topics probs
        
        intent.ratings_data <- list(phi = phi,
                                    theta = theta,
                                    doc.length = doc.length,
                                    vocab = vocab,
                                    term.frequency = term.frequency)
        
        
        # create the JSON object to feed the visualization:
        json <- createJSON(phi = intent.ratings_data$phi, 
                           theta = intent.ratings_data$theta, 
                           doc.length = intent.ratings_data$doc.length, 
                           vocab = intent.ratings_data$vocab, 
                           term.frequency = intent.ratings_data$term.frequency)
        
        
    })
    
    
})


