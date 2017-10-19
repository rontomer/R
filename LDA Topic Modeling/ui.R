
## load libraries
library(shiny)
library(ggplot2)
library(shinythemes)
library(LDAvis)

shinyUI(
    
    fluidPage(
        
        theme = shinytheme("flatly"),
        # theme = shinytheme("united"),
        
        titlePanel(title=div(img(src="picture.png"), "Topic Modeling Using LDA")),
        
        # headerPanel("Topic Modeling Using LDA"),
        
        sidebarPanel(
            
            wellPanel(
                
                helpText(HTML("<b>ABOUT</b>")),
                helpText(HTML("This simulation tool aims to analyze free text in order to find group of topics<br><br>
                               <b>Instructions:</b>: <br>    
                               <ul>
                                  <li><i><font color='red'>Upload file:</i></font></li> csv/text file without header<br>                   
                                  <li><i><font color='red'>Number of clusters:</i></font></li> number of topics you wish to find. Defualt is 10<br>                   
                                  <li><i><font color='red'>Lambda (λ):</i></font></li> please set this value to be 0.6 once plot is active<br>                   
                              </ul><br>
                              <b>How to interpret clusters?</b><br>
                                 LDA is an unsupervised learning method that maximizes the probability of word assignments to one of K fixed topics.<br> 
                                    The topic meaning is extracted by interpreting the top N probability words for a given topic.<br> 
                                    LDA will not output the meaning of topics, rather it will organize words by topic to be interpreted by the user<br><br>  
                                    For example, words like  <i>fast</i>, <i>quick</i>, <i>response</i>, <i>delivery</i> will probably decsribe cluster of good ratings with sellers responding/delivering fast<br>
                                    Words like <i>cancel</i>, <i>failed</i>, <i>delivered</i>, <i>time</i> will probably decsribe cluster of cancellation due to sellers not delivering on time
                              
                              <br><br>"))
                
            ),
            
            fileInput('file1', 'upload file',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv'))
            ,helpText("Note: Please upload CSV of Text file. File should contian one column only with free text. Any language is supported")
            
            ,numericInput("K", "Clusters", 10)
            ,helpText("Note: Clusters value should be bigger than 1. Recommended starting value is 10")
            
            ,submitButton("Update View")
            
            ,width=3)
        
        # ,mainPanel(
            # img(src='picture.png', align = "right")
        #     ### the rest of your code
        # )
        # ,htmlOutput("picture")
        
            ,visOutput('plot')
            
        
    )
    
)
