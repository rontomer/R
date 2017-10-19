LDA is an unsupervised learning method that maximizes the probability of word assignments to one of K fixed topics.
The topic meaning is extracted by interpreting the top N probability words for a given topic.
LDA will not output the meaning of topics, rather it will organize words by topic to be interpreted by the user
Application can be found here: https://rontomer.shinyapps.io/LDA_topic_modeling/
Or run this in R, `shiny::runGitHub("Shiny", "rontomer", subdir = "LDA Topic Modeling")`
