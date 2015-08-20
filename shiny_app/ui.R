# ---
# title:  "Text Prediction Using N-Grams"
# subtitle:   "Coursera Data Science Capstone Milestone Report"
# author: "Alexander Vlasblom"
# date:   "July 2015"
# ---

# ui.R
#

source("sharedheader.R")

library(shiny)

shinyUI(fluidPage(
    titlePanel("WordPred: Text Prediction Using N-Grams"),
    sidebarLayout(
        sidebarPanel(width=4,
            p("This app predicts the next English word of a sentence, using an n-gram based text prediction model."),
            p("Start typing your sentence in the text box. When you finish typing the predicted next word is displayed immediately."),

            HTML("<br/><br/>"),
            
            selectInput("tssample", "Select training sample percentage:", tslist, multiple=FALSE, selected=tslist.selected),

            fluidRow(
                column(3, 
                       p("Training data size:", textOutput("ng.size", inline=TRUE))
                       ),
                column(9, 
                       span("Load time: ", HTML("<span style='font-size:small;'>(Please allow for up to 15 seconds)</span>"), htmlOutput("ng.loadtime"))
                       )
            ),
            #p("Size:", textOutput("ng.size", inline = TRUE)),
            #span("Load time (sec):", htmlOutput("ng.loadtime")),
            
            HTML("<br/>"), p("Prediction time:", textOutput("ng.predtime", inline=TRUE)),
            
            HTML("<br/>"),
            
            radioButtons("showcandidates", "Show candidate words:", c("Yes", "No"), selected="No", inline = TRUE),
            
            HTML("<br/><br/>"),
            
            p("Training set data:", textOutput("ngrams.info1", inline=TRUE)),
            
            tableOutput("ngrams.info2")        
        ),
        mainPanel(width = 8,
                  column(5, 
                         HTML("<br>"),
                         p("Enter your text:", style="margin-bottom:-10px;"),
                         textInput("inputtext", label="", value=""),
                         
                         HTML("<br>"),
                         
                         p("You typed:"),
                         #HTML("<div style='padding-left:10px; height:30px;'>"), 
                         HTML("<em>"), textOutput("inputecho"), HTML("</em>"),
                         #HTML("</div>"),
                         
                         HTML("<br>"),
                         
                         p("The suggested next word is:"),
                         #HTML("<div style='padding-left:10px; height:30px; font-weight:bold;'"), 
                         strong(textOutput("prediction")), 
                         #HTML("</div>"), 
                         
                         HTML("<br><br>"),
                         
                         conditionalPanel(condition = "input.showcandidates == 'Yes'",
                                          p("Cloud of candidate words (blue = 4-gram, green = 3-gram, red = 2-gram, black = unigram, relative size represents likelihood):"),
                                          plotOutput("ngrams.info3", height=200)
                         )
                  ),
                  column(6, offset=1, 
                       fluidRow(
                           conditionalPanel(condition = "input.showcandidates == 'Yes'", 
                                            fluidRow(
                                                p("As selected from the following candidates:"),
                                                tableOutput("candidates")
                                            )
                           )
                       )
                  )
        )
    ) #sidebarLayout
)) # fluidPage shinyUI

