# ---
# title:  "Text Prediction Using N-Grams"
# subtitle:   "Coursera Data Science Capstone Milestone Report"
# author: "Alexander Vlasblom"
# date:   "July 2015"
# ---

# server.R
#

source("sharedheader.R")
source("predict.R")

library(shiny)
library(plyr)
library(Rcpp)
library(RColorBrewer)
library(wordcloud)

values <- reactiveValues(load.time=NULL, pred.time=NULL)

shinyServer(function(input, output, session) {

    ng <- reactive({
         fileNgrams <- paste0("ngrams ", input$tssample, ".rds")
         validate(
             need(any(input$tssample %in% tslist), "Please select training set."),
             need(file.exists(fileNgrams), "File not found.")
         )
         message("readRDS: ", fileNgrams)
         load.time <- system.time({
             ng <- readRDS(file = fileNgrams)
         })
         values$load.time <- HTML(paste0("<table width='100%'><tr><td width='33%'>user</td><td width='33%'>system</td><td width='33%'>elapsed</td></tr>", sprintf("<tr><td>%3.2f s</td><td>%3.2f s</td><td>%3.2f s</td></tr></table>", load.time[1], load.time[2], load.time[3])))
         ng
    })

    output$ng.size <- reactive({
        if (is.null(ng)) { return(NULL) }
        ng.size <- sprintf("%.1f MB", object.size(ng())[1]/(2^20))
        ng.size
    })
    
    output$ng.loadtime <- reactive({
        values$load.time
    })
    
    output$ng.predtime <- reactive({
        values$pred.time
    })
    
    output$ngrams.info1 <- renderText({
        if (is.null(ng())) return(NULL)
        ng()$scenario
    })

    output$ngrams.info2 <- renderTable({
        if (is.null(ng())) return(NULL)
        do.call("cbind", ldply(1:4, function(n) data.frame(n=n, n.grams=nrow(ng()[[n]]))))
    }, include.rownames=FALSE)

    output$ngrams.info3 <- renderPlot({
        if (is.null(ng())) return(NULL)
        wc.data <- predictN(input$inputtext, ng(), ngramRange = 1:4)
        wc.data <- wc.data[!wc.data$dup, c("predict", "ml", "n")]
        # normalize ml value to range 1 .. 100
        wc.data$ml <- as.numeric(wc.data$ml)
        suppressWarnings(
            ml.min <- min(wc.data$ml, na.rm=TRUE)
        )
        if (ml.min == Inf) {
            # all values are missing
            wc.data$ml <- 100
        } else {
            wc.data$ml <- ifelse(is.na(wc.data$ml), ml.min, wc.data$ml)
            ml.max <- max(wc.data$ml)
            wc.data$ml <- wc.data$ml / ml.max * 100
        }
        # compress range for better readability of word cloud image
        wc.data$ml <- round(log(wc.data$ml), 0)
        #pal <- brewer.pal(4, "Set2")
        wordcloud(wc.data$predict, wc.data$ml, scale=c(2,.2), min.freq=0, max.words=Inf, 
                  random.order=FALSE, random.color=FALSE, 
                  rot.per=.1, colors=wc.data$n, ordered.colors=TRUE, use.r.layout=FALSE, fixed.asp=TRUE)
    })
    
    output$inputecho <- reactive({
        input$inputtext
    })

    candidates <- reactive({
        if (is.null(ng)) { return(NULL) }
        pred.time <- system.time({
            candidates <- predictN(input$inputtext, ng(), ngramRange = 1:4)            
        })
        values$pred.time <- HTML(sprintf("%3.2f s", pred.time[3]))
        candidates
    })
    
    output$candidates <- renderTable({
        candidates()[, c("predict", "freq", "ml", "n")]
    }
      , display = c("d", "s", "d", "f", "d"), include.rownames=FALSE
    )
    
    output$prediction <- renderText({
        candidates()[1, "predict"]
    })
})
