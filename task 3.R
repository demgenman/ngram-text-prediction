# Alexander Vlasblom
# Prepare model data (functions)
# 2015-07-16

# split list of n-grams in predictor-tresponse tuples. List item n is a data frame of n-grams. 
# The response is the final word in the n-gram, the predictor is formed
# Returns the list of data frames with columns xgram (for n > 1) and predict, the predictor word(s) and response word.

splitNgrams <- function(ng) {
    dmessage("splitNgrams")
    for (n in 1:length(ng)) {
        if (class(ng[[n]]) == "data.frame") {
            if (n == 1) {
                colnames(ng[[n]])[which(colnames(ng[[n]]) == "ngram")] <- "predict"
            }
            else {
                xgram.pattern <- paste0("^(\\w+( +\\w+){", n-2, "}) +\\w+ *$")
                ng[[n]][,"xgram"] <- sub(xgram.pattern, "\\1", ng[[n]][,"ngram"])
                predict.pattern <- "^.* +(\\w+) *$"
                ng[[n]][,"predict"] <- sub(predict.pattern, "\\1", ng[[n]][,"ngram"])
            }
        }
    }
    dprint(str(ng))
    ng
}

# Replace control and punctuation characters in an input text by space. Single quotes are removed.
# Returns cleaned input text.
cleanInput <- function(inputText) {
    dmessage("cleanInput")
    # remove single quotes, don't ==> dont
    inputText <- gsub("'", "", tolower(inputText), fixed=TRUE)
    # remove non-word charactters
    charsToClean <- c("[:cntrl:]", "[:punct:]")
    for (i in 1:length(charsToClean))
        inputText <- gsub(paste0("[", charsToClean[i], "]"),  " ", inputText, fixed=FALSE)
    inputText
}

# Break an input text in x-grams, i.e. the last n-1 words of the corresponding n-grams.
# Returns list of x-grams.
makeXgrams <- function(inputText, ngramRange=1:4) {
    dmessage("makeXgrams")
    inputText <- cleanInput(inputText)
    xgramRange <- (ngramRange - 1)
    xgramRange <- xgramRange[xgramRange > 0]
    xgrams <- lapply(xgramRange, function(n) {
        if (n > 0) {
            # xgram regex matches 1 .. n-1 words
            xgram <- regmatches(inputText, regexec(paste0("^.*\\b(", paste(rep("\\w+ +", n-1), collapse=""), "\\w+) *$"), inputText))
            dmessage("xgram: ", xgram)
            # drop first element, which contains the entire inputText, keeping only the n words part
            xgram[[1]][-1]
        }
        else character(0)
    })
    xgrams
}

# Predict the next words for all x-grams in an input text, based on a given set of n-grams, n in range 1:4
# Returns best candidates for the next word, as a data frame with columns predict (the predicted word) and total.freq (the observed
# total nr of occurrences of the n-grams predicting the best candidates)
predictN <- function(inputText, ngrams, ngramRange=1:4, order="freq", reorderThreshold=0) {
    # inputText = predictor text
    # ngrams = ordered list of ngrams, n-th item in list is data frame of n-grams (columns ngram and total.freq)

    dmessage("predictN")

    # Predict using longest ending ngram, if no match fall back to longest ngram minus 1, etc.
    # If 2-gram fails, predict single word with highest probability in directionary.
    
    xgrams <- makeXgrams(inputText, ngramRange)
    dprint(xgrams)
    
    # apply xgrams in reverse order (highest n first) to find potential candidates
    candidates <- ldply(length(xgrams):1, function(x) {
        # lookup x in n+1-grams
        ng <- ngrams[[x+1]]
        if (order=="ml") ng <- ng[order(-ng$ml),]
        cands <- head(ng[ng$xgram == xgrams[[x]], c("predict", "freq", "ml")])
        cands$n <- rep(x+1, nrow(cands))
        cands
    })

    # if no candidates found using higher-level ngrams, use 1-gram
    if (nrow(candidates) == 0) {
        candidates <- head(ngrams[[1]])
        candidates$n <- 1
    }
    # remove duplicates
    # because of the data frame's descending order of frequency, 
    # duplicated() will remove the lower frequency duplicated items
    candidates[,"dup"] <- ifelse(duplicated(candidates$predict), TRUE, FALSE) 
    
    # reorder by global frequency
    # if digits(max(total.freq)) for highest n-gram is within threshold digits(max(total.freq)) for lower order n-grams
    freq.max <- sapply(2:3, function(n) max(candidates[candidates$n == n, "freq"]))
    if (min(nchar(freq.max)) + reorderThreshold >= max(nchar(freq.max)))
        candidates <- candidates[order(-candidates$freq),]
    
    dprint(str(candidates))
    dprint(candidates)

    candidates
}

