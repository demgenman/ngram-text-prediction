# task 2c.R

# TO DO:
# line 182, investigate why ml = NA if n = 2 ???
# line 104, two variants: with stop words and without stop words
# export png size 1024 x 800 for good resolution

# Alexander Vlasblom
# Explore and analyze data (functions)
# 2015-07-16

library(tm)
library(RCurl)
library(XML)
library(RWeka)
library(plyr)
library(qdap)
library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)

# global settings
dirData <- "./data/final/en_US"
filesFinal <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
scenarioRange <- c(0.01, 0.05, 0.1, 0.2)
mydebug <- FALSE

# debug print functions
dmessage <- function(...) if (mydebug) message(...)
dprint <- function(...) if (mydebug) print(...)

# read datasets
# returns list of dataset name and content converted to ascii
read.datasets <- function(dirData, filesFinal) {
    dmessage("read.datasets")
    lapply(sprintf("%s/%s", dirData, filesFinal), function(p) {
        dmessage(p)
        system(paste("wc -l", p))
        list(path=p, content=iconv(readLines(p), "UTF-8", "ASCII"))
    })
}

# build corpus
# returns corpus
makeCorpus <- function(corpustexts) {
    # docs.corpus <- Corpus(DirSource(dirData, pattern="\\.txt$"), readerControl=list(reader=readPlain, language="lat"))
    
    corpustexts <- sent_detect(corpustexts)
    docs.corpus <- Corpus(VectorSource(corpustexts))
    
    # cleanup text
    docs.corpus <- tm_map(docs.corpus, stripWhitespace)
    docs.corpus <- tm_map(docs.corpus, removePunctuation)
    docs.corpus <- tm_map(docs.corpus, removeNumbers)
    docs.corpus <- tm_map(docs.corpus, tolower)
    
    # remove profanity words
    pWordLists <- list("http://www.bannedwordlist.com/lists/swearWords.txt", 
                       "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt")
    profanityWords <- ldply(pWordLists, function(pwl.url) {
        pwl.file <- paste0("./data/", gsub("^.*/", "", pwl.url))
        if (!file.exists(pwl.file)) download.file(pwl.url, pwl.file)
        data.frame(word=readLines(pwl.file))
    })
    profanityWords <- profanityWords[!duplicated(profanityWords),]
    
    docs.corpus <- tm_map(docs.corpus, removeWords, profanityWords)    
}

# build document term matrix
# returns document term matrix
makeDtm <- function(docs.corpus, ngram, ...) {
    if (ngram == 1)
        dtm <- DocumentTermMatrix(docs.corpus, ...)
    else {
        ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngram, max = ngram))
        dtm <- DocumentTermMatrix(docs.corpus, control=list(tokenize=ngramTokenizer))        
    }
    dtm
}

# frequency sorted document term matrix
# returns data frame of ngram and freq
freqDtm <- function(dtm) {
    freq <- data.frame(freq=sort(colSums(as.matrix(dtm)), decreasing=T))
    freq$ngram <- rownames(freq); rownames(freq) <- NULL
    freq[,c(2,1)]
}

# extract n-grams from a training dataset for all n in range 1:4
# returns list of n-grams
makeNgrams <- function(alltext.train) {
    # Process text in chunks to avoid memory allocation limits
    # Note: this can take a few minutes, depending on nr of links of data in the Corpus
    chunkSize <- 1000
    ngramRange <- 1:4
    
    dmessage("Lines of data: ", length(alltext.train))
    dmessage("Chunk size: ", chunkSize)
    dmessage("Ngram range: ", paste(ngramRange, collapse=" "))
    
    ngramChunks <- lapply(0:as.integer(length(alltext.train) / chunkSize), function(chunk) {
        cat(sprintf("%d ", chunk))
        alltext.chunk <- alltext.train[(chunk * chunkSize + 1):min(length(alltext.train), (chunk + 1) * chunkSize)]
        # get n-grams
        docs.corpus <- makeCorpus(alltext.chunk)
        freq <- lapply(ngramRange, 
                       function(n) freqDtm(makeDtm(docs.corpus, n, control = list(stopwords=stopwords(kind="en")))))
    })
    
    # combine ngram frequency counts
    # method 1) ngrams <- lapply(ngramRange, function(ngram.nr) --> memory intensive, fails to complete at n=3
    #   for a sample size of 20%
    # method 2) for loop
    #   for (ngram.nr in 1:4) { compute ngram; save to ngram file}
    #   ngrams <- lapply(ngramRange, function(ngram.nr) { load(ngram file) }
    
    mergeMethod <- 2
    if (mergeMethod == 1) {
        ngrams <- lapply(ngramRange, function(ngram.nr) {
            # this is a memory-critical step
            # method a) do.call("rbind.fill", list of df)
            # method b) do.call("rbind", list of df) - should be fine, because colnames are all identical
            # method c) see http://stackoverflow.com/questions/7093984/memory-efficient-alternative-to-rbind-in-place-rbind
            #         nextrow = nrow(df)+1
            #         df[nextrow:(nextrow+nrow(df.extension)-1),] = df.extension
            #         # we need to assure unique row names
            #         row.names(df) = 1:nrow(df)
            dmessage("Merge dtm chunks started: ", Sys.time())
            ngram.freq <- do.call("rbind", lapply(1:length(ngramChunks), function(chunknr) {
                ngramChunks[[chunknr]][[ngram.nr]]
            }))
            dmessage("Dtm chunks merged: ", Sys.time())
            dmessage("Summarize freq")
            ngram.freq <- ddply(ngram.freq, .(ngram), summarize, total.freq=sum(freq), .progress="text")
            colnames(ngram.freq)[which(colnames(ngram.freq) == "total.freq")] <- "freq"
            ngram.freq <- ngram.freq[order(-ngram.freq$freq),]
            save(ngram.freq, file=sprintf("./data/ngram.freq %d.RData", ngram.nr))
        })
    } else {
        ngramTempFile <- "./data/ngram.freq %d.RData"
        for (ngram.nr in ngramRange) {
            dmessage("Merge dtm chunks started: ", Sys.time())
            ngram.freq <- do.call("rbind", lapply(1:length(ngramChunks), function(chunknr) {
                ngramChunks[[chunknr]][[ngram.nr]]
            }))
            dmessage("Dtm chunks merged: ", Sys.time())
            dmessage("Summarize freq")
            ngram.freq <- ddply(ngram.freq, .(ngram), summarize, total.freq=sum(freq), .progress="text")
            colnames(ngram.freq)[which(colnames(ngram.freq) == "total.freq")] <- "freq"
            ngram.freq <- ngram.freq[order(-ngram.freq$freq),]
            pathNgramTempFile <- sprintf(ngramTempFile, ngram.nr)
            dmessage("Save temp: ", Sys.time(), " ", pathNgramTempFile)
            save(ngram.freq, file=pathNgramTempFile) 
            rm(ngram.freq);gc()
        }
        ngrams <- lapply(ngramRange, function(ngram.nr) {
            pathNgramTempFile <- sprintf(ngramTempFile, ngram.nr)
            dmessage("Load temp: ", Sys.time(), " ", pathNgramTempFile)
            load(file = pathNgramTempFile)
            dprint(dim(ngram.freq))
            ngram.freq
        })
    }
    
    # the result has N list elements:
    # [[1]] = 1-gram, [[2]] = 2-gram, [[3]] = 3-gram, ... [[n]] = n-gram

    ngrams
}

# build training scenario from dataset sample with specific sample size
# returns n-grams in training scenario
makeScenario <- function(trainSample) {
    dmessage("makeScenario training sample as % of total data: ", trainSample)

    datasets <- read.datasets(dirData, filesFinal)
    
    set.seed(2345)
    inTrain <- lapply(datasets, function(ds) {
        sample(1:length(ds[[2]]), length(ds[[2]]) * trainSample)
    })
    
    # Build corpus of training set
    alltext.train <- unlist(mapply(datasets, inTrain, FUN = function(ds, train) {
        # use [train] to select training set, use [-train] to select test set
        ds[[2]][train]
    }))
    
    alltext.test <- unlist(mapply(datasets, inTrain, FUN = function(ds, train) {
        # use [train] to select training set, use [-train] to select test set
        ds[[2]][-train]
    }))
    pathTest <- sprintf("./data/alltext.test %.2f.RData", trainSample * 100)
    dmessage("Save test dataset: ", pathTest)
    save(alltext.test, file=pathTest)

    # unload unused variables from memory
    rm(datasets); gc()
    
    ngrams <- makeNgrams(alltext.train)

    # Calculation of maximum likelihood for last word = count(n-gram) / count(x-gram)
    # Step 1 - split ngram in xgram + last word
    for (n in 1:length(ngrams)) {
        df <- ngrams[[n]]
        if (n == 1) {
            df$predict <- df$ngram
        } else {
            xgram.pattern <- paste0("^(\\w+( +\\w+){", n-2, "}) +\\w+ *$")
            df$xgram <- sub(xgram.pattern, "\\1", df$ngram)
            predict.pattern <- "^.* +(\\w+) *$"
            df$predict <- sub(predict.pattern, "\\1", df$ngram)        
        }
        ngrams[[n]] <- df
    }    
    # Step 2 - calculate ML
    for (n in 1:length(ngrams)) {
        if (class(ngrams[[n]]) == "data.frame") {
            df <- ngrams[[n]]
            if (n == 1) {
                df[,"ml"] <- df$freq / sum(df$freq)
            } else {
                dfx <- ngrams[[n-1]]
                df <- merge(df, dfx[,c("ngram", "freq")], by.x="xgram", by.y="ngram", all.x=TRUE, all.y=FALSE)
                colnames(df) [which(colnames(df) == "freq.x")] <- "freq"
                df[,"ml"] <- df$freq / df$freq.y
                df <- df[order(-df$freq, -df$ml),] 
            }
            ngrams[[n]] <- df
        }
    }    
    
    # [[n+1]] = training set selection info (selected line numbers for training set)
    ngrams$selection = inTrain
    # [[n+2]] = training scenario reference
    ngrams$scenario <- sprintf("Train %.2f", trainSample)
    
    ngrams
}

# load training scenario
# returns n-grams in training scenario
loadScenarios <- function(trainSample) {
    dmessage("loadScenarios: ", trainSample)
    pathNgrams <- sprintf("./data/ngrams %.2f.RData", trainSample * 100)
    if (!file.exists(pathNgrams)) {
        dmessage("Generate ngrams: ", pathNgrams)
        ngrams <- makeScenario(trainSample)
        dmessage("Save ngrams: ", pathNgrams)
        save(ngrams, file=pathNgrams)
    }    
    else {
        dmessage("Load ngrams: ", pathNgrams)
        load(pathNgrams)
    }
    ngrams
}

# compute scenario statistics
# returns a data frame containing, for each scenario, per n, the nr of unique n-grams in the scenario, and the nr of n-gram instances.
stats.scenarios <- function(training.scenarios) {
    # print stats for training dataset scenarios 
    ngramRange <- 1:4
    scenario.stats <- ldply(training.scenarios, function(ngrams) {
        cbind(scenario = ngrams$scenario, ldply(ngramRange, function(n) {
            cbind(ngram = n, data.frame(freq.table.length = nrow(ngrams[[n]]),
                                        total.word.instances = sum(ngrams[[n]][,"freq"])))
        }))
    })
    scenario.stats <- scenario.stats[order(scenario.stats$scenario, scenario.stats$ngram),]
    dprint(scenario.stats)
    scenario.stats
}

# compute language coverage statistics
# returns data frame containing, for each training scenario, per n, the nr of unique n-grams to achieve a given percentage of coverage.
stats.wordcoverage <- function(training.scenarios) {
    ngramRange <- 1:4
    coverageRange <- c(0.5, 0.6, 0.7, 0.8, 0.85, 0.875, 0.9)
    # word coverage stats
    word.coverage <- ldply(ngramRange, function(n) {
        ldply(coverageRange, function(coverage) {
            data.frame(ngram=n, coverage=coverage, t(unlist(lapply(training.scenarios, function(ngrams) {
                freq <- ngrams[[n]][,"freq"]
                totalWords <- sum(freq)
                cumulWords <- cumsum(freq)
                words <- sum(cumulWords <= coverage * totalWords)
                words
            }))))        
        })
    })
    colnames(word.coverage)[-(1:2)] <- make.names(paste("Train", scenarioRange))
    dprint(word.coverage)
    word.coverage
}

