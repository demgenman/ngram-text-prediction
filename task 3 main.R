# prediction accuracy

rm(list=ls()); gc()

mydebug <- TRUE

source("task 2c.R")
source("task 3.R")

makeTest <- function(sentence) {
    dmessage("makeTest: ", sentence)
    sentence <- cleanInput(sentence)
    words <- unlist(strsplit(sentence, split = " +", fixed = FALSE))
    dprint(str(words))

    testXgram <- character(length(words)-1); testPredict <- words[2:length(words)]
    for (w in 1:(length(words) - 1)) {
        testXgram[w] <- ifelse(w == 1, words[w], paste(words[w-1], words[w]))
    }
    tests <- data.frame(nr=1:(length(words) - 1), xgram=testXgram, actual=testPredict)
}

makeTests <- function(alltext.test, nTests = 100) {
    inTest <- sample(1:length(alltext.test), size = nTests)
    tests <- ldply(inTest, function(i) {
        cbind(i=i, makeTest(alltext.test[i]))
    })
}

validate <- function(trainSample = 0.01, nTest = 10) {
    message("validate: trainSample=", as.numeric(trainSample), ", nTest=", nTest)
    # load training scenario
    pathTest <- sprintf("./data/alltext.test %.2f.RData", trainSample * 100)
    dmessage("load: ", pathTest)
    load(pathTest)
    # build tests
    tests <- makeTests(alltext.test, nTest)
    dprint(str(tests))
    # initialize  prediction
    # ngrams must be put in the global env for ddply to work inside the scope of this function definition
    # see http://stackoverflow.com/questions/6955128/object-not-found-error-with-ddply-inside-a-function
    tsngrams <<- loadScenarios(trainSample)
    tsngrams <<- splitNgrams(tsngrams)
    # perform tests
    tests.validation <- ddply(tests, .(i, nr), summarize, xgram=xgram, actual=actual, predict=predictN(xgram, tsngrams)[1,1], 
                              .progress="text")
    dprint(str(tests.validation))
    dprint(tests.validation)
    test.error <- sum(tests.validation$predict != as.character(tests.validation$actual), na.rm=TRUE) / nrow(tests.validation)
    data.frame(trainSample=trainSample, nr.testlines=nTest, nr.testgrams=nrow(tests.validation), test.error=test.error)
}

mydebug <- FALSE

# Validation
# nTest=c(50, 50, 50) # 3x 50 lines @ all n-grams per line
# trainSample=c(0.01, 0.05, 0.1)
validation.result <- mdply(.data=expand.grid(nTest=c(50, 50, 50), trainSample=c(0.05)), validate)
print(validation.result)

#----------------------------------------------------------------------------------
# note: test data may contain multiple sentences in single line of text. Test n-grams may therefore span
# sentence breaks, whereas the model n-grams don't span sentence breaks.
