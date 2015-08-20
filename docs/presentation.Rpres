WordPred: Shiny Web App for Text Prediction Using N-Grams
========================================================
author: Alexander Vlasblom
date: July-August 2015


Data Science Specialization SwiftKey Capstone
Coursera / Johns Hopkins University

WordPred: Text Prediction Using N-grams
========================================================

<style>

/* slide titles */
.reveal h3 { 
  font-size: 48px;
  color: #fa821e;
}

img[alt=two-col-image] { 
  width: 1000px; 
}

</style>

[WordPred](https://demgenman.shinyapps.io/shiny_ngram_app) is a web app that accepts typed input and predicts the next word.

- Easy to use: just start typing a sentence, the predicted next word is displayed when typing stops.
- Prediction uses language model trained with a 5% sample of an English text corpus consisting of 4 million lines of news, blog and twitter texts.
- Good performance: initialization typically within seconds, next word prediction in less than 0.4 sec.
- Acceptable accuracy: approx. 13-14% (top suggested word, N=700 tests).

WordPred Features
========================================================

- Instantaneous prediction of next word
- 3 language model sizes (0.3%, 1%, 5% of full corpus) for different accuracy levels
- Candidate word list with frequency and next-word likelihood values
- Next-word word cloud 

***

![two-col-image](presentation-figure/WordPred-3.PNG) 
- Performance stats: initialization time, n-gram counts and memory usage, prediction time

Prediction Algorithm and Implementation
========================================================

- The language model consists of 4-, 3-, 2- and 1-grams and n-gram conditional probabilities that have been computed separately from the initial corpus, using maximum likelihood formula: 
$$
Pr(w_{i}|w_{i-1}) = \frac {count(w_{i-1},w_{i})} {count(w_{i-1})}
$$
where $w_{i}$ is last word, $w_{i-1}$ is n-1 preceding words.
- Trailing 3-, 2-, and 1-grams of entered text are matched against model data. The next word prediction is the top item in the table of matches, reverse ordered by n and likelihood values. It is identical to a simple backoff approach.
- Upon app start, the n-gram language data is efficiently read in binary form (.rds format) as a list of data frames.

Future work and References
========================================================

- Reduce startup time and memory usage: For a language model using a 5% corpus sample the current implementation ("list of data frames") needs 430 MB and 15 seconds startup time. A "data table"-based implementation needs much less memory, reducing startup time to 1-2 seconds. 
- Increase prediction accuracy: To 20% or higher by smoothing approaches, noteably Kneser-Ney, and a larger sample from the language corpus (20% vs. 10% currently). A data table-based implementation ensures performance will not be negatively impacted.

References: [Github repo](https://github.com/demgenman/ngram-text-prediction), [WordPred app](https://demgenman.shinyapps.io/shiny_ngram_app)
