# ---
# title:  "Text Prediction Using N-Grams"
# subtitle:   "Coursera Data Science Capstone Milestone Report"
# author: "Alexander Vlasblom"
# date:   "July 2015"
# ---

# sharedheader.R
#

# rm(list=ls()); gc()

# debug print functions
dmessage <- function(...) if (mydebug) message(...)
dprint <- function(...) if (mydebug) print(...)

tslist <- c("0.30", "1.00", "5.00")
tslist.selected <- "1.00"

mydebug <- FALSE
