rm(list = ls())
library(here)
library(MASS)
here::i_am("Model4/Loop_run.R")
for (n in c(20000, 50000, 100000)) {
  for (a in c(0, 0.2, 0.3, 0.33, 0.5, 0.7)) {
    for (RandomSeed in 1:500) {
      e <- new.env(parent = globalenv())  # <- key change
      e$commandArgs <- function(trailingOnly = TRUE) {
        if (trailingOnly)
          c(as.character(n),
            as.character(a),
            as.character(RandomSeed))
        else
          c(
            "Rscript",
            "R_main_Model4.R",
            as.character(n),
            as.character(a),
            as.character(RandomSeed)
          )
      }
      source(here::here("Model4", "R_main_Model4.R"), local = e)
    }
  }
}
