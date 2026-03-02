rm(list = ls())
library(here)
library(MASS)
here::i_am("Additional_Simulation/Model7/Loop_run.R")
for (n in c(20000, 50000, 100000)) {
  for (a in c(0.33)) {
    for (B in c(50, 200, 500, 1000)) {
      for (RandomSeed in 1:500) {
        e <- new.env(parent = globalenv())  # <- key change
        e$commandArgs <- function(trailingOnly = TRUE) {
          if (trailingOnly)
            c(as.character(n),
              as.character(a),
              as.character(B),
              as.character(RandomSeed))
          else
            c(
              "Rscript",
              "R_main_Add_Model7.R",
              as.character(n),
              as.character(a),
              as.character(B),
              as.character(RandomSeed)
            )
        }
        source(
          here::here(
            "Additional_Simulation",
            "Model7",
            "R_main_Add_Model7.R"
          ),
          local = e
        )
      }
    }
  }
}
