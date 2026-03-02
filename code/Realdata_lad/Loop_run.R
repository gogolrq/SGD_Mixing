rm(list = ls())
library(here)
library(MASS)
here::i_am("Realdata_lad/Loop_run.R")
for (num in 1:4) {
  e <- new.env(parent = globalenv())  # <- key change
  e$commandArgs <- function(trailingOnly = TRUE) {
    if (trailingOnly)
      c(as.character(num))
    else
      c("Rscript", "R_main_lad.R", as.character(num))
  }
  source(here::here("Realdata_lad", "R_main_lad.R"), local = e)
}
