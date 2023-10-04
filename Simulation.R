#import libraries
library(googledrive)
library(googlesheets4)
library(tidyverse)

senate_sheets <- drive_get("Senate Data")

stateslist <- senate_sheets %>%
  range_read(sheet = 1) %>%
  pull(State)
demographics <- senate_sheets %>%
  range_read(sheet = 6) %>%
  slice(-c(1, 2))

normalized_dems <- scale(demographics[, 2:8])
normalized_dems$State <- demographics$States
  

r_value <- function(state1, state2) {
  
}

combination <- expand.grid(stateslist, stateslist) %>%
  slice(c(1:578)) 

