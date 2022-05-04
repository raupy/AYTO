library(dplyr)
library(readxl)
library(tibble)
library(stringr)
library(assertive)
### functions for reading AYTO excel sheet and extracting data 


readAYTO <- function(ayto_excel_file) {
  read_excel(ayto_excel_file) %>%
    column_to_rownames(var = "boys") # get the boys column as row names
}

# extracts from the @ayto_data frame the couple of the @girl in night @x 
get_couple_for_girl_in_night_x <- function(girl, x, ayto_data) {
  assert_all_are_positive(x)
  girl_filtered <- ayto_data %>%
    filter(str_detect(.data[[girl]], as.character(x)))
  if (nrow(girl_filtered) == 0) { # a girl might not have a couple because of 11th female candidate
    return("")
  } 
  paste(girl, rownames(girl_filtered)[1], sep = "+") # e.g. "Aurelia+Diogo"
}

# returns a chr with all the couples from matching night @night_nr using the @ayto_tbl and the vector of the @girls names
filter_night_x <- function(night_nr, ayto_tbl, girls) {
  couples <- c()
  couples <- sapply(girls, get_couple_for_girl_in_night_x, x = night_nr, ayto_data = ayto_tbl)
  couples <- as.vector(couples)
  couples[which(couples != "")] # remove empty string couples (girls without couple)
}