library(dplyr)
library(readxl)
library(tibble)
library(stringr)
library(assertive)

### ------------------------------------------------------------------------------
### functions for reading AYTO excel sheet and extracting data 

readAYTO <- function(ayto_excel_file){
  read_excel(ayto_excel_file) %>%
    column_to_rownames(var = "boys") # get the boys column as row names
}

# extracts from the @ayto_data frame the couple of the @girl in night @x 
get_couple_for_girl_in_night_x <- function(girl, x, ayto_data){
  assert_all_are_positive(x)
  girl_filtered <- ayto_data %>%
    filter(str_detect(.data[[girl]], as.character(x)))
  if(nrow(girl_filtered) == 0){ # a girl might not have a couple because of 11th female candidate
    return("")
  } 
  paste(girl, rownames(girl_filtered)[1], sep = "+") # e.g. "Aurelia+Diogo"
}

# returns a chr with all the couples from matching night @night_nr using the @ayto_tbl and the vector of the @girls names
filter_night_x <- function(night_nr, ayto_tbl, girls){
  couples <- c()
  couples <- sapply(girls, get_couple_for_girl_in_night_x, x = night_nr, ayto_data = ayto_tbl)
  couples <- as.vector(couples)
  couples[which(couples != "")] # remove empty string couples (girls without couple)
}

### ------------------------------------------------------------------------------
### ------------------------------------------------------------------------------


ayto_excel_file <- "ayto_reality_special.xlsx"
ayto_tbl <- readAYTO(ayto_excel_file)


night_lights <- read_excel(ayto_excel_file, sheet = "night_lights") 
# this sheet contains a table with one row: the matching nights' numbers and their corresponding number of lights

# this sheet contains a table with one row: the matching nights' numbers and their corresponding number of lights
nights_in_the_future <- which(is.na(night_lights[1,])) # any missing data? (e.g. beginning of a season)
night_lights <- night_lights[1, -nights_in_the_future] # ignore missing data = drop future nights

boys <- rownames(ayto_tbl)
girls = colnames(ayto_tbl)
all_nights_couples <- lapply(1:ncol(night_lights), filter_night_x, ayto_tbl = ayto_tbl, girls = girls)
# a list with all matching night couples in every matching night

perfect_matches <- read_excel(ayto_excel_file, sheet = "perfect_matches") 
no_matches <- read_excel(ayto_excel_file, sheet = "no_matches") 