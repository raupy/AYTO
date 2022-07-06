

library(shiny)
library(bslib)
library(showtext)
library(thematic)
library(DT)
library(rebus)
library(readxl)
library(tidyverse)
library(assertive)


my_theme2 <- bs_theme(
  #bg = "#002B36", fg = "#EEE8D5", 
  primary = "#6f42c1",
  secondary = "#ea39b8",
  bg = "#1a0933",
  fg = "#ffffff",
  version = 5,
  #bootswatch = "vapor",
  base_font = font_google("Comfortaa")
)

thematic_shiny(font = "auto")








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


ayto_excel_file_names <- paste0("data/", dir("data"))
ayto_excel_file <- ayto_excel_file_names[1]
ayto_tbl <- readAYTO(ayto_excel_file)


night_lights <- read_excel(ayto_excel_file, sheet = "night_lights") 
# this sheet contains a table with one row: the matching nights' numbers and their corresponding number of lights

nights_in_the_future <- which(is.na(night_lights[1, ])) # any missing data? (e.g. beginning of a season)
night_lights <- night_lights[1, -nights_in_the_future] # ignore missing data = drop future nights
max_cap <- as.numeric(night_lights)

boys <- rownames(ayto_tbl)
girls <- colnames(ayto_tbl)
special_person <- girls[11] # 11th candidate comes later to the cast -> one boy gets a second perfect match
nights <- lapply(1:ncol(night_lights), # a list with all matching night couples in every matching night
                 filter_night_x, 
                 ayto_tbl = ayto_tbl, girls = girls) 

perfect_matches <- read_excel(ayto_excel_file, sheet = "perfect_matches") 
no_matches <- read_excel(ayto_excel_file, sheet = "no_matches") 

ayto_tbl <- ayto_tbl %>%
  mutate(across(everything(),
                ~ str_remove(.x, fixed("0"))
                )
  )
#### read rds files ----

github <- "https://github.com/raupy/AYTO/tree/dev/ayto/data/"


combs <- read_csv("data/combs.csv")
  #nflreadr::rds_from_url(paste0(github, "combs.rds")) #readRDS("data/combs.rds")
full_combs <- read_csv("data/full_combs.csv")
  #readRDS(gzcon(url(paste0(github, "combs_dfs_list.rds")))) #readRDS("data/combs_dfs_list.rds")
summarized_table <- read_csv("data/sumtable.csv")
  #readRDS(gzcon(url(paste0(github, "sumtable.rds")))) 
  #readRDS("data/sumtable.rds")