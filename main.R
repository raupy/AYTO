# this script doesn't contain any own functions
# it uses the functions from the five files below for solving the AYTO permutations puzzle:

if(!exists("readAYTO", mode="function")) source("read_AYTO_data.R")
if(!exists("combinations", mode="function")) source("get_possible_AYTO_couples.R")
if(!exists("make_tidy_comb_df", mode="function")) source("reshape_calculated_combs.R")
if(!exists("get_summarised_table_for_every_night", mode="function")) source("get_summarized_table.R")
if(!exists("plot_match_probabilities", mode="function")) source("plot_match_probabilities.R")





### ------------------------------------------------------------------------------ ###
### ----------------------------read_AYTO_data.R---------------------------------- ###


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

### ------------------------------------------------------------------------------ ###
### ------------------------------------------------------------------------------ ###





### ------------------------------------------------------------------------------ ###
### -------------------------get_possible_AYTO_couples.R-------------------------- ###

nights <- all_nights_couples
special_person = girls[11] # 11th candidate comes later to the cast -> one boy gets a second perfect match
max_cap <- as.numeric(night_lights)
combs <- combinations(nights,max_cap, special_person, perfect_matches, no_matches, girls, boys)

### ------------------------------------------------------------------------------ ###
### ------------------------------------------------------------------------------ ###





### ------------------------------------------------------------------------------ ###
### --------------------------reshape_calculated_combs.R-------------------------- ###

combs_dfs_list <- lapply(combs[2:9], get_df_from_combs, girls, boys, special_person)

### ------------------------------------------------------------------------------ ###
### ------------------------------------------------------------------------------ ###





### ------------------------------------------------------------------------------ ###
### --------------------------get_summarized_table.R------------------------------ ###

summarized_table <- get_summarised_table_for_every_night(combs_dfs_list, nights = 1:8, as.factor(boys)) %>%
  mutate(night = night + 1) %>%
  bind_rows(get_summarised_table_for_night_1(max_cap[1], girls, boys, nights[[1]]))

### ------------------------------------------------------------------------------ ###
### ------------------------------------------------------------------------------ ###





### ------------------------------------------------------------------------------ ###
### --------------------------plot_match_probabilities.R-------------------------- ###

plot_match_probabilities(summarized_table, "Aurelia") 

### ------------------------------------------------------------------------------ ###
### ------------------------------------------------------------------------------ ###
