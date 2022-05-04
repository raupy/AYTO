library(tidyr)
library(purrr)
library(data.table)
library(DescTools)


### functions for calculating all the combinations of perfect matches that are possible i matching night x


# this function is the heart of the script
# it joins the possible combinations from two matching nights @df_night_1 and @df_night_2
# TODO: explain how it works in general
join_nights <- function(df_night_1, df_night_2, nights, perfect_matches_df, no_matches_df) {
  night_y <- df_night_2$night[1]
  perfect_matches_night_y <- perfect_matches_df %>% 
    filter(night == night_y) %>% 
    pull(perfect_matches)
  
  no_matches_night_y <- no_matches_df %>% 
    filter(night <= night_y) %>% 
    pull(no_matches)
  
  inner <- df_night_1 %>%
    inner_join(df_night_2, by = "comb") %>%
    select(comb)
  
  filtered_night_join <- df_night_1 %>%
    full_join(df_night_2, by = "comb") %>%
    filter(is.na(night.x) | is.na(night.y)) 
  
  unique_combs <- filtered_night_join %>%
    inner_join(filtered_night_join, by = c("night.x" = "night.y")) %>% # we are not really matching night numbers but NAs
    transmute(whole_comb = paste(comb.x, comb.y)) %>% #, paste(perfect_matches_night_y, collapse = " "))) %>%
    remove_doubles() 
  
  unique_combs %>%
    bind_rows(inner) %>% 
    mutate(is_collision = sapply(comb, is_collision, perfect_match = perfect_matches_night_y, no_matches = no_matches_night_y)) %>% 
    filter(is_collision == F) %>% 
    transmute(comb, possible = sapply(comb, is_possible, matching_night = night_y, nights = nights)) %>% 
    filter(possible == T) %>% 
    transmute(night = night_y, comb) %>%
    arrange(comb)
}

# removes double entries from possible combinations -> only keeps unique combs
# example: "Aurelia+Diogo Aurelia+Diogo Jacky+Salvo Jill+Manu Kathleen+Salvo" -> "Aurelia+Diogo Jacky+Salvo Jill+Manu Kathleen+Salvo"
# it ignores that Salvo has two matches in this combination (Jacky+Salvo and Kathleen+Salvo) (the function "is.collision" deals with that)
remove_doubles <- function(comb) {
  comb %>% 
    separate(1, letters[1:20], sep = " ", extra = "drop", fill = "right") %>%
    apply(1, function(x) paste(sort(na.omit(unique(x))), collapse = " ")) %>%
    data.frame() %>%
    rename(comb = 1)
}


is_collision <- function(couples, perfect_match, no_matches) {
  if (length(no_matches) > 0) {
    if (any(str_detect(couples, fixed(no_matches)))) {
      return(TRUE)
    }
  }
  if (length(perfect_match) > 0) {
    if (str_detect(couples, pattern = fixed(perfect_match)) == FALSE) {
      return(TRUE)
    }
  }
  couples <- unlist(str_split(couples, fixed(" ")))
  special_person_detected <- str_detect(couples, fixed(special_person))
  if (sum(special_person_detected) == 1) {
    # the person she is with could have a second match, so don't set collision to true
    # instead: remove her couple from the vector 
    couples <- couples[-which(special_person_detected)]
  }
  solo_names <- unlist(str_split(couples, fixed("+")))
  if (length(solo_names) > length(unique(solo_names))) {
    return(TRUE)
  }
  return(FALSE)
}


is_possible <- function(comb, matching_night, nights) {
  is_possible <- TRUE
  comb <- unlist(str_split(comb[[1]], fixed(" "))) #it is a list with one element: the vector of couples
  for (i in 1:matching_night) {
    if (!check_upper_bound(nights[[i]], comb, max_cap[i])) { 
      is_possible <- FALSE
      break
    }
  }
  is_possible
}


check_upper_bound <- function(night, comb, upper_bound) {
  good_check <- FALSE
  if (length(intersect(night, comb)) <= upper_bound) {
    good_check <- TRUE
  } 
  good_check
}


### ------------------------------------------------------------------------------
### ------------------------------------------------------------------------------






# transforms a CombSet matrix @sub (from the DescTools package) into a data frame
# then pastes together all columns into one
# the resulting df contains two columns: the @night_number as night and comb string from @sub
# example:
#   night                                      comb
# 1     1       Aurelia+Diogo Jacky+Salvo Jill+Jamy
# 2     1   Aurelia+Diogo Jacky+Salvo Kathleen+Manu
# 3     1   Aurelia+Diogo Jacky+Salvo Steffi+Danilo
df_from_sub <- function(sub, night_number) {
  data.frame(sub) %>%
    unite(col = "comb", sep = " ") %>%
    transmute(night = night_number, comb)
}




combinations <- function(nights, max_cap, special_person, perfect_matches, no_matches, girls, boys) {
  start_sub <- CombSet(sort(nights[[1]]), m = max_cap[1]) 
  dfx <- df_from_sub(start_sub, 1)
  combs_list <- list()
  combs_list[[1]] <- dfx
  for (i in 2:length(nights)) {
    night <- nights[[i]]
    sub <- CombSet(sort(night), m = max_cap[i])
    dfy <- df_from_sub(sub, i)
    dfx <- join_nights(dfx, dfy, nights, perfect_matches, no_matches)
    if (nrow(dfx) == 0) {
      #TODO error warning??? not a single comb possible
    }
    combs_list[[i]] <- dfx
  }
  combs_list
}