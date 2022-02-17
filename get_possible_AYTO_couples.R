library(tidyr)
library(purrr)
library(data.table)
#install.packages("DescTools")
library(DescTools)

if(!exists("readAYTO", mode="function")) source("read_AYTO_data.R")

### ------------------------------------------------------------------------------
### ------------------------------------------------------------------------------





# this function is the heart of the script
# it joins the possible combinations from two matching nights @df_night_1 and @df_night_2
# TODO: explain how it works in general
join_nights<- function(df_night_1, df_night_2, perfect_matches_df){
  night_y <- df_night_2$night[1]
  perfect_matches_night_y <- perfect_matches_df %>% 
    filter(night == night_y) %>% 
    pull(perfect_matches)
  
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
    mutate(is_collision = sapply(comb, is.collision, perfect_match = perfect_matches_night_y)) %>% 
    filter(is_collision == F) %>% 
    transmute(comb, possible = sapply(comb, is.possible)) %>% 
    filter(possible == T) %>% 
    transmute(night = night_y, comb) %>%
    arrange(comb)
}

# removes double entries from possible combinations -> only keeps unique combs
# example: "Aurelia+Diogo Aurelia+Diogo Jacky+Salvo Jill+Manu Kathleen+Salvo" -> "Aurelia+Diogo Jacky+Salvo Jill+Manu Kathleen+Salvo"
# it ignores that Salvo has two matches in this combination (Jacky+Salvo and Kathleen+Salvo) (the function "is.collision" deals with that)
remove_doubles <- function(comb){
  comb %>% 
    separate(1, letters[1:11], sep = " ", extra = "drop", fill = "right") %>%
    apply(1, function(x) paste(sort(na.omit(unique(x))), collapse = " ")) %>%
    data.frame() %>%
    rename(comb = 1)
  #l <- list()
  #for(i in 1:nrow(comb)){
  #  vec <- sort(unique(unlist(str_split(comb[i,1], " "))))
  #  l[[i]] <- paste(vec, collapse = " ")
  #}
  #unlist(l)
}




is.collision <- function(vec_of_couples, perfect_match){
  vec_of_couples <- vec_of_couples[[1]] # it's a list with one element: the vector of couples
  org_vec <- vec_of_couples
  vec_of_couples <- unlist(str_split(vec_of_couples, fixed(" ")))
  collision <- FALSE
  special_person_detect <- sapply(vec_of_couples, str_detect, pattern = fixed(special_person))
  if(sum(special_person_detect) == 1){ 
    # the person she is with could have a second match, so don't set collision to true
    # instead: remove her couple from the vector 
    vec_of_couples <- vec_of_couples[-which(special_person_detect)]
  }
  solo_names <- unlist(str_split(vec_of_couples, fixed("+")))
  if(length(perfect_match) > 0){
    if(str_detect(org_vec, pattern = fixed(perfect_match)) == FALSE){
      collision <- TRUE
    }
  } 
  if(length(solo_names) > length(unique(solo_names))){
    collision <- TRUE
  }
  collision
}


is.possible <- function(comb){
  is_possible <- TRUE
  comb <- unlist(str_split(comb[[1]], fixed(" "))) #it is a list with one element: the vector of couples
  for(i in 1:length(nights)){
    if(!check.upper.bound(nights[[i]], comb, max_cap[i])){ 
      is_possible <- FALSE
      break
    }
  }
  is_possible
}


check.upper.bound <- function(night, comb, upper_bound){
  good_check <- FALSE
  if(length(intersect(night, comb)) <= upper_bound){
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
df_from_sub <- function(sub, night_number){
  data.frame(sub) %>%
    unite(col = "comb", sep = " ") %>%
    transmute(night = night_number, comb)
}


save_number_of_combinations <- function(df, special_person){
  df %>% 
    mutate(n_whitespace = str_count(comb, " "),
           contains_special_person = str_detect(comb, fixed(special_person))) %>% 
    group_by(n_whitespace, contains_special_person) %>%
    count() %>% 
    mutate(n_couples = n_whitespace + as.numeric(!contains_special_person),
           n_missing_couples = 10 - n_couples,
           n_comb = n * factorial(n_missing_couples) * 10^(as.numeric(!contains_special_person))) %>%
    ungroup() %>%
    summarise(n_comb = sum(n_comb)) %>%
    transmute(night = df$night[1], n_comb)
}

reverse_paste <- function(x, y, sep){
  paste(y,x,sep = sep)
}

get_permutations <- function(combs, girls, boys, special_person){
  list_of_comb_dfs <- lapply(combs, get_permutations_from_comb, girls = girls, boys = boys, special_person = special_person)
  rbindlist(list_of_comb_dfs) 
}

get_permutations_from_comb <- function(fixed_comb, girls, boys, special_person){
  single_girls <- girls[which(str_detect(fixed_comb, girls, negate = TRUE))]
  single_boys <- boys[which(str_detect(fixed_comb, boys, negate = TRUE))]
  permutations <- data.frame(Permn(single_boys, sort = T))
  perms_bind <- tibble()
  if(special_person %in% single_girls){
    for(i in 1:length(boys)){
      perms_bind <- bind_rows(perms_bind, permutations)
    }
    names(perms_bind) <- single_girls[which(single_girls != special_person)]
    perms_bind <- perms_bind %>%
      mutate(special = rep(boys, each = nrow(permutations))) 
    names(perms_bind)[ncol(perms_bind)] <- special_person
  }else{
    # special_person has a match with a boy who also has another match, so he needs to be part of the single boys here
    vec_of_couples <- unlist(str_split(fixed_comb, fixed(" ")))
    special_person_match <- vec_of_couples[sapply(vec_of_couples, str_detect, pattern = fixed(special_person))]
    special_person_boy <- str_split(special_person_match, pattern = fixed("+"), simplify = T)[,2]
    single_boys <- c(single_boys, special_person_boy)
    perms_bind <- data.frame(Permn(single_boys, sort = T))
    names(perms_bind) <- single_girls
  }
  perms_bind %>%
    mutate(across(everything(), ~ reverse_paste(.x, cur_column(), sep = "+"))) %>%
    unite(col = "comb", sep = " ") %>%
    transmute(fixed_comb = fixed_comb, comb) %>%
    unite(col = "comb", sep = " ") %>%
    remove_doubles()
}


make_tidy_comb_df <- function(comb_df){
  boys_as_values <- comb_df %>%
    separate(1, letters[1:11], sep = " ", extra = "drop", fill = "right") %>%
    mutate(across(everything(), ~ str_split(.x, fixed("+"), simplify = TRUE)[, 2]))
  girls <- comb_df[1,] %>%
    separate(1, letters[1:11], sep = " ", extra = "drop", fill = "right") %>%
    mutate(across(everything(), ~ str_split(.x, fixed("+"))[[1]][1]))
  colnames(boys_as_values) <- as.character(girls)
  data.frame(sapply(boys_as_values, as.factor), stringsAsFactors = T)
}

get_df_from_combs <- function(combs, girls, boys, special_person){
  get_permutations(combs, girls, boys, special_person) %>%
    make_tidy_comb_df()
}


combinations <- function(nights, max_cap, special_person, perfect_matches, girls, boys){
  start_sub <- CombSet(sort(nights[[1]]), m=max_cap[1]) 
  dfx <- df_from_sub(start_sub,1)
  number_of_combs <- tibble()
  for(i in 2:length(nights)){
    night <- nights[[i]]
    sub <- CombSet(sort(night), m=max_cap[i])
    dfy <- df_from_sub(sub,i)
    dfx <- join_nights(dfx,dfy,perfect_matches)
    if(nrow(dfx) == 0){
      #TODO ???
    }
    number_of_combs <- rbind(number_of_combs, save_number_of_combinations(dfx, special_person))
  }
  list(dfx, number_of_combs, get_df_from_combs(dfx$comb, girls, boys, special_person)) 
}



### ------------------------------------------------------------------------------
### ------------------------------------------------------------------------------

nights <- all_nights_couples
special_person = girls[11] # 11th candidate comes later to the cast -> one boy gets a second perfect match
max_cap <- as.numeric(night_lights)
combs2 <- combinations(nights,max_cap, special_person, perfect_matches, girls, boys)