### functions for reshaping data frames of perfect matches chr strings into tidier dfs


save_number_of_combinations <- function(df, special_person) {
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

reverse_paste <- function(x, y, sep) {
  paste(y, x, sep = sep)
}

get_permutations <- function(combs, night, girls, boys, special_person) {
  list_of_comb_dfs <- lapply(combs, get_permutations_from_comb, night = night, girls = girls, boys = boys, special_person = special_person)
  rbindlist(list_of_comb_dfs) 
}

get_permutations_from_comb <- function(fixed_comb, night, girls, boys, special_person) {
  single_girls <- girls[which(str_detect(fixed_comb, girls, negate = TRUE))]
  single_boys <- boys[which(str_detect(fixed_comb, boys, negate = TRUE))]
  permutations <- data.frame(Permn(single_boys, sort = T))
  perms_bind <- tibble()
  if (special_person %in% single_girls) {
    for (i in 1:length(boys)) {
      perms_bind <- bind_rows(perms_bind, permutations)
    }
    names(perms_bind) <- single_girls[which(single_girls != special_person)]
    perms_bind <- perms_bind %>%
      mutate(special = rep(boys, each = nrow(permutations))) 
    names(perms_bind)[ncol(perms_bind)] <- special_person
  }else{
    if (length(single_boys) != length(single_girls)) {
      # look at the boy who is matched with the special_person:
      # has he already a match with the original cast or should he be part of the single_boys vector?
      # special_person has a match with a boy who also has another match, so he needs to be part of the single boys here
      vec_of_couples <- unlist(str_split(fixed_comb, fixed(" ")))
      special_person_match <- vec_of_couples[sapply(vec_of_couples, str_detect, pattern = fixed(special_person))]
      special_person_boy <- str_split(special_person_match, pattern = fixed("+"), simplify = T)[, 2]
      single_boys <- c(single_boys, special_person_boy)
    }
    perms_bind <- data.frame(Permn(single_boys, sort = T))
    names(perms_bind) <- single_girls
  }
  perms_bind %>%
    mutate(across(everything(), ~ reverse_paste(.x, cur_column(), sep = "+"))) %>%
    unite(col = "comb", sep = " ") %>%
    transmute(fixed_comb = fixed_comb, comb) %>%
    unite(col = "comb", sep = " ") %>%
    remove_doubles() %>%
    transmute(comb, possible = sapply(comb, is_possible, matching_night = night)) %>% 
    filter(possible == T) %>% 
    select(comb)
}


make_tidy_comb_df <- function(comb_df) {
  boys_as_values <- comb_df %>%
    separate(1, letters[1:11], sep = " ", extra = "drop", fill = "right") %>%
    mutate(across(everything(), ~ str_split(.x, fixed("+"), simplify = TRUE)[, 2]))
  girls <- comb_df[1, ] %>%
    separate(1, letters[1:11], sep = " ", extra = "drop", fill = "right") %>%
    mutate(across(everything(), ~ str_split(.x, fixed("+"))[[1]][1]))
  colnames(boys_as_values) <- as.character(girls)
  if (nrow(boys_as_values) > 1) {
    boys_as_values <- data.frame(sapply(boys_as_values, as.factor), stringsAsFactors = T)
  }
  else{
    data.frame(boys_as_values)
  }
}

get_df_from_combs <- function(comb_df, girls, boys, special_person) {
  get_permutations(comb_df$comb, comb_df$night[1], girls, boys, special_person) %>%
    make_tidy_comb_df()
}

