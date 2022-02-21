### functions for summarizing the list of dfs into a single summarized table ready for plotting


get_number_of_possible_permutations <- function(df){
  vars <- names(df)
  conditions <- 1:ncol(df)
  for(i in 1:length(vars)){
    df <- df %>% filter(.data[[vars[[i]]]] != conditions[[i]])
  }
  nrow(df)
}

get_number_of_impossible_permutations <- function(n){
  coeffs <- map_int(3:(n-1), ~ data.frame(Permn(1:.x)) %>%
                      get_number_of_possible_permutations)
  coeffs <- c(1,0,1,coeffs)
  n_combs <- sapply(n:1, CombN, n = n)  
  sum(coeffs*n_combs)
}

get_summarised_table_for_night_1_and_girl_x <- function(match, boys, prob_match, prob_other_matches, freq_match, freq_other_matches, n_combs){
  match_split <- str_split(match, fixed("+"), simplify = T)
  girl <- match_split[,1]
  boy <- match_split[,2]
  df <- data.frame(night = 1,
                   girl = girl,
                   boys = as.factor(boys),
                   prob = prob_other_matches,
                   freq = freq_other_matches,
                   total = n_combs) 
  boys_number <- which(boys == boy)
  df$prob[boys_number] <- prob_match
  df$freq[boys_number] <- freq_match
  df
}

get_summarised_table_for_night_1 <- function(n_matches, girls, boys, night1){
  n_couples <- length(night1) # in most of the seasons it should be 10
  n_comb <- CombN(n_couples, n_matches) # e.g. CombN(10,3) = 120
  prob_girl_correct_match <- n_matches/n_couples # e.g. 3/10
  prob_girl_other_matches <- (1-prob_girl_correct_match)/(n_couples - 1) # e.g. (7/10) / 9
  n_wrong_matches <- n_couples - n_matches # e.g. 10-3=7
  
  # freq:
  # supposing the match is correct, how many combinations exist with that match?
  # n_comb * factorial(n_wrong_matches) is the number of all combinations
  # we just have to multiply with prob_girl_correct_match
  # proof: write down the formula for CombN(n_couples-1, n_matches-1)/CombN(n_couples, n_matches). it is n_matches/n_couples = prob_girl_correct_match
  combs_correct_match <- prob_girl_correct_match * n_comb * factorial(n_wrong_matches) # e.g. 0.3*120*7!
  # out of combs_correct_match, there exist prob_girl_correct_match * n_comb * x combinations that are not possible; e.g. 0.3*120*x 
  # what is x? we have to sum up the binomial coefficients of n_wrong_matches multiplied by their corresponding number of combinations
  # e.g.: n_wrong_matches = 7 -> x = 1*CombN(7,7) + 0* CombN(7,6) + 1*CombN(7,5) + a*CombN(7,4)+ b*CombN(7,3) + c*CombN(7,2) + d*CombN(7,1)
  # the coefficients a,b,c.. etc. are the number of permutations that are not equal to the original CombN(7,7) permutation
  x <- get_number_of_impossible_permutations(n_wrong_matches)
  freq_girl_correct_match <- combs_correct_match - prob_girl_correct_match * n_comb * x
  
  # total
  total <- freq_girl_correct_match/prob_girl_correct_match
  
  # freq match with other boys
  freq_other_matches <- prob_girl_other_matches*total
  
  map_dfr(night1, ~ get_summarised_table_for_night_1_and_girl_x(.x, boys, 
                                                                prob_girl_correct_match, prob_girl_other_matches, 
                                                                freq_girl_correct_match*10, freq_other_matches*10,
                                                                total*10)) %>%
    bind_rows(get_summarised_table_for_night_1_and_girl_x("Vanessa+Diogo", boys, 
                                                          0.1, 0.1, 
                                                          total, total,
                                                          total*10))
}





### __________________________________________________________________________________________
### __________________________________________________________________________________________
### __________________________________________________________________________________________


get_summarised_table_for_col <- function(col_number, comb_df, night_number, boys_factor){
  table(comb_df[,col_number]) %>% 
    data.frame() %>% 
    complete(Var1 = boys_factor, fill = list(Freq = 0L)) %>%
    mutate(girl = names(comb_df)[col_number],
           night = night_number,
           total = sum(Freq), 
           prob = Freq/total) %>%
    rename(boy = Var1, freq = Freq) %>%
    select(night, girl, boy, prob, freq, total)
}

get_summarised_table_for_night <- function(comb_df, night, boys_factor){
  df <- comb_df %>% select(-night)
  bind_rows(lapply(1:ncol(df), get_summarised_table_for_col, comb_df = df, night_number = night, boys_factor = boys_factor))
}

mutate_col_into_df <- function(num, list_of_dfs){
  list_of_dfs[[num]] %>% mutate(night = num)
}

get_summarised_table_for_every_night <- function(list_of_comb_dfs, nights, boys_factor){
  list_of_dfs_with_night <- map(nights, ~ mutate_col_into_df(.x, list_of_comb_dfs))
  map_dfr(list_of_dfs_with_night, ~ get_summarised_table_for_night(.x, night = min(.x[["night"]]), boys_factor = boys_factor))
}