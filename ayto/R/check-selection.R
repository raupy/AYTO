check_selection <- function(night_y, comb, no_matches){
  # perfect_matches_night_y <- perfect_match %>% 
  #   filter(night <= night_y) %>% 
  #   pull(perfect_matches)
  has_no_match <- has_a_no_match(night_y, comb, no_matches)
  if(has_no_match != ""){
    df <- tibble(x = has_no_match) 
    names(df) = c("No Match")
    return(df)
  }
  
  has_collision <- is_collision(comb)
  if (has_collision != "") {
    df <- tibble(x = has_collision) 
    names(df) = c("Problem mit")
    return(df)
  } else {
    map_dfr(1:night_y, ~ is_possible(.x, comb, nights)) %>%
      filter(!is.na(collision_with))
  }
}

has_a_no_match <- function(night_y, couples, no_matches) {
  no_matches_night_y <- no_matches %>%
    filter(night <= night_y) %>%
    pull(no_matches)
  
  res <- ""
  if (length(no_matches) > 0) {
    has_no_match <- str_detect(couples, fixed(no_matches_night_y))
    if(any(has_no_match)){
      res <- no_matches_night_y[has_no_match] %>% paste(collapse = ", ")
    }
  }
  res
}



is_collision <- function(couples) {
  # if (length(perfect_match) > 0) {
  #   if (str_detect(couples, pattern = fixed(perfect_match)) == FALSE) {
  #     return(TRUE)
  #   }
  # }
  couples <- unlist(str_split(couples, fixed(", ")))
  special_person_detected <- str_detect(couples, fixed(special_person))
  if (sum(special_person_detected) == 1) {
    # the person she is with could have a second match, so don't set collision to true
    # instead: remove her couple from the vector 
    couples <- couples[-which(special_person_detected)]
  }
  solo_names <- unlist(str_split(couples, fixed("+")))
  if (length(solo_names) > length(unique(solo_names))) {
    duplicats <- solo_names[duplicated(solo_names)]
    paste(couples[str_detect(couples, rebus::or1(duplicats))], collapse = ", ")
  } else {
    ""
  }
}


is_possible <- function(matching_night, comb, nights) {
  comb <- unlist(str_split(comb, fixed(", "))) 
  intersection <- intersect(nights[[matching_night]], comb)
  collision <- NA
  zu_viele <- max_cap[matching_night] - length(intersection)
  if (zu_viele < 0) {
    collision <- intersection %>% paste(collapse = ", ")
  }
  tibble(matching_night = matching_night, 
         max = max_cap[matching_night],
         zu_viele = zu_viele * (-1),
         collision_with = collision)
}


match_to_matrix <- function(matches){
  if(length(matches) > 0){
    matches <- unlist(str_split(matches, ", "))
    solo_names <- unlist(str_split(matches, fixed("+")))
    girls_i <- seq(1, length(solo_names), 2)
    girls <- solo_names[girls_i]
    boys <- solo_names[-girls_i]
    girls_i <- map_int(girls, ~ which(names(ayto_tbl) == .x))
    boys_i <- map_int(boys, ~ which(rownames(ayto_tbl) == .x))
    cbind(boys_i, girls_i)
  } else{
    NULL
  }
}

selectable_cells <- function(no_matches){
  all_cells <- cbind(rep(1:nrow(ayto_tbl), ncol(ayto_tbl)), 
                    rep(1:ncol(ayto_tbl), nrow(ayto_tbl))) 
  # colnames(all_cells) = c("row", "col")
  # res <- all_cells %>%
  #   as_tibble() 
  
  if(is.null(no_matches)){
    return(all_cells)
  }
  
  for(i in 1:nrow(no_matches)){
    drop_row <- which(all_cells[,1] == no_matches[i, 1]
          & all_cells[,2] == no_matches[i, 2])
    all_cells <- all_cells[-drop_row, ]
      # filter(row != no_matches[i, 1] & col != no_matches[i, 2])
  }
  # res %>% 
  #   arrange(row) %>%
  #   as.matrix()
  all_cells
}