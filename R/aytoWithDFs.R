








set.cell.value <- function(x, y, matching_df, value){
  if(missing(x)){
    for(i in 1:length(rownames(matching_df))){
      matching_df <- set.cell.value(i, y, matching_df, value)
    } 
  }
  else if(missing(y)){
    for(i in 1:length(names(matching_df))){
      matching_df <- set.cell.value(x, i, matching_df, value)
    } 
  }
  else {
    if(matching_df[x,y] == 0){
      matching_df[x, y] <- value
    }
    else{
      matching_df[x, y] <- paste(matching_df[x,y], value, sep = ", ")
    }
  }
  matching_df
}

make.entry <- function(matching_night, matching_df){
  couples <- str_split(matching_night$couples, fixed("+"))
  for (i in 1:length(couples)){
    x = couples[[i]][2]
    y = couples[[i]][1]
    night <- matching_night$night
    matching_df <- set.cell.value(x, y, matching_df, night)
  }
  matching_df
}

make.all.entries <- function(night_list, matching_df){
  for(night in night_list){
    matching_df <- make.entry(night, matching_df)
  }
  matching_df
}

initData <- function(){
  girls <- c("Kathleen", "Jill", "Steffi", "Sarah", "Aurelia", "Walentina", "Melina", "Finnja", "Jacky", "TestXX2")
  boys <- c("Salvo", "Manu", "Jamy", "Alex", "Danilo", "Diogo", "Tommy", "Eugen", "Josua", "TestXY2")
  boys_sort <- sort(boys)
  girls_sort <- sort(girls)
  na <- rep(0, 10)
  matching_df <- data.frame(na, na, na, na, na,
                            na, na, na, na, na,
                            na) # "vanessa" 11th single lady
  colnames(matching_df) <- girls_sort
  colnames(matching_df)[11] <- "Vanessa"
  rownames(matching_df) <- boys_sort
  #matching_df <- matching_df %>% as_tibble()
  
  vier_zweier = c("Kathleen+Salvo", "Jill+Manu", "Steffi+Jamy", "Sarah+Alex")
  fuenf_zweier = c("Kathleen+Manu",	"Jill+Danilo",	"Steffi+Alex", "Aurelia+Diogo", "Jacky+Salvo"	)
  zwei_zweier = c("Kathleen+Manu",	"Jill+Jamy" , "Sarah+Danilo", "Aurelia+Diogo", "Jacky+Salvo", "Finnja+Alex")
  sechs_einer = c("Kathleen+Manu", "Jill+Eugen", 	"Jacky+Alex", "Finnja+Jamy", "Walentina+Salvo")
  eins_dreier = c("Kathleen+Manu",	"Jill+Jamy",	"Steffi+Danilo",	"Aurelia+Diogo",	"Jacky+Salvo","Walentina+Eugen")
  drei_dreier = c("Vanessa+Jamy", "Kathleen+Manu", "Jill+Tommy","Steffi+Alex",	"Aurelia+Diogo","Walentina+Salvo","Melina+Danilo")
  sieben_dreier  = c("Walentina+Salvo","Melina+Eugen","Kathleen+Manu","Jill+Danilo","Finnja+Tommy","Steffi+Jamy","Sarah+Alex","Aurelia+Diogo","Vanessa+Josua")
  acht_fuenfer = c("Walentina+Eugen","Kathleen+Manu","Jill+Diogo","Finnja+Tommy","Steffi+Jamy","Sarah+Danilo","Jacky+Salvo","Vanessa+Alex")
  
  night_1 <- list(night = 1, lights = 3, couples = eins_dreier)
  night_2 <- list(night = 2, lights = 2, couples = zwei_zweier)
  night_3 <- list(night = 3, lights = 3, couples = drei_dreier)
  night_4 <- list(night = 4, lights = 2, couples = vier_zweier)
  night_5 <- list(night = 5, lights = 2, couples = fuenf_zweier)
  night_6 <- list(night = 6, lights = 1, couples = sechs_einer)
  night_7 <- list(night = 7, lights = 3, couples = sieben_dreier)
  night_8 <- list(night = 8, lights = 5, couples = acht_fuenfer)
  night_list <- list(night_1, night_2, night_3, night_4, night_5,
                     night_6, night_7, night_8)
  
  matching_df <- make.all.entries(night_list, matching_df)
  matching_df
}

matching_df <- initData()
match_heart <- "\U1F495"
no_match_broken_heart <- "\U1F494"
no_match_emoji <- "\U1F625"
matching_df <- set.cell.value(x = "Josua", matching_df = matching_df, value = no_match_emoji)
matching_df <- set.cell.value(y = "Melina", matching_df = matching_df, value = no_match_emoji)
matching_df["Josua", "Melina"] <- match_heart
matching_df["Diogo", "Aurelia"] <- no_match_emoji

#nights <- list(vier_zweier, fuenf_zweier, zwei_zweier, sechs_einer, eins_dreier, drei_dreier, sieben_dreier, acht_fuenfer)

#max_cap <- c(2,2,2,1,3,3,3,5)
#max_cap <- as.numeric(night_lights[1,])
#combs2 <- combinations2(nights,max_cap)
