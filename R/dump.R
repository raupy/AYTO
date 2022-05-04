library(plyr)
library(dplyr)
library(DescTools)
library(stringr)

update.list <- function(valid_combs, old_list, i){
  new_list = list()
  for(item in old_list[-i]){
    new_list[length(new_list) + 1] <- list(item)
  }
  if(length(valid_combs) > 0){
    for(valid_comb_item in valid_combs){
      new_list[length(new_list) + 1] <- list(unique(c(old_list[[i]], valid_comb_item)))
    }
  }
  
  new_list
}

valid.combinations <- function(res, test_combs, ncol_test_combs){
  comb_list = list()
  l = length(res)
  y = l/ncol_test_combs
  for(i in 1:y){
    indices = unique(c(i, (y + i) %% l,(2*y + i) %% l, (3*y + i) %% l, (4*y + i) %% l, (5*y + i) %% l, (6*y + i) %% l, (7*y + i) %% l))
    indices[which(indices == 0)] = l
    res_i = res[indices]
    if(sum(res_i) == ncol_test_combs){
      comb_list[length(comb_list) + 1] <- list(names(res_i))
      #comb_list[letters[i]] <- list(names(res_i))
    }
  }
  comb_list
}



fusion.two.nights <- function(test_combs, comb_list){
  for(i in 1:length(comb_list)){
    res = sapply(test_combs, go.further, whole_comb = comb_list[[i]])
    valid_combs = valid.combinations(res)
    comb_list <- update.list(valid_combs, comb_list, i)
  }
  comb_list
}


#rbind.fill(data_plyr1, data_plyr2)

data.frame.from.sub <- function(sub, night_number){
  night <- c()
  night[1:nrow(sub)] <- night_number
  amount <- c()
  amount[1:nrow(sub)] <- ncol(sub)
  df <- as.data.frame(sub)
  df %>%
    arrange_all()
  cbind(night, amount, df)
} 

df.from.sub <- function(sub, night_number){
  night <- c()
  night[1:nrow(sub)] <- night_number
  comb <- c()
  for(i in 1:nrow(sub)){
    vec <- c(sub[i,1])
    temp_paste <- paste(sub[i,1])
    if(ncol(sub) > 1){
      for(j in 2:ncol(sub)){
        vec <- c(vec, sub[i,j])
        temp_paste <- paste(temp_paste, sub[i,j])
      }
    }
    comb <- c(comb, temp_paste)
    #comb <- c(comb, paste(sort(vec)))
  }
  df <- data.frame(comb)
  names(df)[1] <- "comb"
  cbind(night, df)
}


remove.doubles <- function(comb){
  l <- list()
  for(i in 1:nrow(comb)){
    vec <- sort(unique(unlist(str_split(comb[i,1], " "))))
    temp_paste <- paste(vec[1])
    for(j in 2:length(vec)){
      temp_paste <- paste(temp_paste, vec[j])
    }
    #comb <- c(comb, temp_paste)
    l[[i]] <- temp_paste
  }
  unlist(l)
}

filtered.night.join <- function(df_night_1, df_night_2){
  filtered_night_join <- df_night_1 %>%
    full_join(df_night_2, by = "comb") %>%
    filter(is.na(night.x) | is.na(night.y)) #%>%
    
  filtered_night_join %>% inner_join(filtered_night_join, by = c("night.x" = "night.y"))
}

join.nights<- function(df_night_1, df_night_2){
  inner <- df_night_1 %>%
    inner_join(df_night_2, by = "comb") %>%
    select(comb)
  
  filtered_night_join <- df_night_1 %>%
    full_join(df_night_2, by = "comb") %>%
    filter(is.na(night.x) | is.na(night.y)) 
  
  filtered_night_join <- filtered_night_join %>%
    inner_join(filtered_night_join, by = c("night.x" = "night.y"))
  
  new_comb <- filtered_night_join %>% 
    transmute(whole_comb = paste(comb.x, comb.y)) 
  
  remove_doubles <- remove.doubles(new_comb)
  new_comb <- new_comb %>% transmute(comb = remove_doubles) 
  
  collision_free <- rbind(new_comb, inner) %>% 
    mutate(collision = collisions.all.comb(comb)) %>% 
    filter(collision == F)
  
  possible_combs <- collision_free %>% 
    transmute(comb, possible = possible.all.comb(comb)) %>% 
    filter(possible == T) %>% 
    transmute(night = 0, comb) %>%
    arrange(comb)
  
  possible_combs
}








is.collision <- function(vec_of_couples){
     vec_of_couples <- vec_of_couples[[1]] # it's a list with one element: the vector of couples
     vec_of_couples <- unlist(str_split(vec_of_couples, fixed(" ")))
     collision <- FALSE
     special_person_detect <- sapply(vec_of_couples, str_detect, pattern = special_person)
     if(sum(special_person_detect) == 1){ 
         # the person she is with could have a second match, so don't set collision to true
           # instead: remove her couple from the vector 
           vec_of_couples <- vec_of_couples[-which(special_person_detect)]
         }
     solo_names <- unlist(str_split(vec_of_couples, fixed("+")))
     if(length(solo_names) > length(unique(solo_names))){
         collision <- TRUE
       }
     collision
   }

collisions.all.comb <- function(whole_comb){
  sapply(whole_comb, is.collision)
}


check.upper.bound <- function(night, comb, upper_bound){
  good_check <- FALSE
  if(length(intersect(night, comb)) <= upper_bound){
    good_check <- TRUE
  } 
  good_check
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

possible.all.comb <- function(whole_comb){
  sapply(whole_comb, is.possible)
}


combinations <- function(nights, max_cap){
  start_sub <- CombSet(sort(nights[[1]]), m=max_cap[1]) 
  dfx <- df.from.sub(start_sub,1)
  for(i in 2:length(nights)){
    night <- nights[[i]]
    sub <- CombSet(sort(night), m=max_cap[i])
    dfy <- df.from.sub(sub,i)
    dfx <- join.nights(dfx,dfy)
  }
  dfx
}

vier_zweier = c("Kathleen+Salvo", "Jill+Manu", "Steffi+Jamy", "Sarah+Alex")
fuenf_zweier = c("Kathleen+Manu",	"Jill+Danilo",	"Steffi+Alex", "Aurelia+Diogo", "Jacky+Salvo"	)
zwei_zweier = c("Kathleen+Manu",	"Jill+Jamy" , "Sarah+Danilo", "Aurelia+Diogo", "Jacky+Salvo", "Finnja+Alex")
sechs_einer = c("Kathleen+Manu", "Jill+Eugen", "Sarah+Josua", 	"Jacky+Alex", "Finnja+Jamy", "Walentina+Salvo")
eins_dreier = c("Kathleen+Manu",	"Jill+Jamy",	"Steffi+Danilo",	"Sarah+Josua",	"Aurelia+Diogo",	"Jacky+Salvo","Walentina+Eugen")
drei_dreier = c("Vanessa+Jamy", "Kathleen+Manu", "Jill+Tommy","Steffi+Alex",	"Sarah+Josua",	"Aurelia+Diogo","Walentina+Salvo","Melina+Danilo")
nights <- list(vier_zweier, fuenf_zweier, zwei_zweier, sechs_einer, eins_dreier, drei_dreier)
special_person = "Vanessa"
max_cap <- c(2,2,2,1,3,3)
vier_zweier_sub <- CombSet(nights[[1]], m=max_cap[1])
fuenf_zweier_sub <- CombSet(sort(fuenf_zweier), m=max_cap[2])
zwei_zweier_sub <- CombSet(sort(zwei_zweier), m = max_cap[3])
sechs_einer_sub <- CombSet(sort(sechs_einer), m = max_cap[4])

df5 <- df.from.sub(fuenf_zweier_sub,5)
df2 <- df.from.sub(zwei_zweier_sub,2)
df4 <- df.from.sub(vier_zweier_sub,4)
df6 <- df.from.sub(sechs_einer_sub,6)

df42 <- join.nights(df4,df2)
df425 <- join.nights(df42,df5)


df25<-join.nights(df2,df5)
df254<-join.nights(df25,df4)
df52<-join.nights(df5,df2)

combs2 <- combinations(nights,max_cap)
