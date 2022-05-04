library(DescTools)
library(stringr)

subs.as.list <- function(sub){
  sub_list <- list()
  for(i in 1:nrow(sub)){
    row <- sub[i,]
    sub_list[length(sub_list) + 1] <- list(row)
  }
  sub_list
}

check.upper.bound <- function(night, comb, ncol_test_combs){
  good_check <- FALSE
  if(length(intersect(night, comb)) <= ncol_test_combs){
    good_check <- TRUE
  } 
  good_check
}

go.further <- function(test_comb, whole_comb, ncol_test_combs){
  go_further <- TRUE
  if(length(whole_comb) > 0){
       ### check for max cap
      for(i in 1:length(nights)){
        if(!check.upper.bound(nights[[i]], union(whole_comb, test_comb), max_cap[i])){
          #print(i)
          #print(test_comb)
          go_further <- FALSE
          break
        }
      }
    
    if(go_further != FALSE){
      solo_names <- unlist(str_split(whole_comb, ", "))
      for(x in test_comb){
          detect_doubles_couple <- sapply(whole_comb, str_detect, x)
          if(sum(detect_doubles_couple) > 0){ # genau dieses couple ist schon in der liste
            #print("ist drin")
            next
          }
          else {
            if(sum(sapply(x, str_detect, solo_names)) > 0){# mind. ein couple ist schon in der liste, das einen namen von dem test couple enthält
              #print("name schon besetzt")
              if(!str_detect(x, "Vanessa")){
                go_further <- FALSE
              }
              
            }
            
          }
      }
    }
  }
  go_further
}


fusion.combs <- function(working_comb, test_combs){
  new_list = list()
  res = sapply(test_combs, go.further, whole_comb = working_comb)
  if(sum(res) > 0){
    valid_combs <- test_combs[res]
    if(length(valid_combs) > 0){
      for(valid_comb_item in valid_combs){
        new_list[length(new_list) + 1] <- list(union(working_comb, valid_comb_item))
      }
    } 
  }
  new_list
}


make.new.list <- function(sapply_result){
  new_list <- list()
  for(item in sapply_result){
    if(length(item) > 0){
      for(sub_item in item){
        if(length(sub_item) == 1){
          new_list <- sapply_result
          break
        }
        new_list[length(new_list) + 1] <- list(sub_item)
      }
    }
  }
  new_list
}

combinations <- function(nights, max_cap){
  start_night <- nights[[1]]
  start_sub <- CombSet(start_night[1:length(start_night)], m=max_cap[1]) 
  sub_list <- list()
  for(i in 2:length(nights)){
    night <- nights[[i]]
    sub <- CombSet(night[1:length(night)], m=max_cap[i])
    sub_list[[length(sub_list) + 1]] <- subs.as.list(sub)
  }
  
  test_combs <- subs.as.list(start_sub)
  res <- sapply(test_combs, go.further, whole_comb = c())
  comb_list = test_combs[res]
  
  for(subs in sub_list){
    new_list <- make.new.list(sapply(comb_list, fusion.combs, test_combs = subs))
    comb_list <- new_list
  }
  comb_list
}

vier_zweier = c("Kathleen, Salvo", "Jill, Manu", "Steffi, Jamy", "Sarah, Alex")
fuenf_zweier = c("Kathleen, Manu",	"Jill, Danilo",	"Steffi, Alex", "Aurelia, Diogo", "Jacky, Salvo"	)
zwei_zweier = c("Kathleen, Manu",	"Jill, Jamy" , "Sarah, Danilo", "Aurelia, Diogo", "Jacky, Salvo", "Finnja, Alex")
sechs_einer = c("Kathleen, Manu", "Jill, Eugen", "Sarah, Josua", 	"Jacky, Alex", "Finnja, Jamy", "Walentina, Salvo")
eins_dreier = c("Kathleen, Manu",	"Jill, Jamy",	"Steffi, Danilo",	"Sarah, Josua",	"Aurelia, Diogo",	"Jacky, Salvo","Walentina, Eugen")
drei_dreier = c("Vanessa, Jamy", "Kathleen, Manu", "Jill, Tommy","Steffi, Alex",	"Sarah, Josua",	"Aurelia, Diogo","Walentina, Salvo","Melina, Danilo")
nights <- list(vier_zweier, fuenf_zweier, zwei_zweier, sechs_einer, eins_dreier, drei_dreier)
max_cap <- c(2,2,2,1,3,3)
combs <- combinations(nights, max_cap)

