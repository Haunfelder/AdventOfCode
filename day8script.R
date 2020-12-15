library(tidyverse)
library(stringr)

df = readLines("day8.txt")
head(df)

df = tibble(x = readLines("day8.txt"))

df = read.table("day8.txt", sep = "")
head(df)


## Code ------------

test = df %>% 
  rowwise() %>%
  mutate(
    first = strsplit(x, " ")[[1]][1],
    second = strsplit(x, " ")[[1]][2]
  ) %>%
  ungroup() %>%
  mutate(
    ind = 1:n()
  )

ind_l = numeric()
match = F
i = 1
iter = 2
acc = 0
ind_l[1] = 1
acc_l = numeric()
acc_l[1] = 0
while(!match){
  if(test$first[i] == 'jmp'){
    i = i + as.numeric(test$second[i])
  } else if(test$first[i] == 'acc'){
    acc = acc + as.numeric(test$second[i])
    i = i + 1
  } else {
    i = i + 1
  }
  print(i)
  if(i <= 0){
    i = ((i - 1) %% nrow(test)) + 1
  } else {
    i = (i %% nrow(test))
  }
  ind_l[iter] = i
  acc_l[iter] = acc
  match = any(ind_l[-iter] == i)
  iter = iter + 1
}



### Part 2 ----------------


which(test$first == 'nop' & (as.numeric(test$second) == nrow(test):1))



swap = which(test$first == 'jmp')
for(j in 1:length(swap)){
  test_mod = test
  test_mod[swap[j], 'first'] = 'nop'
  
  ind_l = numeric()
  match = F
  i = 1
  iter = 2
  acc = 0
  ind_l[1] = 1
  acc_l = numeric()
  acc_l[1] = 0
  while(!match){
    if(test_mod$first[i] == 'jmp'){
      i = i + as.numeric(test_mod$second[i])
    } else if(test_mod$first[i] == 'acc'){
      acc = acc + as.numeric(test_mod$second[i])
      i = i + 1
    } else {
      i = i + 1
    }
    if(i == (nrow(test_mod) + 1)){
      print(j)
    }
    if(i <= 0){
      i = ((i - 1) %% nrow(test_mod)) + 1
    } else if(i == nrow(test_mod)){
      i = i
    } else {
      i = (i %% nrow(test_mod))
    }
    ind_l[iter] = i
    acc_l[iter] = acc
    match = any(ind_l[-iter] == i)
    iter = iter + 1
  }
}


swap = which(test$first == 'nop')
for(j in 1:length(swap)){
  test_mod = test
  test_mod[swap[j], 'first'] = 'jmp'
  
  ind_l = numeric()
  match = F
  i = 1
  iter = 2
  acc = 0
  ind_l[1] = 1
  acc_l = numeric()
  acc_l[1] = 0
  while(!match){
    if(test_mod$first[i] == 'jmp'){
      i = i + as.numeric(test_mod$second[i])
    } else if(test_mod$first[i] == 'acc'){
      acc = acc + as.numeric(test_mod$second[i])
      i = i + 1
    } else {
      i = i + 1
    }
    if(i == (nrow(test_mod) + 1)){
      print(j)
    }
    if(i <= 0){
      i = ((i - 1) %% nrow(test_mod)) + 1
    } else if(i == nrow(test_mod)){
      i = i
    } else {
      i = (i %% nrow(test_mod))
    }
    ind_l[iter] = i
    acc_l[iter] = acc
    match = any(ind_l[-iter] == i)
    iter = iter + 1
  }
}
