library(tidyverse)
library(stringr)

df = readLines("day11.txt")
head(df)

df = tibble(x = readLines("day10.txt")) %>%
  mutate(x = as.numeric(x)) %>%
  arrange(x)
head(df) 

df = tibble(x = readLines("day10.txt")) %>%
  mutate(x = as.character(x))
head(df) 


## Code ---------------

mat = Reduce("rbind",strsplit(df, ""))

swap = function(mat){
  empty_mat = mat
  for(i in 1:dim(mat)[1]){
    for(j in 1:dim(mat)[2]){
      x = mat[max(c((i - 1), 1)):min(c((i + 1), dim(mat)[1])),
              max(c((j - 1),1)):min(c((j + 1), dim(mat)[2]))]
      states = as.vector(x)
      if(mat[i,j] == "L"){
        if(all(states %in% c("L", "."))){
          empty_mat[i, j] = "#"
        } 
      } else if(mat[i,j] == "#"){
        if(sum(states == "#") >= 5){
          empty_mat[i,j] = "L"
        }
      }
    }
  }
  return(empty_mat)
}
current_mat = mat
last_mat = matrix(".", nrow = dim(mat)[1], ncol = dim(mat)[2])
i = 0
while(!all(current_mat == last_mat)){
  last_mat = current_mat
  current_mat = swap(last_mat)
  i = i + 1
  print(i)
}

sum(current_mat == "#")

## Part 2 ---------------

mat = Reduce("rbind",strsplit(df, ""))

swap = function(mat){
  empty_mat = mat
  nonempty_inds = which(mat != ".", arr.ind = T)
  for(i in 1:dim(mat)[1]){
    for(j in 1:dim(mat)[2]){
      right = mat[i,min(c((j + 1), dim(mat)[2])):dim(mat)[2]]
      right = first(right[right != "."])
      left = mat[i,1:max(c((j - 1),1))]
      left = first(left[left != "."])
      
      up = mat[1:max(c((j - 1),1)), j ]
      up = first(up[up != "."])
      down = mat[min(c((j + 1), dim(mat)[2])):dim(mat)[1] , j]
      down = first(down[down != "."])
      
      
      
      x = mat[max(c((i - 1), 1)):min(c((i + 1), dim(mat)[1])),
              max(c((j - 1),1)):min(c((j + 1), dim(mat)[2]))]
      states = as.vector(x)
      if(mat[i,j] == "L"){
        if(all(states %in% c("L", "."))){
          empty_mat[i, j] = "#"
        } 
      } else if(mat[i,j] == "#"){
        if(sum(states == "#") >= 6){
          empty_mat[i,j] = "L"
        }
      }
    }
  }
  return(empty_mat)
}
current_mat = mat
last_mat = matrix(".", nrow = dim(mat)[1], ncol = dim(mat)[2])
i = 0
while(!all(current_mat == last_mat)){
  last_mat = current_mat
  current_mat = swap(last_mat)
  i = i + 1
  print(i)
}

sum(current_mat == "#")
