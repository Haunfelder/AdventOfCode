library(tidyverse)
library(stringr)

df = readLines("day9.txt")
head(df)

df = tibble(x = readLines("day9.txt")) %>%
  mutate(x = as.numeric(x))


## Code -------------



for(i in 26:nrow(df)){
  inds = which(colSums(combn(df$x[(i - 25):(i-1)], 2)) == df$x[i])
  if(length(inds) == 0){
    print(i)
  }
}


for(i in 2:20){
  seqs = lapply(as.list(1:(nrow(df) - i + 1)), function(x){x:(x + i - 1)})
  for(j in 1:length(seqs)){
    s = sum(df$x[seqs[[j]]])
    if(s == df$x[556]){print(seqs[[j]])}
  }
}
