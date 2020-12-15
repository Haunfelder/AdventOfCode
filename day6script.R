library(tidyverse)
library(stringr)

df = readLines("day6.txt")
head(df)

df = tibble(x = readLines("day6.txt"))

df = read.table("day6.txt", sep = "")
head(df)


## Code ---------

test= df %>% 
  mutate(
    group = cumsum(x == "")
  ) %>%
  group_by(
    group
  ) %>%
  summarize(tog = paste0(x, collapse = "")) %>%
  mutate(
    splits = strsplit(tog, "")
  ) 

sum(sapply(test$splits, function(x){length(unique(x))}))

## Part 2 -------------

df %>%
  mutate(
    group = cumsum(x == ""),
    splits = strsplit(x, "")
  ) %>%
  group_by(group) %>%
  summarize(all = intersect_func(splits)) %>%
  ungroup() %>%
  summarize(sum(all))
  

s = df %>%
  mutate(
    group = cumsum(x == ""),
    splits = strsplit(x, "")
  ) %>%
  filter(group == 5) %>%
  .$splits

intersect_func = function(x){
  #list_x = list(x)
  #print(Reduce(intersect, x[-1]))
  if(length(x) == 2){return(length(x[[2]]))}
  return(length(Reduce(intersect, x[-1])))
}
