library(tidyverse)
library(stringr)

df = readLines("day10.txt")
head(df)

df = tibble(x = readLines("day10.txt")) %>%
  mutate(x = as.numeric(x)) %>%
  arrange(x)
head(df) 

df = tibble(x = readLines("day10.txt")) %>%
  mutate(x = as.character(x))
head(df) 


## COde ------------
df_mod = data.frame(x = c(0, df$x, max(df$x) + 3))
#df[which.max(df$x),1] = max(df$x) + 3
df_mod %>%
  mutate(
    y = lag(x),
    diff = x - y
  )  %>%
  summarize(
    sum(diff == 3, na.rm = T)*sum(diff == 1, na.rm = T)
  )


## Part 2 ------


df = tibble(x = readLines("day10.txt")) %>%
  mutate(x = as.numeric(x)) %>%
  arrange(x)
df_mod = data.frame(x = c(0, df$x, max(df$x) + 3))
A = I(outer(df_mod$x, df_mod$x, "-") <=3 &  
        outer(df_mod$x, df_mod$x, "-") >= 1)*1

library(matrixcalc)
walks = lapply(floor(nrow(df_mod)/3):nrow(df_mod),
               function(x){matrix.power(t(A), x)})
options(scipen = 999)
Reduce("+", walks)[1, nrow(df_mod)]
