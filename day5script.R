library(tidyverse)
df = read.table("day5.txt")

test = df %>% 
  mutate(
    row = str_sub(V1, 1, 7),
    col = str_sub(V1, 8, 10)
  ) %>%
  arrange(row, desc(col)) %>%
  rowwise() %>%
  mutate(
    col_split = strsplit(col, ""),
    row_split = strsplit(row, "")
  ) %>%
  rowwise() %>%
  mutate(
    col_ans = id_func(col_split, b = 8),
    row_ans = id_func(row_split)
  ) %>%
  ungroup() %>%
  mutate(
    ans = row_ans*8 + col_ans
  )

128*( 1- 1/2^5)*8 + 8*(1 - 1/2^2)

id_func = function(x, y = 0, i = 1, b = 128){
  #print(i)
  if(x[i] == "B" | x[i] == "R"){
    resp = y + (b)*(1/2^i)
  } else{
    resp = y 
  }
  if(i  == length(x)){
    return(resp) 
  } else {
    id_func(x, resp, i + 1, b = b)
  }
}
id_func(x = c('B', 'B'))
id_func(x = c('R', 'R'), b= 8)


test %>%
  arrange(ans) %>%
  mutate(
    diff = ans - lag(ans)
  ) %>%
  filter(diff == 2) %>% data.frame()


x = c('B', 'B')
sum(128*(1/2^((x == "B")* 1:length(x))))

id_func_manip = function(x, b = 128){
  sum(b*(1/2^((x == "B" | x == "R")* 1:length(x))))
}
id_func_manip(x = c('B', 'B'))
test_manip = df %>% 
  mutate(
    row = str_sub(V1, 1, 7),
    col = str_sub(V1, 8, 10)
  ) %>%
  arrange(row, desc(col)) %>%
  rowwise() %>%
  mutate(
    col_split = strsplit(col, ""),
    row_split = strsplit(row, "")
  ) %>%
  rowwise() %>%
  mutate(
    col_ans = id_func_manip(col_split, b = 8),
    row_ans = id_func_manip(row_split, b = 128)
  ) %>%
  ungroup() %>%
  mutate(
    ans = row_ans*8 + col_ans
  )
