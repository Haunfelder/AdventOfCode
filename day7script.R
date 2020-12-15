library(tidyverse)
library(stringr)

df = readLines("day7.txt")
head(df)

df = tibble(x = readLines("day7.txt"))


## Start over ---------------------

other_colors = function(x, coi = c('shiny gold')){
  ind = which(gsub(" bag", "", x) %in% coi)
  if((length(ind) > 0) & (min(ind) > 1)){
    list(gsub(" bag", "", x)[1])
  } else{
    return(list(''))
  }
}

gc_i = 'shiny gold'
last_length = 0
while(length(gc_i) != last_length){
  last_length = length(gc_i)
  test_i = df %>% 
    mutate(
      color = regmatches(x, gregexpr("(\\w+) (\\w+) bag",  x))
    ) %>%
    rowwise() %>%
    do(
      oc = other_colors(.$color, coi = gc_i)
    ) %>%
    filter(oc != '') %>%
    distinct(oc)
  gc_i = unique(c(gc_i, unlist(test_i$oc)))
  #print(gc_i)
}
length(gc_i[gc_i != 'shiny gold'])



## Part 2 ----------------

test_ic = df %>% 
  mutate(ind = 1:n()) %>%
  rowwise() %>%
  mutate(
    first_bag = stringr::str_extract(x, "(\\w+ \\w+) bag"),
    all_color = stringr::str_extract_all(x, "([0-9] \\w+ \\w+) bag")
  ) %>%
  ungroup() %>%
  unnest(cols = c(all_color), keep_empty = T) %>%
  mutate(
    quantity = as.numeric(gsub("([0-9]).*", "\\1", all_color)),
    bag_color = gsub("([0-9]) (\\w+) (\\w+).*", "\\2 \\3", all_color),
    first_bag = gsub("(\\w+) (\\w+) bag", "\\1 \\2", first_bag)
  ) %>%
  group_by(
    ind
  ) %>%
  mutate(
    order = 1:n()
  )

inside_colors = function(coi = 'shiny gold'){
  if(is.na(coi)){return(0)}
  df_filt = test_ic %>%
    filter(
      (first_bag == coi)
    ) %>%
    mutate(quantity = ifelse(is.na(quantity), 0, quantity))
  df_agg = df_filt %>% group_by(first_bag) %>% summarize(quantity = sum(quantity, na.rm = T))
  #df_filt = df_filt %>% 
  #  filter(!is.na(bag_color)) 
  return(
    sum(df_agg$quantity) + sum(sapply(1:nrow(df_filt), 
                             function(x){
                               df_filt$quantity[x]*inside_colors(df_filt$bag_color[x])
                               }))
  )
}


inside_colors(coi = 'shiny gold')


inside_colors = function(x, coi = 'shiny gold bag'){
  test_i = df %>% 
    mutate(
      ind = 1:n(),
      color = regmatches(x, regexpr("(\\w+) (\\w+) bag",  x)),
      all_color = regmatches(x, gregexpr("(\\w+) (\\w+) bag",  x))
    ) %>%
    filter(color == coi)
  resp = test_i$all_color[[1]][test_i$all_color[[1]] != coi]
  return(list("colors" = resp, "ind" = test_i$ind))
}
start_colors = inside_colors(coi = 'shiny gold bag')
inds = start_colors[['ind']]
current_colors = lapply(start_colors[['colors']], function(x){inside_colors(coi = x)})

while(!all(current_colors == 'no other bag')){
  current_colors = as.vector(unlist(current_colors))
  current_colors = current_colors[current_colors != 'no other bag']
  current_colors = sapply(current_colors, function(x){inside_colors(coi = x)})
}


for(i in 1:length(start_colors)){
  
}



length(gc_i[gc_i != 'shiny gold'])