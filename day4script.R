library(tidyverse)
library(stringr)

df = read.table("clipboard",sep = '')
head(df)
df = readClipboard("clipboard")
head(df)

x = readLines("day4.txt")
x[1]

###

strs = c('byr',
'iyr',
'eyr',
'hgt',
'hcl',
'ecl',
'pid'#, 
#'cid'
) 

seps = c(0, which(x == ""), length(x) + 1)
count = 0
for(i in 1:(length(seps) - 1)){
  fields = unlist(
    strsplit(
      x[(seps[i] + 1):(seps[i + 1] - 1)], " "
    )
  )
  logs = sapply(strs, function(x){
    str_detect(paste(fields, collapse = " "), x)
  })
  if(all(logs)){
  count = count + 1}
}


## Part 2------------



# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
# If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.


strs = c('byr',
'iyr',
'eyr',
'hgt',
'hcl',
'ecl',
'pid'#, 
#'cid'
) 

seps = c(0, which(x == ""), length(x) + 1)
count = 0
for(i in 1:(length(seps) - 1)){
  fields = unlist(
    strsplit(
      x[(seps[i] + 1):(seps[i + 1] - 1)], " "
    )
  )
  df = data.frame(fields = as.character(fields)) %>%
    mutate(fields = as.character(fields)) %>%
    rowwise() %>%
    mutate(
      first = strsplit(fields, "\\:")[[1]][1],
      sec = strsplit(fields, "\\:")[[1]][2]
    ) 
    
  logs = sapply(strs, function(x){
    str_detect(paste(fields, collapse = " "), x)
  })
  #print(df[df$first == 'ecl', 'sec']$sec)
  if(all(logs)){ 
    if(
     between(as.numeric(df[df$first == 'byr', 'sec']), 1920, 2002) &
     between(as.numeric(df[df$first == 'iyr', 'sec']), 2010, 2020) &
     between(as.numeric(df[df$first == 'eyr', 'sec']), 2020, 2030) &
     ((
       between(
         as.numeric(strsplit(df[df$first == 'hgt', 'sec']$sec, "cm")[[1]]), 150, 193) &
       grepl("cm", df[df$first == 'hgt', 'sec']$sec)
     ) | (
       
       between(
         as.numeric(strsplit(df[df$first == 'hgt', 'sec']$sec, "in")[[1]]), 59, 76) &
       grepl("in", df[df$first == 'hgt', 'sec']$sec)
     )) & 
     grepl('^#[0-9A-Fa-f]{6}$', df[df$first == 'hcl', 'sec']$sec) &
     (df[df$first == 'ecl', 'sec']$sec %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')) &
     grepl('^[0-9]{9}$', df[df$first == 'pid', 'sec']$sec) 
     ){
      count = count + 1
    }
  }
}
