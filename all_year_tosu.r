library(tidyverse)
library(dplyr)
library(anytime)

file <- read.csv("modified_tosu.csv")

date <- as.Date(file$dates, "%m/%d/%Y")
ko_time <- anytime(paste(date,as.character(file$ko)))
x <- as.character(file$attend)
attends <- as.numeric(x)
scores_home <- as.integer(substr(as.character(file$score), start = 1, stop = 1))
scores_away <- as.integer(substr(as.character(file$score), start = 3, stop = 3))
homes <- factor(unlist(lapply(file$home, gsub, pattern = "\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t", replacement = "", fixed = TRUE)))
aways <- factor(unlist(lapply(file$away, gsub, pattern = "\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t", replacement = "", fixed = TRUE)))


dat <- file %>% mutate(date,ko_time,attends, scores_home, scores_away, aways, homes) %>% 
  filter(year == 2019 & tournaments == 'J1' & !is.na(attends) & !is.na(scores_home)) %>% 
  select(date, ko_time, tournaments, homes, scores_home, scores_away, aways)

i <- 1
count_win <- 0
count_lose <- 0
count_draw <- 0

for(t in dat$date){
  if(dat$scores_home[i] > dat$scores_away[i]){
    #print('a')
    if(dat$home[i]=='Tosu'){
      count_win <- count_win + 1
      print('win')
    }
    else{
      count_lose <- count_lose + 1
      print('lose')
    }
    i<-i+1
  }
  else if(dat$scores_home[i] < dat$scores_away[i]){
    #print('b')
    if(dat$away[i]=='Tosu'){
      count_win <- count_win + 1
      print('win')
    }
    else{
      count_lose <- count_lose + 1
      print('lose')
    }
    i<-i+1
  }
  else{
    #print('c')
    print("draw")
    count_draw <- count_draw + 1
    i<-i+1
  }
}




#checking if the results are created from all rows
#length(dat$date) == sum(count_win,count_lose,count_draw)
#df <- data.frame(win=c(count_win), lose=c(count_lose), draw=c(count_draw))
#prop.table(df)
