library(tidyverse)
library(dplyr)
library(anytime)

directory <- "modified_tosu.csv"
file <- read.csv(directory)

check_file_info <- function(file){
  #class(file)
  #colnames(file)
  #ncol(file)
  #nrow(file)
  str(file)
  head(file)
}

mod_table <- function(){
  date <- as.Date(file$dates, "%m/%d/%Y")
  ko_time <- anytime(paste(date,as.character(file$ko)))
  x <- as.character(file$attend)
  attends <- as.numeric(x)
  scores_home <- as.integer(substr(as.character(file$score), start = 1, stop = 1))
  scores_away <- as.integer(substr(as.character(file$score), start = 3, stop = 3))
  homes <- factor(unlist(lapply(file$home, gsub, pattern = "\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t", replacement = "", fixed = TRUE)))
  aways <- factor(unlist(lapply(file$away, gsub, pattern = "\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t", replacement = "", fixed = TRUE)))

  dat <- file %>% mutate(date,ko_time,attends, scores_home, scores_away, aways, homes) %>% 
                  filter(year %in% c(2017,2018,2019) & tournaments == 'J1' & !is.na(attends) & !is_empty(scores_home) & !is.na(scores_home)) %>% 
                  select(year,date, ko_time, tournaments, homes, scores_home, scores_away, aways)
  
  return(dat)
}


is_win <- function(dat){
  i <- 1
  result <- list()
  count_win <- 0
  count_lose <- 0
  count_draw <- 0
  
  for(t in dat$date){
    if(dat$scores_home[i] > dat$scores_away[i]){
      #print('a')
      if(dat$home[i]=='Tosu'){
        count_win <- count_win + 1
        result[i] <- "win"
      }
      else{
        count_lose <- count_lose + 1
        result[i] <- "lose"
      }
      i<-i+1
    }
    else if(dat$scores_home[i] < dat$scores_away[i]){
      if(dat$away[i]=='Tosu'){
        count_win <- count_win + 1
        result[i] <- "win"
      }
      else{
        count_lose <- count_lose + 1
        result[i] <- "lose"
      }
      i<-i+1
    }
    else{
      count_draw <- count_draw + 1
      result[i] <- "draw"
      i<-i+1
    }
  }
  
  result <- unlist(result)
  result <- as.factor(result)
  dat_result <- mutate(dat, result)
  
  print(count_win)
  print(count_lose)
  print(count_draw)
  
  print(length(dat_result$date) == sum(count_win,count_lose,count_draw))
  
  df <- data.frame(win=c(count_win), lose=c(count_lose), draw=c(count_draw))
  print(prop.table(df))
  
  return(dat_result)
}

