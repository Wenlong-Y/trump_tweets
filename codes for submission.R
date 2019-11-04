library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)


url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
all_tweets <- map(2009:2019, ~sprintf(url, .x)) %>%
  #alternatively
  #paste0("http://www.trumptwitterarchive.com/data/realdonaldtrump/",2009:2017,".json")
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  mutate(created_at = parse_date_time(created_at, "a b! d! H!:M!:S! z!* Y!")) %>%
  tbl_df()
all_tweets%>%head()

write.csv(all_tweets, "trump_tweets_20191103.csv")

#in the unix shell at the project directory 
#echo "# Analyze tweet behavior of Donal Trump and the fav/retweet behaviors " >> README.md
#git init     #initialize a git directory on local project directory
#git add README.md
#git add trump_tweets_20191103.csv
#git commit -m "Commit README.md and raw data of Trump's tweets"
#git remote add origin https://github.com/Wenlong-Y/trump_tweets.git
#git push -u origin master
