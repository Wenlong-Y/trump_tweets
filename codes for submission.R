library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
all_tweets <- map(2009:2019, ~sprintf(url, .x)) %>%
  #alternatively
  #paste0("http://www.trumptwitterarchive.com/data/realdonaldtrump/",2009:2017,".json")
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  mutate(created_at = parse_date_time(created_at, "a b! d! H!:M!:S! z!* Y!")) %>%
  tbl_df()
all_tweets%>%head()

write.csv(all_tweets, "trump_tweets_20191103.csv")

all_tweets <- as_tibble(read.csv("trump_tweets_20191103.csv", stringsAsFactors=FALSE))

all_tweets <- all_tweets %>% mutate(datetime = as_datetime(created_at)) %>% mutate(year = year(datetime), month = month(datetime), day = day(datetime), hour = hour(datetime), minute = minute(datetime))

all_tweets %>% mutate(source = as.factor(source))

ttweets <- all_tweets %>% filter(source %in% c("Twitter for Android","Twitter for iPhone","Twitter Web Client"))

ttweets %>% ggplot(aes(source)) + geom_bar() + xlab("Devices") + ggtitle("Fig. 1. Trump tweeting devices")

ttweets %>% ggplot(aes(as.character(year), width = 1))+geom_bar() + ggtitle("Fig. 2. Trump tweets by year") + xlab("Year")

yearsource <- ttweets %>% group_by(year,source) %>% summarize(n=n())

year <- ttweets %>% group_by(year) %>% summarize(nyear=n())

full_join(yearsource,year)

yearsourcepercentage <- full_join(yearsource,year) %>% mutate(npercentage = n/nyear)



#in the unix shell at the project directory 
#echo "# Analyze tweet behavior of Donal Trump and the fav/retweet behaviors " >> README.md
#git init     #initialize a git directory on local project directory
#git add README.md
#git add trump_tweets_20191103.csv
#git commit -m "Commit README.md and raw data of Trump's tweets"
#git remote add origin https://github.com/Wenlong-Y/trump_tweets.git
#git push -u origin master
