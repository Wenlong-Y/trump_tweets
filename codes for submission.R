library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(tidytext)

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

yearsourcepercentage %>% ggplot(aes(year,npercentage,color=as.character(source)))+geom_line(aes(year,npercentage))+geom_point() + scale_x_continuous(breaks=seq(2009,2019,2)) + ggtitle("Fig. 3. Trump tweeting device by year") + labs(color="Device") + ylab("Percentage") + xlab("Year")


pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

ttwords <- ttweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word & !word %in% c("http","rt", "@realdonaldtrump", "tinyurl", "pqpfvm", "www") & !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", "")) 

ttyearword <- ttwords %>% group_by(year,word) %>% summarize(n=n()) %>% arrange(desc(n), .by_group= TRUE) 

ttyearwordtop <- ttyearword %>% top_n(10)

ttyeartop <- ttyearwordtop %>% summarize(nword = sum(n))

ttyearwordtop %>% summarize(nword = sum(n))

ttyearwordper <- full_join(ttyearwordtop,ttyeartop) %>% mutate(nper = n / nword)

saveGIF({
for(y in 2009:2019) {
  pic <- ttplot %>% filter(year == y) %>% ggplot(aes(nper, reorder(word,nper)), subset(year)) + geom_point(color="blue", size=2) +theme(axis.text.y = element_text(size = 12)) + xlab("Percentage") + ylab("") + ggtitle(paste("Fig. 4. Most common words used in Trump's tweets for", y))
  pic
  }
})

#in the unix shell at the project directory 
#echo "# Analyze tweet behavior of Donal Trump and the fav/retweet behaviors " >> README.md
#git init     #initialize a git directory on local project directory
#git add README.md
#git add trump_tweets_20191103.csv
#git commit -m "Commit README.md and raw data of Trump's tweets"
#git remote add origin https://github.com/Wenlong-Y/trump_tweets.git
#git push -u origin master
