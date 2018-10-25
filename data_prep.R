# load required packages
library(utils)
library(readr)
library(dplyr)
library(tidyr)

# download and unzip Twitter IRA tweet data, manually or using the commented out code below
# download.file("https://storage.googleapis.com/twitter-election-integrity/hashed/ira/ira_tweets_csv_hashed.zip", destfile="ira_tweets.zip")
# unzip("ira_tweets.zip", exdir = "data")

# load, filter for English language tweets, create field for join to Clemson data
en_ira_tweets <- read_csv("data/ira_tweets_csv_hashed.csv", col_types = cols(tweetid = col_character())) %>%
  filter(tweet_language == "en") %>%
  mutate(handle = tolower(user_screen_name))

# load Clemson tweets
clemson <- data_frame()
for (n in c(1:13)) {
  tmp <- read_csv(paste0("https://raw.githubusercontent.com/fivethirtyeight/russian-troll-tweets/master/IRAhandle_tweets_",n,".csv"), col_types = cols(
    .default = col_character(),
    following = col_integer(),
    followers = col_integer(),
    updates = col_integer(),
    retweet = col_integer(),
    new_june_2018 = col_integer()
  ))
  clemson <- bind_rows(clemson,tmp)
}
rm(tmp,n)

# joins and cleanup
clemson_tweetids <- clemson %>%
  select(tweet_id,account_category) %>%
  rename(tweetid = tweet_id) %>%
  unique()

clemson_handles <- clemson  %>%
  select(author,account_category) %>%
  mutate(handle = tolower(author)) %>%
  select(handle,account_category) %>%
  unique()

en_ira_tweets<- left_join(en_ira_tweets, clemson_tweetids, by = "tweetid") %>%
  rename(account_category1 = account_category) %>%
  arrange(userid)

en_ira_tweets <- left_join(en_ira_tweets, clemson_handles, by = "handle") %>%
  rename(account_category2 = account_category)

en_ira_tweets <- en_ira_tweets %>%
  mutate(account_category = case_when(is.na(account_category1) ~ account_category2,
                                      !is.na(account_category1) ~ account_category1)) %>% 
  select(-account_category1, -account_category2, -handle)

en_ira_tweets <- en_ira_tweets %>%
  group_by(userid) %>%
  fill(account_category, .direction = "down") %>%
  fill(account_category, .direction = "up") %>%
  mutate(account_category = case_when(is.na(account_category) ~ "Unknown",
                                      !is.na(account_category) ~ account_category)) %>%
  ungroup()


# write data to file
write_csv(en_ira_tweets, "data/en_ira_tweets.csv", na="")

