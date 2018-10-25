# load required packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyr)
library(stringr)
library(scales)
library(DT)
library(lubridate)

# set time zone to UTC
Sys.setenv(TZ = "UTC")

# regexes for parsing tweets
replace_reg <- "https?://[^\\s]+|&amp;|&lt;|&gt;|\\bRT\\b"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

# load data
ira_tweets <- read_csv("data/ira_tweets.csv", col_types = cols(tweetid = col_character()))

ira_tweets <- ira_tweets %>%
  mutate(tweet_date = as.Date(tweet_time),
         tweet_month = month(tweet_time),
         tweet_year = year(tweet_time),
         tweet_lang2 = case_when(tweet_language == "ru" ~ "ru",
                                 tweet_language == "en" ~ "en",
                                 tweet_language != "en" & tweet_language != "ru" | is.na(tweet_language) ~ "other"))
                                
View(head(ira_tweets,1000))

# tweets by language
ira_languages <- ira_tweets %>%
  group_by(tweet_language) %>%
  count() %>%
  arrange(-n)


# tweets by language and day
ira_languages_day <- ira_tweets %>%
  group_by(tweet_day,tweet_lang2) %>%
  count()

ggplot(ira_languages_day, aes(x=tweet_date, y=n, color = tweet_lang2)) +
  geom_line(size=0.5) +
  theme_minimal()

# tweets by language and month
ira_languages_month <- ira_tweets %>%
  group_by(tweet_month,tweet_year,tweet_lang2) %>%
  count() %>%
  mutate(month = as.Date(paste0(tweet_year,"-",tweet_month,"-15")))

ggplot(ira_languages_month, aes(x=month, y=n, color = tweet_lang2)) +
  geom_line(size=0.5) +
  geom_point() +
  scale_y_continuous(labels = comma) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_family = "Proxima Nova Semibold", base_size = 16) +
  xlab("") +
  ylab("tweets") +
  theme(legend.position = "top")

# russian words/bigrams
ru_ira_tweets <- ira_tweets %>%
  filter(tweet_language == "ru")

ru_stopwords <- read_csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-ru/master/stopwords-ru.txt") %>%
  rename(word = c) # more here than in the tm package

# words 
ru_ira_tweets <- ru_ira_tweets %>% 
  mutate(tweet_text = str_replace_all(tweet_text, replace_reg, "")) %>%
  select(tweet_text,tweet_month,tweet_year) %>%
  unnest_tokens(word,tweet_text) %>%
  anti_join(ru_stopwords, by = "word")

View(head(ru_ira_tweets))

# top words overall
ru_ira_tweets_words <- ru_ira_tweets %>%
  group_by(word) %>%
  filter(!grepl("[[:digit:]]",word)) %>%
  count() %>%
  arrange(-n) %>%
  head(1000) %>%
  rename(mentions = n)

datatable(ru_ira_tweets_words)

# top words, by month
ru_ira_tweets_words_month  <- ru_ira_tweets %>%
  group_by(word,tweet_month,tweet_year) %>%
  filter(!grepl("[[:digit:]]",word)) %>%
  count() %>%
  ungroup() %>%
  group_by(tweet_month,tweet_year) %>%
  top_n(20) %>%
  arrange(tweet_year,tweet_month,-n) %>%
  rename(mentions = n) %>%
  mutate(month = paste0(tweet_month,"-",tweet_year)) %>%
  ungroup() %>%
  select(word,month,mentions)

datatable(ru_ira_tweets_words_month)
  
# bigrams
ru_ira_tweets <- ru_ira_tweets %>% 
  mutate(tweet_text = str_replace_all(tweet_text, replace_reg, "")) %>%
  select(tweet_text,tweet_month,tweet_year) %>%
  unnest_tokens(bigram, tweet_text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(ru_stopwords, by = c("first" = "word")) %>%
  anti_join(ru_stopwords, by = c("second" = "word")) %>%
  filter(!grepl("[[:digit:]]",first),
         !grepl("[[:digit:]]",second))

View(head(ru_ira_tweets))

# top bigrams overall
ru_ira_tweets_bigrams <- ru_ira_tweets %>%
  group_by(bigram) %>%
  filter(!grepl("[[:digit:]]",bigram)) %>%
  count() %>%
  arrange(-n) %>%
  head(1000) %>%
  rename(mentions = n)

ru_ira_tweets_bigrams <- ru_ira_tweets_bigrams %>%
  rename(mentions = tweets)

datatable(ru_ira_tweets_bigrams)

# top bigrams, by month
ru_ira_tweets_bigrams_month  <- ru_ira_tweets %>%
  group_by(bigram,tweet_month,tweet_year) %>%
  filter(!grepl("[[:digit:]]",bigram)) %>%
  count() %>%
  ungroup() %>%
  group_by(tweet_month,tweet_year) %>%
  top_n(20) %>%
  arrange(tweet_year,tweet_month,-n) %>%
  rename(mentions = n) %>%
  mutate(month = paste0(tweet_month,"-",tweet_year)) %>%
  ungroup() %>%
  select(bigram,month,mentions)

datatable(ru_ira_tweets_bigrams_month)

# english language tweets
en_ira_tweets <- ira_tweets %>%
  filter(tweet_language == "en") %>%
  mutate(handle = tolower(user_screen_name))

glimpse(en_ira_tweets)

# clemson handles
clemson <- read_csv("clemson_handles.csv") %>%
  mutate(handle = tolower(author)) %>%
  select(-author)

# join
en_ira_tweets <- left_join(en_ira_tweets,clemson)

en_ira_handles_categories <- en_ira_tweets %>%
  select(handle,account_category) %>%
  unique()

top_en_tweets <- en_ira_tweets %>%
  arrange(-retweet_count) %>%
  head(1000) %>%
  select(user_reported_location,user_display_name,user_screen_name,account_category,user_profile_description,follower_count,following_count,account_creation_date,tweet_text,is_retweet,quote_count,reply_count,like_count,retweet_count,tweet_date,tweet_month,tweet_year)

datatable(top_en_tweets)

en_ira_account_stats <- en_ira_tweets %>%
  group_by(userid,user_display_name,user_screen_name,account_category,user_profile_description) %>%
  summarize(mean_followers = mean(follower_count,na.rm=T),
            tweets = n(),
            total_retweets = sum(retweet_count, na.rm=T),
            mean_retweets = round(mean(retweet_count, na.rm=T),2)) %>%
  arrange(-total_retweets)

datatable(en_ira_account_stats)

en_ira_category_stats <- en_ira_tweets %>%
  group_by(account_category) %>%
  summarize(tweets = n(),
            total_retweets = sum(retweet_count, na.rm=T),
            mean_retweets = round(mean(retweet_count, na.rm=T),2)) %>%
  arrange(-total_retweets)

datatable(en_ira_category_stats)

# load clemson tweets
clemson_tweets <- read_csv("data/IRAhandle_tweets_v3.csv", col_types = cols(
                                                              tweet_id = col_character(),
                                                              alt_external_id = col_character(),
                                                              external_author_id = col_character()
                                                              ))

# join to en_ira_tweets
clemson_tweetids <- clemson_tweets %>%
  select(tweet_id,account_category) %>%
  rename(tweetid = tweet_id) %>%
  unique()

glimpse(clemson_tweets)

clemson_handles <- clemson_tweets %>%
  select(author,account_category) %>%
  mutate(handle=tolower(author)) %>%
  select(handle,account_category) %>%
  unique()
  

# remove old join
en_ira_tweets <- en_ira_tweets %>%
  select(-account_category1)

glimpse(en_ira_tweets)

# new join
en_ira_tweets <- left_join(en_ira_tweets,clemson_tweetids) %>%
  rename(account_category1 = account_category) %>%
  left_join(clemson_handles)

# cleanup 
en_ira_tweets <- en_ira_tweets %>%
  rename(account_category2 = account_category) %>%
  mutate(account_category = case_when(is.na(account_category1) ~ account_category2,
                                      !is.na(account_category1) ~ account_category1))

en_ira_tweets <- en_ira_tweets %>%
  mutate(test = account_category) %>%
  group_by(handle) %>%
  fill(test)

en_ira_tweets <- en_ira_tweets %>%
  select(1:35,39) %>%
  ungroup()

en_ira_tweets <- en_ira_tweets %>%
  rename(account_category = test)

glimpse(en_ira_tweets)

top_en_tweets <- en_ira_tweets %>%
  arrange(-retweet_count) %>%
  head(1000) %>%
  select(user_reported_location,user_display_name,user_screen_name,account_category,user_profile_description,follower_count,following_count,account_creation_date,tweet_text,is_retweet,quote_count,reply_count,like_count,retweet_count,tweet_date,tweet_month,tweet_year)

datatable(top_en_tweets)

en_ira_account_stats <- en_ira_tweets %>%
  group_by(userid,user_display_name,user_screen_name,account_category,user_profile_description) %>%
  summarize(mean_followers = mean(follower_count,na.rm=T),
            tweets = n(),
            total_retweets = sum(retweet_count, na.rm=T),
            mean_retweets = round(mean(retweet_count, na.rm=T),2)) %>%
  arrange(-total_retweets)

datatable(en_ira_account_stats)

en_ira_category_stats <- en_ira_tweets %>%
  group_by(account_category) %>%
  summarize(tweets = n(),
            total_retweets = sum(retweet_count, na.rm=T),
            mean_retweets = round(mean(retweet_count, na.rm=T),2)) %>%
  arrange(-total_retweets)

datatable(en_ira_category_stats)

pre_election <- en_ira_tweets %>%
  filter(tweet_date >= "2015-11-08" & tweet_date <= "2016-11-07")

pre_election_category_stats <- pre_election %>%
  group_by(account_category) %>%
  summarize(accounts = n_distinct(handle),
            tweets = n(),
            total_retweets = sum(retweet_count, na.rm=T),
            mean_retweets = round(mean(retweet_count, na.rm=T),2)) %>%
  arrange(-total_retweets)

datatable(pre_election_category_stats)

post_election <- en_ira_tweets %>%
  filter(tweet_date >= "2016-11-09" & tweet_date <= "2017-11-08")

post_election_category_stats <- post_election %>%
  group_by(account_category) %>%
  summarize(accounts = n_distinct(handle),
            tweets = n(),
            total_retweets = sum(retweet_count, na.rm=T),
            mean_retweets = round(mean(retweet_count, na.rm=T),2)) %>%
  arrange(-total_retweets)

datatable(post_election_category_stats)

right_trolls <- en_ira_account_stats %>%
  filter(account_category == "RightTroll")

tweet_type <- en_ira_tweets %>%
  group_by(tweet_year,account_category,is_retweet) %>%
  count() %>%
  filter(grepl("Left|Right", account_category))



top_en_orig_tweets <- en_ira_tweets %>%
  filter(is_retweet == "false") %>%
  arrange(-retweet_count) %>%
  head(1000) %>%
  select(user_reported_location,user_display_name,user_screen_name,account_category,user_profile_description,follower_count,following_count,account_creation_date,tweet_text,is_retweet,quote_count,reply_count,like_count,retweet_count,tweet_date,tweet_month,tweet_year)

datatable(top_en_orig_tweets)


pre_election_orig <- en_ira_tweets %>%
  filter(tweet_date >= "2015-11-08" & tweet_date <= "2016-11-07" & is_retweet == "false")

pre_election_orig_category_stats <- pre_election_orig %>%
  group_by(account_category) %>%
  summarize(accounts = n_distinct(handle),
            tweets = n(),
            total_retweets = sum(retweet_count, na.rm=T),
            mean_retweets = round(mean(retweet_count, na.rm=T),2)) %>%
  arrange(-total_retweets)

datatable(pre_election_orig_category_stats)

post_election_orig <- en_ira_tweets %>%
  filter(tweet_date >= "2016-11-09" & tweet_date <= "2017-11-08" & is_retweet == "false")

post_election_orig_category_stats <- post_election_orig %>%
  group_by(account_category) %>%
  summarize(accounts = n_distinct(handle),
            tweets = n(),
            total_retweets = sum(retweet_count, na.rm=T),
            mean_retweets = round(mean(retweet_count, na.rm=T),2)) %>%
  arrange(-total_retweets)

datatable(post_election_orig_category_stats)

top_tweet_text <- en_ira_tweets %>%
  filter(grepl("Daily reminder that the most educated First Lady",tweet_text, ignore.case = T))


retweet_bot_ids <- intersect(unique(en_ira_tweets$retweet_userid),unique(en_ira_tweets$userid))

bot_retweets <- en_ira_tweets %>%
  filter(is_retweet == "true" & retweet_userid %in% retweet_bot_ids) 

bot_retweets_pre_election_summary <- bot_retweets %>%
  filter(tweet_date >= "2015-11-08" & tweet_date <= "2016-11-07") %>%
  group_by(account_category) %>%
  count()

bot_retweets_post_election_summary <- bot_retweets %>%
  filter(tweet_date >= "2016-11-09" & tweet_date <= "2017-11-08") %>%
  group_by(account_category) %>%
  count()















