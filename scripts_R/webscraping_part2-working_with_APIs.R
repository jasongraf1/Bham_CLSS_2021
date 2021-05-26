# =========================================================================
# file: webscraping_part2-working_with_APIs.R
# author: Jason Grafmiller
# date: 26/05/2021
#
# description:
# Code for using rvest to scrape data from a web page. Companion code
# for the UBham Corpus Linguistics Summer School session
# =========================================================================

# Load libraries ----------------------------------------------------------

library(tidyverse, quietly = T) # for convenient data wrangling and styling
library(tictoc) # for timing processes
library(here) # for creating consistent file paths
library(usethis) # for editing environment files

# R libraries for interfacing with APIs
library(RedditExtractoR) # for scraping reddit forums
library(rtweet) # for getting tweets
library(httr) # for interfacing with APIs in general
library(jsonlite) # for parsing JSON


# Working with API wrapper packages ---------------------------------------

## Reddit -----------------------------------------------------------------

# get threads on Spinosaurs
spinosaur_threads <- get_reddit(
  search_terms = "Spinosaur",
  page_threshold = 1, # number of pages to be searched
  subreddit = "Dinosaurs"
)

spinosaur_threads %>%
  glimpse()

spinosaur_threads %>%
  arrange(title) %>%
  select(num_comments, title, author, comment) %>%
  arrange(desc(num_comments)) # put most popular thread first

# graph the comment chains with `construct_graph()`
thread_chain <- spinosaur_threads %>%
  dplyr::filter(title == "[Question] Questions about the Spinosaurs.") %>%
  construct_graph(plot = TRUE)

# graph the user network
thread_network <- spinosaur_threads %>%
  dplyr::filter(title == "[Question] Questions about the Spinosaurs.") %>%
  user_network(include_author = TRUE, agg = TRUE)

thread_network$plot


## Twitter ----------------------------------------------------------------

# get tweets containing 'cheugy' . This should open a browser for authentication
cheugy_tweets <- rtweet::search_tweets(
  "cheugy lang:en", # the terms to search for
  n = 200, # the number of tweets to collect
  include_rts = FALSE # don't include retweets
)

names(cheugy_tweets)

# more exapansive search for two possible terms
cheugy_tweets <- rtweet::search_tweets(
  q = "cheugy OR cheug lang:en", # the terms to search for
  n = 200, # the number of tweets to collect
  include_rts = FALSE # don't include retweets
)

# stream tweets for 30 seconds
rt <- stream_tweets(
  q = "cheugy OR cheug lang:en",
  timeout = 30
)

# DO NOT RUN THIS
# stream tweets for a week (60 secs * 60 mins * 24 hours *  7 days)
# stream_tweets(
#   q = "cheugy OR cheug lang:en",
#   timeout = 60 * 60 * 24 * 7,
#   file_name = here("data_raw", "live_cheugy_tweets.json"),
#   parse = FALSE
# )

# get user IDs of accounts followed by @Rbloggers
rblog_friends <- get_friends("@Rbloggers", n = 1000)

# get more information on users
rblog_friends %>%
  pull(user_id) %>% # pull out the column as a vector
  lookup_users()

# get user IDs of accounts following @Rbloggers
rblog_followers <- get_followers("@Rbloggers", n = 1000)

rblog_followers %>%
  pull(user_id) %>% # pull out the column as a vector
  lookup_users()

### Get timelines

# you should get an error:
bbc_tml <- get_timelines("@BBCNews", n = 100)

# You need to create a developer app, and get the authentication keys and tokens
# yourself.
# make list of information
jgtwitterapp_keylist <- list(
  api_key = "xxxxxxxxxxx",
  api_secret = "xxxxxxxxxxx",
  access_token = "xxxxxxxxxxx",
  access_secret = "xxxxxxxxxxx",
  bearer_token = "xxxxxxxxxxx"
)
# save to file
saveRDS(jgtwitterapp_keylist, here("keys", "jgtwitterapp_keys.rds"))

# this can be loaded any time
jgtwitterapp_keylist <- readRDS(here("keys", "jgtwitterapp_keys.rds"))

# create token with personal keys
my_token <- create_token(
  app = "jgtwitterapp", # the name of my app
  consumer_key = jgtwitterapp_keylist$api_key,
  consumer_secret = jgtwitterapp_keylist$api_secret,
  access_token = jgtwitterapp_keylist$access_token,
  access_secret = jgtwitterapp_keylist$access_secret
)

# Now you should be able to get the timeline for a user account.
bbc_tml <- get_timeline(
  "@BBCNews",
  n = 100,
  token = my_token # include your token here
)

bbc_tml

### Locating tweets

# List of cities in {rtweet}
rtweet:::citycoords

# Get coords for Birmingham
lookup_coords("birmingham england")

bham_tweets <- search_tweets(
  q = "lang:en",
  n = 1000,
  include_rts = FALSE, # don't include retweets
  geocode = lookup_coords("birmingham england")
)

bham_tweets %>%
  select(screen_name, location, place_full_name, geo_coords)

# look at the frequecy of locations in our dataset.
bham_tweets %>%
  count(location, sort = T)

# add latitude and longitude to the tweets, if possible
bham_tweets <- lat_lng(bham_tweets)
bham_tweets %>%
  select(lat, lng)


# Working with general APIs -----------------------------------------------

# endpoint path of the 'yo momma jokes' API
ym_path <- "https://yomomma-api.herokuapp.com/jokes"

# request ten jokes
ym_request <- GET(
  url = ym_path,
  query = list(count = 10) # the number of jokes to get
)

ym_request

# We can check whether our request worked just to be sure.
http_status(ym_request)

# Now we can extract the content.
ym_content <- content(ym_request, as = "text", encoding = "UTF-8")
ym_content

# parse the JSON data.
# flatten tells it to create a single unnested dataframe
ym_jokes_df <- fromJSON(ym_content, flatten = TRUE) %>%
  data.frame()

ym_jokes_df

## Cat facts ------------------------------------------------------------

cat_path <- "https://cat-fact.herokuapp.com/facts"
cat_facts <- GET(
  url = cat_path,
)
http_status(cat_facts)

cat_df <- content(cat_facts, as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE) %>%
  data.frame()

cat_df %>%
  select(text)

## Guardian articles ----------------------------------------------------

# Get APi key here: https://open-platform.theguardian.com/documentation/

# read in saved key and paste it to the path
gd_api <- readRDS(here::here("keys", "guardian_keys.rds"))
gd_path <- paste0("https://content.guardianapis.com/search?api-key=",
                  gd_api$api_key)

# Load
gd_request <- GET(
  url = gd_path,
  query = list(
    q = "dinosaur" # pieces mentioning dinosaurs
  )
)

# Check status
http_status(gd_request)

# Get the content and parse the JSON code.
gd_content <- content(gd_request, as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE) %>%
  data.frame()
# What's in there?
names(gd_content)

# Look at the date and titles
d_content %>%
  select(response.results.webPublicationDate, response.results.webTitle)

## Oxford English Dictionary --------------------------------------------

# register developer account here: https://developer.oxforddictionaries.com/

# read in saved keys
oed_keys <- readRDS(here::here("keys", "oed_keys.rds"))

# note that the path contains the word you are searching for
word <- "dinosaur"
ox_path <- paste0("https://od-api.oxforddictionaries.com/api/v2/entries/en-gb/", word)

# use id and key in the `add_headers()`
ox_request <- GET(
  url = ox_path,
  add_headers(
    app_id = oed_keys$app_id,
    app_key = oed_keys$app_key
  ))

http_status(ox_request) # it works!

# function for getting entries from the OED API
get_OED_entry <- function(word, lang = "en-gb"){
  # Load the keys if not already in the workspace
  if(!"oed_keys" %in% ls()) readRDS(here::here("keys", "oed_keys.rds"))

  path <- paste(
    "https://od-api.oxforddictionaries.com/api/v2/entries",
    lang,
    tolower(word),
    sep = "/"
  )
  ox_request <- GET(
    url = path,
    add_headers(
      app_id = oed_keys$app_id,
      app_key = oed_keys$app_key
    ))

  if(ox_request$status_code != 200){
    http_status(ox_request) %>%
      print()
  } else {
    ox_request %>%
      content(as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE) %>%
      data.frame()
  }
}

# take a look at our entry
kraken_entry <- get_OED_entry("kraken")
kraken_entry %>%
  glimpse()

# get our definition from this complex object
kraken_entry$results.lexicalEntries %>%
  first() %>% # get the first item of a list
  pull(entries) %>% # pull the content of a data.frame column
  first() %>%
  pull(senses) %>%
  first() %>%
  pull(definitions) %>%
  simplify() # collapse a list to a vector

# try another word
bank_entry <- get_OED_entry("bank")

bank_entry$results.lexicalEntries %>%
  first() %>%
  pull(entries) %>% # pull the content of a data.frame column
  first() %>%
  pull(senses) %>%
  first() %>%
  pull(definitions) %>%
  simplify()










