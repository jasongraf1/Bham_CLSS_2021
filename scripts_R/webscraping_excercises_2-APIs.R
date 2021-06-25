# =========================================================================
# file: webscraping_exercises_2-APIs.R
# author: Jason Grafmiller
# date: 26/05/2021
#
# description:
# Code for the exercises on using APIs to request data from a web page.
# =========================================================================

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)

# Q1 ----------------------------------------------------------------------

# Use {RedditExtractoR} to get a list of threads with the search term "sightings" from the "UFOs" subreddit.
#   A. Find the threads with the highest and lowest number of comments.
#   B. Create a graph of comment chain of the thread with the lowest number of comments (it may be a lot).

library(RedditExtractoR) # for scraping reddit forums

# get threads on UFO sightings
ufo_sightings_threads <- get_reddit(
  search_terms = "sightings",
  page_threshold = 1, # number of pages to be searched
  subreddit = "UFOs"
)

# look at the results
ufo_sightings_threads %>%
  glimpse()

#   A. Find the threads with the highest and lowest number of comments.
#
# The threads with the most comments:
ufo_sightings_threads %>%
  select(num_comments, title, author) %>%
  arrange(desc(num_comments)) %>% # put most popular thread first
  slice_head(n = 6) # gives us the top 6 rows (this is the same as the base `head()` function)

# Notice that the rows are repeated, because each row represents a single comment.
# You can use `distinct()` to remove repeated rows
ufo_sightings_threads %>%
  select(num_comments, title, author) %>%
  distinct() %>% # this comes AFTER we select the three columns above
  arrange(desc(num_comments)) %>% # put most popular thread first
  slice_head(n = 6) # gives us the top 6 rows

# threads with the least comments:
ufo_sightings_threads %>%
  select(num_comments, title, author) %>%
  distinct() %>% # this comes AFTER we select the three columns above
  arrange(desc(num_comments)) %>% # put most popular thread first
  slice_tail(n = 6) # gives us the bottom 6 rows (this is the same as the base `tail()` function)

# So the top thread has 772 comments, and the bottom has 278 comments...

# an alternative is to group by the title and count the rows, but this will give
# slightly different results. This is because we only searched through one page.
# If we set page_threshold higher, we'd get more (but it would also take longer)
ufo_sightings_threads %>%
  group_by(title) %>%
  count(sort = T)

#   B. Create a graph of comment chain of the thread with the lowest number of comments (it may be a lot).

# one way to do this is to use the same code but set n = 1 in `slice_tail()`, then use `pull()` to pull out the content of the 'title' column
bottom_title <- ufo_sightings_threads %>%
  select(num_comments, title, author) %>%
  distinct() %>% # this comes AFTER we select the three columns above
  arrange(desc(num_comments)) %>% # put most popular thread first
  slice_tail(n = 1) %>%
  pull(title)

# Now we filter the dataframe and make the plot.
bottom_thread_chain <- ufo_sightings_threads %>%
  dplyr::filter(title == bottom_title) %>%
  construct_graph(plot = TRUE)

# The result isn't very easy to read, frustratingly...

# Q2 ----------------------------------------------------------------------

# Use {rtweet} to get the most recent 200 English language tweets (no retweets) containing the hashtag "#ufo". Hint: remember to add "lang:en" to your query. Display the text of the first 10 tweets in the resulting dataframe.

library(rtweet) # for getting tweets

ufo_tweets <- search_tweets(
  "#ufo lang:en", # the terms to search for
  n = 200, # the number of tweets to collect
  include_rts = FALSE # don't include retweets
)

# look at the results
ufo_tweets %>%
  glimpse()

# Now we can use the `slice_head()` function to look at the first 10 rows of the dataframe.
ufo_tweets %>%
  slice_head(n = 10) %>% # `head(n = 10)` also will work
  select(text)


# Q3 ----------------------------------------------------------------------
# Use {rtweet} to get the most recent 500 tweets containing either one of the terms "brum" or "brummie", restricting your search to the area around Birmingham.

# Get coords for Birmingham
lookup_coords("birmingham england")

bham_brum_tweets <- search_tweets(
  q = "brum OR brummie lang:en", # note the query syntax for searches of multiple terms!
  n = 500,
  include_rts = TRUE, # I include retweets, but you can set this to FALSE if you like
  geocode = lookup_coords("birmingham england")
)

# You may not get many...

bham_brum_tweets %>%
  slice_head(n = 10) %>% # `head(n = 10)` also will work
  select(text)

