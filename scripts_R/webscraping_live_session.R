# =========================================================================
# file: webscraping_live_session.R
# author: Jason Grafmiller
# date: 07/07/2021
#
# description:
# Code from the live session
# =========================================================================

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here) # on why you should use `{here}`: https://www.tidyverse.org/blog/2017/12/workflow-vs-script/
library(tidytext)

library(patchwork) # incredibly cool package for putting together multiple plots

library(rvest)

# Troubleshooting --------------------------------------------------------

# A comment in the Discussion Board mentioned getting this message

# -- Conflicts --------------------------------------------------------------- tidyverse_conflicts() --
# x dplyr::filter()    masks stats::filter()
# x dplyr::lag()       masks stats::lag()

# What does it mean to have "conflicts", and how do we deal with it?

mtcars # a small dataset that comes with R

# look at only cars with 6 cylinder engines

mtcars %>%
  filter(cyl == 6)

# to call `function()` in specific package, we use the syntax:
# package::function()

mtcars %>%
  dplyr::filter(cyl == 6) # works!

mtcars %>%
  stats::filter(cyl == 6) # doesn't work

# Usually, when there is a conflict, R will default to the function in the most recently loaded package. In this case, that would be `dplyr`, which is part of the tidyverse.
# You can read more about this here:
# https://stats.idre.ucla.edu/r/faq/how-does-r-handle-overlapping-object-names/

# more info on standard datasets
library(help = "datasets")

# Moving on...


# User defined functions --------------------------------------------------

# Being able to create your own custom functions is VERY useful when it comes to things like web scraping, where you often need to run the same process many times over different items. Fortunately, it is fairly easy to do...

# Basic function structure:
# myfunction <- function(arg1, arg2, ... ){
#   statements
#   return(object)
# }

sum_two_numbers <- function(x, y){
  s <- x + y   # assign the sum of "x" and "y" to "s"
  return(s)    # return "s" as the output
}

sum_two_numbers(200, 400)

a <- 2.141592654
b <- 2.718281828
sum_two_numbers(a, b)
# easy!

# You can apply functions to items in a vector, list, dataframe, etc. with the `map()` functions in `{purrr}` (part of the tidyverse).

# create function that transforms probabilities [0,1] into log odds (-Inf, Inf)
logit <- function(p){
  logodds <- log(p/(1-p))
  return(logodds)
}

# create sequence from 0 to 1 by .1, i.e. c(0, .1, .2, .3,...,1)
x <- seq(from = 0, to = 1, by = .1)
x

# now use `map()` to apply the `logit()` function to each value in "x"
map(x, logit) # the output is a list, which is maybe not what we want here

# Check the documentation to see what we might do:
help(map)

map_dbl(x, logit)
map_chr(x, logit)

# We'll see more as we go.


# Now on to web scraping...

# 1. Exercises 1
# 2. Exercises 2


# Yelp reviews ------------------------------------------------------------

# Install and load the `{yelpr}` package. More info here: https://github.com/OmaymaS/yelpr
# to install:
# devtools::install_github("OmaymaS/yelpr")

library(yelpr)

# You'll need an API key, but you can get that by following the directions at
# the GitHub page above, or by going here:
# https://www.yelp.com/developers/documentation/v3/authentication

# Once you have an app, ID, and API key, you can save it to a file
# yelp_keys <- list(
#   appname = "myapp",
#   client_id = "XXXXXXXXXXXXXXXXXXX",
#   api_key = "XXXXXXXXXXXXXXXXXXXX"
# ) %>%
#   saveRDS(here("keys", "yelp_keys.rds"))

# I already have a key, which I keep in "yelp_keys.rds" in my "keys" subfolder

# load the yelp keys
yelp_keys <- here("keys", "yelp_keys.rds") %>%
  readRDS()

# Now let's get started!
# The basic fucntion here is `business_search()`:

?business_search()

# More info on what the various columns mean can be found in the
# API documentation here:
# https://www.yelp.com/developers/documentation/v3/business_search

# We'll look for coffee shops near the University of Birmingham
coffee_bham <- business_search(
  api_key = yelp_keys$api_key,
  latitude = 52.45, # The lat and long for our ELAL dept. building
  longitude = -1.93,
  term = "coffee",
  limit = 10
)
# try using `location = "Birmingham, UK"` instead to get slightly different results

coffee_bham$businesses %>%
  glimpse()

coffee_bham$businesses$name

coffee_bham$businesses$coordinates

coffee_bham$businesses$location

coffee_bham$businesses %>%
  select(name, rating, review_count, id)

# Now to get the reviews we'll use `business_search_review()` with the business ID
?business_search_review()

coffee_bham$businesses %>%
  dplyr::filter(name == "Faculty") %>%
  pull(id)

# Get the faculty reviews
faculty_revs <- business_search_review(yelp_keys$api_key, "XYEYtDwbpy9xeAlLzgDQlQ")

faculty_revs$reviews %>%
  as_tibble()

# we only get three reviews per business, and the text is incomplete...


# pull the url for the Faculty Coffee Yelp page
faculty_url <- coffee_bham$businesses %>%
  dplyr::filter(name == "Faculty") %>%
  pull(url)

# Here is the main Yelp page for Faculty coffee:
faculty_url

# Using this, we can just scrape the reviews the 'old-fashioned way'...

# read the HTML for the page with `{rvest}`
faculty_page <- read_html(faculty_url)

# get the text of the reviews using Selector gadget to find the CSS selector
faculty_revs_full <- faculty_page %>%
  html_elements(".comment__373c0__1M-px") %>%
  html_text()

faculty_revs_full[1] # look at the first review


# Now what about the rest of the pages?

faculty_page2 <- read_html("https://www.yelp.com/biz/faculty-birmingham?adjust_creative=t7KWfTH9HmUCULe8TlzTbA&utm_campaign=yelp_api_v3&utm_medium=api_v3_business_search&utm_source=t7KWfTH9HmUCULe8TlzTbA&start=10")

faculty_page2 %>%
  html_elements(".comment__373c0__1M-px") %>%
  html_text()

# Now we can reconstruct the pages by pasting the page-specific parts to the main URL
faculty_pages <- c(
  faculty_url, paste0(faculty_url, "&start=", seq(10, 40, by = 10))
)
faculty_pages

# make a quick function to scrape the reviews.
scrape_reviews <- function(url){
  reviews <- read_html(url) %>%
    html_elements(".comment__373c0__1M-px") %>%
    html_text()
  return(reviews)
}

# use `map()` to apply this function to each fo the 5 pages
faculty_revs_all <- map(faculty_pages, scrape_reviews)

# simplify the list to a single vector
faculty_revs_all %>%
  simplify()

# What about the ratings? We need to do a bit more detective work here. The stars don't have text, so we can't use `html_text()`. We can use selector gadget to find the CSS selector, and the inspector pane to find the attribute containing the star rating.
#
# elements with the ".margin-b1-5__373c0__2Wblx .overflow--hidden__373c0__2B0kz" selector also have a special 'aria-label' attribute whose value is the rating.
# We use `html_attr()` to get the value of this attribute
faculty_page %>%
  html_elements(".margin-b1-5__373c0__2Wblx .overflow--hidden__373c0__2B0kz") %>%
  html_attr("aria-label")

scrape_stars <- function(url){
  stars <- read_html(url) %>%
    html_elements(".margin-b1-5__373c0__2Wblx .overflow--hidden__373c0__2B0kz") %>%
    html_attr("aria-label")
  return(stars)
}

faculty_stars_all <- map(faculty_pages, scrape_stars)


# Now we could put the ratings and reviews from the first page into a tibble
# (or data.frame)
tibble(
  ratings = simplify(faculty_stars_all), # `simplify()` converst these to vectors
  text = simplify(faculty_revs_all)
)
# Rinse and repeat for the other pages!

# Now we can visualise these texts..
faculty_df <- tibble(
  ratings = simplify(faculty_stars_all), # `simplify()` converst these to vectors
  text = simplify(faculty_revs_all)
)

# Word frequencies

faculty_counts <- faculty_df %>%
  mutate(ratings = str_remove(ratings, " star rating")) %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  count(name = "freq", sort = T) %>%
  ungroup()

# bar plot of top 20 words
faculty_counts %>%
  slice_head(n = 20) %>%
  ggplot(aes(reorder(word, freq), freq)) +
  geom_col(fill = "chocolate4") +
  coord_flip() +
  theme_classic()

# word cloud
library(wordcloud2)

faculty_counts %>%
  wordcloud2(size = 2)

# remove 'stopwords' with tidytext's stop_words dataset
stop_words %>%
  dplyr::filter(lexicon == "snowball")

snow_stop <- stop_words %>%
  dplyr::filter(lexicon == "snowball")

faculty_content <- faculty_counts %>%
  anti_join(snow_stop, by = "word") %>%
  arrange(desc(freq))

# now try plots again
faculty_content %>%
  slice_head(n = 20) %>%
  ggplot(aes(reorder(word, freq), freq)) +
  geom_col(fill = "chocolate4") +
  coord_flip() +
  theme_classic()

faculty_content %>%
  wordcloud2(size = 2)

# How does another shop compare?
coffee_bham$businesses %>%
  select(name, rating, review_count, id)

# What about Quarter Horse Coffee?
qhc_url <- faculty_url <- coffee_bham$businesses %>%
  dplyr::filter(name == "Quarter Horse Coffee") %>%
  pull(url)

qhc_pages <- c(qhc_url, paste0(qhc_url, "&start=10")) # only 2 pages here

qhc_revs_all <- map(qhc_pages, scrape_reviews)
qhc_stars_all <- map(qhc_pages, scrape_stars)

# Create the content tibble all at once
qhc_content <- tibble(
  ratings = simplify(qhc_stars_all),
  text = simplify(qhc_revs_all)
  ) %>%
  mutate(ratings = str_remove(ratings, " star rating")) %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  count(name = "freq", sort = T) %>%
  ungroup() %>%
  anti_join(snow_stop, by = "word") %>%
  arrange(desc(freq))

# check the content
qhc_content

# compare plots
fac_plot <- faculty_content %>%
  slice_head(n = 20) %>%
  ggplot(aes(reorder(word, freq), freq)) +
  geom_col(fill = "#83502e") +
  coord_flip() +            # flip x and y axes
  theme_classic() +
  xlab("") +                # remove x axis label
  ggtitle("Faculty")

fac_plot

qhc_plot <- qhc_content %>%
  slice_head(n = 20) %>%
  ggplot(aes(reorder(word, freq), freq)) +
  geom_col(fill = "#bd7e4a") +
  coord_flip() +            # flip x and y axes
  theme_classic() +
  xlab("") +                # remove x axis label
  ggtitle("Quarter Horse Coffee")

qhc_plot

# Use the awesome `{pactchwork}` package to combine and annotate ggplots.
# See the documentation for more: https://patchwork.data-imaginist.com/

(fac_plot + qhc_plot) +
  plot_annotation(
    title = "Top 20 words in coffee shop Yelp reviews",
    caption = "Scraped with {yelpr} and {rvest} packages in R"
  )

