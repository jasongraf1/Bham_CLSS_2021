# =========================================================================
# file: scraping_with_rvest.R
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
library(tidytext) # for text mining
library(here) # for creating consistent file paths
library(rvest) # for scraping


# Simple scraping ---------------------------------------------------------

# save the URL for the main page we're working with
books_main_url <- "http://books.toscrape.com/"
books_main_page <- read_html(books_main_url)

# pull out all the html elements with the selector ".product_pod a"
books_title_nodes <- html_elements(books_main_page, ".product_pod a")

# print first 6 elements
head(books_title_nodes)

# get text of titles
books_titles <- html_text(books_title_nodes)
books_titles

# fix our selector
books_title_nodes <- html_elements(books_main_page, "h3 a")
books_titles <- html_text(books_title_nodes)
books_titles

# pull out the href attributes containing the links
books_urls <- books_title_nodes %>%
  html_attr("href")
head(books_urls)

# paste the main url to each individual book's address
paste(books_main_url, books_urls, sep = "") %>%
  head()

# read the first book's page
book1_page <- paste(books_main_url, books_urls[1], sep = "/") %>%
  read_html()

# get the description text
book1_page %>%
  html_elements("#product_description+ p") %>%
  html_text()

# Scraping multiple book descriptions ------------------------------------

## Method 1 --------------------------------------------------------------

books_df <- data.frame(
  url = paste(books_main_url, books_urls, sep = ""),
  title = books_titles,
  description = NA
)

# create function for getting a description
get_description_from_url <- function(url){
  description <- read_html(url) %>%
    html_elements("#product_description+ p") %>%
    html_text()
  return(description) # this is a character string
}

# Apply function to all books in the dataframe
# map_chr() takes one argument, applies a function, and returns a character string
tic()
books_df <- books_df %>%
  mutate(description = map_chr(url, get_description_from_url))
toc()

books_df

## Method 2 --------------------------------------------------------------

# start a session
books_session <- session(books_main_url)

# create function for getting a description from a link in a live session
get_description_from_link <- function(title){
  # use tryCatch to deal with any possible errors
  possibleError <- tryCatch(
    page <- books_session %>%
      session_follow_link(title) %>%
      read_html(),
    error = function(e) e
  )
  # move on to the next link if an error is found
  if(inherits(possibleError, "error")) next

  description <- page %>%
    html_elements("#product_description+ p") %>%
    html_text()

  return(description) # this is a character string
}

# Apply function to all books in the dataframe
# map2_chr() takes two arguments, applies a function, and returns a character string
tic()
books_df2 <- books_df %>%
  mutate(description = map_chr(title, get_description_from_link))
toc()

books_df


# Save data ---------------------------------------------------------------

# Save dataframe to .rds file
books_df %>%
  saveRDS(here("data_raw", "books_df.rds"))

# Create function to write descriptions to separate files
write_to_file <- function(url, title, description, ...){
  # Paste the title and url before the description
  text <- paste(title, url, description, sep = "\n")

  # Replace spaces in title with underscores
  title <- str_replace_all(title, " ", "_")

  file_name <- here("book_descriptions", paste(title, "txt", sep = "."))

  # Open a file and write the text to the file
  file_connection <- file(file_name)
  writeLines(text, file_name)
  close(file_connection) # Close the file
  # Notice that this function does not produce any output in R
}

# like map(), pwalk() will go through each row, and apply a function to all the columns
# specified in the function
# See ?purrr::pwalk
tic()
books_df %>%
  pwalk(write_to_file)
toc()