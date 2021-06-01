# =========================================================================
# file: webscraping_exercises_1.R
# author: Jason Grafmiller
# date: 26/05/2021
#
# description:
# Code for the exercises on using rvest to scrape data from a web page.
# =========================================================================

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(rvest)

# Q1 ----------------------------------------------------------------------

# Scrape the description of the book Sharp Objects on the first page of the https://books.toscrape.com/ site. Write the description to a text file "sharp_objects_description.txt".

sharp_obj_url <- "https://books.toscrape.com/catalogue/sharp-objects_997/index.html"

# get description:
sharp_obj_desc <- read_html(sharp_obj_url) %>%
  html_elements("#product_description+ p") %>%
  html_text()

# write to file:
file_connection <- file("sharp_objects_description.txt")
writeLines(sharp_obj_desc, "sharp_objects_description.txt")
close(file_connection)


# Q2 ----------------------------------------------------------------------

# Create a dataframe containing the titles and URLs of all the books on the 3rd page of the https://books.toscrape.com/ site.

# Here's the URL for the 3rd page in the site
books_page3_url <- "https://books.toscrape.com/catalogue/page-3.html"
books_page3 <- read_html(books_page3_url)

# get the title elements as before
books_title_nodes <- html_elements(books_page3, "h3 a")

# get the titles
books_titles <- books_title_nodes %>%
  html_text()

# get the href attributes
books_urls <- books_title_nodes %>%
  html_attr("href")

# create dataframe (remember to paste the full URL path)
page3_df <- data.frame(
  title = books_titles,
  url = paste(
    "https://books.toscrape.com/catalogue/", # note the slightly different URL
    books_urls,
    sep = ""
  )
)


# Q3 ----------------------------------------------------------------------
# Scrape the descriptions of all the books in the dataframe from exercise 2 using the static URLs (Method 1)
# Scrape the descriptions of all the books in the dataframe from exercise 2 using links in a live session (Method 2)

## METHOD 1

# add blank column to dataframe
page3_df <- page3_df %>%
  mutate(description = NA)

# create function for getting a description
get_description_from_url <- function(url){
  description <- read_html(url) %>%
    html_elements("#product_description+ p") %>%
    html_text()
  return(description) # this is a character string
}

# Apply function to all books in the dataframe
# map_chr() takes one argument, applies a function, and returns a character string
page3_df <- page3_df %>%
  mutate(description = map_chr(url, get_description_from_url))

## METHOD 2
##
# start a session
books_session <- session(books_page3_url)

# create function for getting a description from a link in a live session
get_description_from_link <- function(title){
  page <- books_session %>%
    session_follow_link(title) %>%
    read_html()

  description <- page %>%
    html_elements("#product_description+ p") %>%
    html_text()

  return(description) # this is a character string
}

# Apply function to all books in the dataframe
# map2_chr() takes two arguments, applies a function, and returns a character string
page3_df <- page3_df %>%
  mutate(description = map_chr(title, get_description_from_link))


# Q4 ----------------------------------------------------------------------
# Scrape only the titles of the books contained in the first 10 pages of the https://books.toscrape.com/ (Links to an external site.) site.

# Note that the pages all have URLs of https://books.toscrape.com/catalogue/page-XX.html, where 'XX' is the number of the page. We can create a vector of URLs with this info.

ten_pages_url <- c(
  "https://books.toscrape.com", # first page is different
  paste("https://books.toscrape.com/catalogue/page-", 2:10, ".html", sep = "")
)
ten_pages_url

get_titles_on_page <- function(url){
  titles <- read_html(url) %>%
    html_elements("h3 a") %>%
    html_text()
  return(titles)
}

ten_titles <- map(ten_pages_url, get_titles_on_page) %>%
  simplify() # map() returns a list, and here we simplify the list to a simple vector
ten_titles

# Q5 ----------------------------------------------------------------------
# Scrape the author names and the content of the description in their 'about' page (e.g. this page (Links to an external site.)) from the 10 authors (some are duplicates) listed on the first page of the https://quotes.toscrape.com/ (Links to an external site.) site. Put the results into a dataframe with two columns: 'Author' and 'Description'. Hint: you may need scrape the author names and URLs of the 'about' pages separately.

quote_url <- "https://quotes.toscrape.com/"

quote_page <- quote_url %>%
  read_html()

# get element containing author name
authors <- html_elements(quote_page, ".author")
authors

# get element containing 'about' link
bios <- html_elements(quote_page, ".quote span a")
bios

# create dataframe with author name, 'about' URL, and bio description
df <- data.frame(
  author = html_text(authors),
  url = paste("https://quotes.toscrape.com", html_attr(bios, "href"), sep = ""),
  bio = NA
)
df

# create function for getting bio description from URL
get_bio <- function(url){
  page <- read_html(url)
  bio <- page %>%
    html_element(".author-description") %>%
    html_text() %>%
    str_trim() # trim whitespace
  return(bio)
}

# apply function to dataframe
df <- df %>%
  mutate(bio = map_chr(url, get_bio))
df %>%
  select(-url) # select out 'url' column
