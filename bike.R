# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.rosebikes.de/"
xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_family_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".main-navigation-category-with-tiles__link") %>%
  # ...and extract the information of the id attribute
  html_attr('href') %>%
  
  # Remove the product families Gear and Outlet and Woman 
  # (because the female bikes are also listed with the others)
  discard(.p = ~stringr::str_detect(.x,"sale|kinder")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )

view(bike_family_tbl)

# 1.2 COLLECT PRODUCT CATEGORIES ----

# Combine all Ids to one string so that we will get all nodes at once
# (seperated by the OR operator ",")
family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")
family_id_css

# Extract the urls from the href attribute
bike_category_tbl <- html_home %>%
  
  # Select nodes by the ids
  #html_nodes(css = family_id_css) %>%
  
  # Going further down the tree and select nodes by class
  # Selecting two classes makes it specific enough
  html_nodes(css = "catalog-navigation__link") %>%
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.canyon.com{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

view(bike_category_tbl)