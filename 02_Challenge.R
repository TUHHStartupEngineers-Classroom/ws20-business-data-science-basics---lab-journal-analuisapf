# WEBSCRAPING ----

# 1.0 LIBRARIES ----
# rm(list=ls())
# gc()

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(future)
library(furrr) 





# 1.1 COLLECT PRODUCT FAMILIES ----

rose_url_home          <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb"
# xopen(rose_url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
rose_html_home         <- read_html(rose_url_home)

# Web scrape the ids for the families
rose_bike_family_tbl <- rose_html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".catalog-navigation__link") %>%
  # ...and extract the information of the id attribute
  html_attr('href') %>%
  
  # Remove the product families Gear and Outlet and Woman
  # (because the female bikes are also listed with the others)
  discard(.p = ~stringr::str_detect(.x,"All|Trail")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )

rose_bike_family_tbl
view(rose_bike_family_tbl)

# 1.2 COLLECT PRODUCT CATEGORIES ----

# # Combine all Ids to one string so that we will get all nodes at once
# # (seperated by the OR operator ",")
rose_family_id_css <- rose_bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")
rose_family_id_css

# Extract the urls from the href attribute
rose_bike_category_tbl <- data.frame(rose_bike_family_tbl[-1,1:2]) %>%
  # enframe(name = "position", value = "subdirectory") # %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.rosebikes.de{family_class}")
  ) %>%
  dplyr::select(url) %>% 
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

rose_bike_category_tbl
view(rose_bike_category_tbl)


# 2.0 COLLECT BIKE DATA ----

# 2.1 Get URL for each bike of the Product categories

# select first bike category url
rose_bike_category_url <- rose_bike_category_tbl$url[1]

# Alternatives for selecting values
# bike_category_url <- bike_category_tbl %$% url %>% .[1]
# bike_category_url <- bike_category_tbl %>% pull(url) %>% .[1]
# bike_category_url <- deframe(bike_category_tbl[1,])
# bike_category_url <- bike_category_tbl %>% first %>% first

xopen(rose_bike_category_url)

# Get the URLs for the bikes of the first category
rose_html_bike_category  <- read_html(rose_bike_category_url)
rose_bike_url_tbl        <- rose_html_bike_category %>%
  
  # Get the 'a' nodes, which are hierarchally underneath 
  # the class productTile__contentWrapper
  html_nodes(css = ".row .align-middle > a") %>%
  html_attr("href") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "url") %>%
  mutate(
    url = glue("https://www.rosebikes.de{url}")
  )

view(rose_bike_url_tbl)


# 2.1.3 Get even more data from JSON files
rose_bike_json_tbl1  <- rose_html_bike_category %>%  
  html_nodes(css = '.catalog-category-bikes__price-title') %>%
  html_text() %>%
  str_remove("ab") %>%
  enframe(name = "position", value = "price") 
 

rose_bike_json_tbl  <- rose_html_bike_category %>%
  html_nodes(css = '.catalog-category-bikes__title') %>%
  html_text() %>% 
  enframe(name = "position", value = "name") %>%
  left_join(rose_bike_json_tbl1) %>%
  left_join(rose_bike_url_tbl)
  
view(rose_bike_json_tbl)

# 2.2 Wrap it into a function ----
rose_get_bike_data <- function(url) {
  
  rose_html_bike_category <- read_html(url)
  
  # Get the URLs
  rose_bike_url_tbl  <- rose_html_bike_category %>%
    html_nodes(css = ".row .align-middle > a") %>%
    html_attr("href") %>%
    enframe(name = "position", value = "url") %>%
    mutate(
    url = glue("https://www.rosebikes.de{url}")
  )
 
  # Get JSON data
  rose_bike_json_tbl1  <- rose_html_bike_category %>%  
    html_nodes(css = '.catalog-category-bikes__price-title') %>%
    html_text() %>%
    str_remove("ab") %>%
    enframe(name = "position", value = "price") 
  
  
  rose_bike_json_tbl2  <- rose_html_bike_category %>%
    html_nodes(css = '.catalog-category-bikes__title') %>%
    html_text() %>% 
    enframe(name = "position", value = "name") %>%
    left_join(rose_bike_json_tbl1) %>%
    left_join(rose_bike_url_tbl)
}

# Run the function with the first url to check if it is working
rose_bike_category_url <- rose_bike_category_tbl$url[1]
rose_bike_data_tbl     <- rose_get_bike_data(url = rose_bike_category_url)

rose_bike_data_tbl

# 2.3.1a Map the function against all urls

# Extract the urls as a character vector
rose_bike_category_url_vec <- rose_bike_category_tbl %>% 
  pull(url)

rose_bike_category_url_vec

# Run the function with every url as an argument
rose_bike_data_lst <- purrr::map(rose_bike_category_url_vec, rose_get_bike_data)

# Merge the list into a tibble
rose_bike_data_tbl <- bind_rows(rose_bike_data_lst)
saveRDS(rose_bike_data_tbl, "rose_bike_data_tbl.rds")

rose_bike_data_tbl$position <- NULL

rose_bike_data_tbl1 <- rose_bike_data_tbl %>%
  mutate(price =  price %>% str_remove_all("€|\\.|\\,") %>% str_trim() %>% as.numeric)

view(rose_bike_data_tbl1)


