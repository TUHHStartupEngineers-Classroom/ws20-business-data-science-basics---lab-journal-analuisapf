# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.canyon.com/en-de"
xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_family_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".js-navigationDrawer__list--secondary") %>%
  # ...and extract the information of the id attribute
  html_attr('id') %>%
  
  # Remove the product families Gear and Outlet and Woman 
  # (because the female bikes are also listed with the others)
  discard(.p = ~stringr::str_detect(.x,"WMN|WOMEN|GEAR|OUTLET")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )

bike_family_tbl
view(bike_family_tbl)

# The updated page has now also ids for CFR and GRAVEL. You can either include or remove them.

# 1.2 COLLECT PRODUCT CATEGORIES ----

# Combine all Ids to one string so that we will get all nodes at once
# (seperated by the OR operator ",")
family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")
family_id_css
## "#js-navigationList-ROAD, #js-navigationList-MOUNTAIN, #js-navigationList-EBIKES, #js-navigationList-HYBRID-CITY, #js-navigationList-YOUNGHEROES"

# Extract the urls from the href attribute
bike_category_tbl <- html_home %>%
  
  # Select nodes by the ids
  html_nodes(css = family_id_css) %>%
  
  # Going further down the tree and select nodes by class
  # Selecting two classes makes it specific enough
  html_nodes(css = ".navigationListSecondary__listItem .js-ridestyles") %>%
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

bike_category_tbl
view(bike_category_tbl)

# 2.0 COLLECT BIKE DATA ----

# 2.1 Get URL for each bike of the Product categories

# select first bike category url
bike_category_url <- bike_category_tbl$url[1]

# Alternatives for selecting values
# bike_category_url <- bike_category_tbl %$% url %>% .[1]
# bike_category_url <- bike_category_tbl %>% pull(url) %>% .[1]
# bike_category_url <- deframe(bike_category_tbl[1,])
# bike_category_url <- bike_category_tbl %>% first %>% first

xopen(bike_category_url)

# Get the URLs for the bikes of the first category
html_bike_category  <- read_html(bike_category_url)
bike_url_tbl        <- html_bike_category %>%
  
  # Get the 'a' nodes, which are hierarchally underneath 
  # the class productTile__contentWrapper
  html_nodes(css = ".productTile__contentWrapper > a") %>%
  html_attr("href") %>%
  
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "\\?.*") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "url")

# 2.1.2 Extract the descriptions (since we have retrieved the data already)
bike_desc_tbl <- html_bike_category %>%
  
  # Get the nodes in the meta tag where the attribute itemprop equals description
  html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
  
  # Extract the content of the attribute content
  html_attr("content") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "description")

# 2.1.3 Get even more data from JSON files
bike_json_tbl  <- html_bike_category %>%
  
  html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
  html_attr("data-gtm-impression") %>%
  
  # Convert the JSON format to dataframe
  # map runs that function on each element of the list
  map(fromJSON) %>% # need JSON ### need lists
  
  # Extract relevant information of the nested list
  map(purrr::pluck, 2, "impressions") %>% # Need purrr and expl above
  
  # Set "not defined" and emtpy fields to NA (will be easier to work with)
  map(na_if, "not defined") %>%
  map(na_if, "") %>%
  
  # The class of dimension56 and price varies between numeric and char.
  # This converts this column in each list to numeric
  # across allows to perform the same operation on multiple columns
  map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
  
  # Stack all lists together
  bind_rows() %>%
  # Convert to tibble so that we have the same data format
  as_tibble() %>%
  
  # Add consecutive numbers so that we can bind all data together
  # You could have also just use bind_cols()
  rowid_to_column(var='position') %>%
  left_join(bike_desc_tbl) %>%
  left_join(bike_url_tbl)

# 2.2 Wrap it into a function ----
get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".productTile__contentWrapper > a") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
  
  # Get the descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(css = '.productTile__productSummaryLeft > 
                      meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  # Get JSON data
  bike_json_tbl <- html_bike_category %>%
    html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>% # need JSON ### need lists
    map(purrr::pluck, 2, "impressions") %>% 
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var='position') %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
}

# Run the function with the first url to check if it is working
bike_category_url <- bike_category_tbl$url[1]
bike_data_tbl     <- get_bike_data(url = bike_category_url)

bike_data_tbl

# 2.3.1a Map the function against all urls

# Extract the urls as a character vector
bike_category_url_vec <- bike_category_tbl %>% 
  pull(url)

# Run the function with every url as an argument
bike_data_lst <- map(bike_category_url_vec, get_bike_data)

# Merge the list into a tibble
bike_data_tbl <- bind_rows(bike_data_lst)
saveRDS(bike_data_tbl, "bike_data_tbl.rds")

# Check for duplicates
bike_data_tbl %>%
  group_by(id) %>%
  filter(n()>1) %>%
  arrange(id) %>% 
  View()

# Filter non Canyon bikes (based on id length) and add an empty column for the colors
bike_data_cleaned_tbl <- bike_data_tbl %>%
  
  # Filter for bikes. Only unique ones
  filter(nchar(.$id) == 4) %>%
  filter(!(name %>% str_detect("Frameset"))) %>%
  distinct(id, .keep_all = T) %>%
  
  # Split categories (Speedmax had to be treated individually)
  mutate(category = replace(category, 
                            name == "Speedmax CF SLX 8.0 SL", "Road/Triathlon Bike/Speedmax")) %>%
  separate(col = category, into = c("category_1",
                                    "category_2",
                                    "category_3"),
           sep = "(?<!\\s)/(?!\\s)") %>%
  
  # Renaming
  rename("year"       = "dimension50") %>%
  rename("model"      = "name") %>%
  rename("gender"     = "dimension63") %>%
  rename("price_euro" = "metric4") %>%
  
  # Fix years manually (have checked the website)
  mutate(year = replace_na(year, 2021)) %>%
  
  # Add frame material
  mutate(frame_material = case_when(
    model %>% str_detect(" CF ") ~ "carbon",
    model %>% str_detect(" CFR ") ~ "carbon",
    TRUE ~ "aluminium"
  )
  ) %>%
  
  # Select and order columns
  select(-c(position, brand, variant, starts_with("dim"), 
            quantity, feedProductId, price, metric5)) %>%
  select(id, model, year, frame_material, price_euro, everything())

saveRDS(bike_data_cleaned_tbl, "bike_data_cleaned_tbl.rds")

# 3.1a Get all color variations for each bike

# Extract all bike urls
bike_url_vec <- bike_data_cleaned_tbl %>% 
  pull(url)

# Create function to get the variations
get_colors <- function(url) {
  
  url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    str_replace("^[^\\{]+", "") %>%
    # 2. replace at the end everything after the last "}" with ""
    str_replace("[^\\}]+$", "") %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON() %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 1, "value")
}

# Run the function over all urls and add result to bike_data_cleaned_tbl
# This will take a long time (~ 20-30 minutes) because we have to iterate over many bikes
bike_data_colors_tbl <- bike_data_cleaned_tbl %>% 
  mutate(colors = map(bike_url_vec, get_colors))

saveRDS(bike_data_colors_tbl, "bike_data_colors_tbl.rds")

# 3.2 Create the urls for each variation

bike_data_colors_tbl <- bike_data_colors_tbl %>%
  
  # Create entry for each color variation
  unnest(colors) %>%
  
  # Merge url and query parameters for the colors
  mutate(url_color = glue("{url}?dwvar_{id}_pv_rahmenfarbe={colors}")) %>%
  select(-url) %>%
  
  # Use stringi to replace the last dash with the HTLM format of a dash (%2F)
  # Only if there is a dash in the color column
  mutate(url_color = ifelse(str_detect(colors, pattern = "/"),
                            
                            # if TRUE --> replace      
                            stringi::stri_replace_last_fixed(url_color, "/", "%2F"),
                            
                            # ELSE --> take the original url
                            url_color))

bike_data_colors_tbl %>% glimpse()

# Create function

library(future)
library(furrr)     # Parallel Processing using purrr (iteration)

get_sizes <- function(url) {
  
  json <- url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    str_replace("^[^\\{]+", "") %>%
    # 2. replace at the end everything after the last "}" with ""
    str_replace("[^\\}]+$", "") %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON(flatten = T) %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 2) %>%
    
    # select(id, value, available, availability)# %>%
    select(id, value, availability.onlyXLeftNumber) %>%
    
    # Rename
    rename(id_size = id) %>%
    rename(size = value) %>%
    rename(stock_availability = availability.onlyXLeftNumber) %>%
    
    # Conver to tibble
    as_tibble()
  
}

# Pull url vector
bike_url_color_vec <- bike_data_colors_tbl %>% 
  pull(url_color)

# Map
bike_data_sizes_tbl <- bike_data_colors_tbl %>% 
  mutate(size = future_map(bike_url_color_vec, get_sizes))

# Unnest
bike_data_sizes_tbl <- bike_data_sizes_tbl %>% 
  unnest(size)

saveRDS(bike_data_sizes_tbl, "bike_data_sizes_tbl.rds")

install.packages("readxl")

library(readxl)

bikes_tbl <- read_excel("DS_101/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
  
  # Separate product category name in main and sub
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # Renaming columns
  set_names(names(.) %>% str_replace_all("\\.", "_"))

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice((nrow(.)-3))

bikes_tbl %>%
  distinct(category_1)

bikes_tbl %>%
  distinct(category_1, category_2)

install.packages("readr")

library(readr)

bike_orderlines_tbl <- read_rds("DS_101/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

bike_orderlines_tbl %>%
  mutate(freight_costs = 2 * weight)

view(bike_orderlines_tbl)

bike_orderlines_tbl %>%
  mutate(total_price = log(total_price))

bike_orderlines_tbl %>%
  mutate(price_log = log(total_price)) %>%
  mutate(price_sqrt = total_price^0.5)

bike_orderlines_tbl %>%
  mutate(is_strive = model %>% str_to_lower() %>% str_detect("strive")) %>%
  filter(is_strive)

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>% 
  select(total_price, price_binned, everything())

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>%
  mutate(price_binned2 = case_when(
    total_price > quantile(total_price, 0.75) ~ "High",
    total_price > quantile(total_price, 0.25) ~ "Medium",
    TRUE ~ "Low" # Everything else
  )) %>% 
  select(total_price, price_binned, price_binned2, everything())

bike_orderlines_tbl %>%
  mutate(bike_type = case_when(
    model %>% str_to_lower() %>% str_detect("aeroad") ~ "Aeroad",
    model %>% str_to_lower() %>% str_detect("ultimate") ~ "Ultimate",
    TRUE ~ "Not Aeroad or Ultimate" # Everything else
  )) %>% 
  select(bike_type, everything())

bike_orderlines_tbl %>%
  summarise(
    revenue = sum(total_price)
  )

bike_orderlines_tbl %>%
  group_by(category_1) %>%
  summarise(revenue = sum(total_price))

bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(revenue = sum(total_price)) %>%
  # Always ungroup() after you summarise(). Left-over groups will cause difficult-to-detect errors.
  ungroup() %>%
  arrange(desc(revenue))

bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(
    count = n(),
    avg   = mean(total_price),
    med   = median(total_price),
    sd    = sd(total_price),
    min   = min(total_price),
    max   = max(total_price)
  ) %>%
  ungroup() %>%
  arrange(desc(count))

# Create total_price column and insert missing values for demonstration
bike_orderlines_missing <- bike_orderlines_tbl %>%
  mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

# detect missing (absolute)
bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.))))

# detect missing (relative)
bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.)) / length(.)))

# Handling missing data
bike_orderlines_missing %>%
  filter(!is.na(total_price))

bike_data_sizes_tbl %>% 
  select(model, year, price_euro, colors, size, stock_availability) %>% 
  pivot_wider(names_from  = size, 
              values_from = stock_availability)

bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
  select(bikeshop, category_1, total_price) %>%
  
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(sales))

bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>%
  pivot_wider(names_from  = category_1,
              values_from = sales) %>%
  mutate(
    Mountain = scales::dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Road     = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `Hybrid / City` = scales::dollar(`Hybrid / City`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `E-Bikes` = scales::dollar(`E-Bikes`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")
  )

bikeshop_revenue_formatted_tbl %>%
  pivot_longer(cols           = c(names(.)[2:6]),
               names_to       = "category_1",
               values_to      = "sales",
               values_drop_na = T) %>%
  mutate(sales =  sales %>% str_remove_all("€|\\.") %>% as.double())

order_dates_tbl <- bike_orderlines_tbl %>% select(1:3)
order_items_tbl  <- bike_orderlines_tbl %>% select(1:2,4:8)

order_dates_tbl %>%
  
  # By argument not necessary, because both tibbles share the same column names
  left_join(y = order_items_tbl, by = c("order_id" = "order_id", "order_line" = "order_line"))

bike_orderlines_tbl %>%
  select(-contains("category")) %>%
  
  bind_cols(
    bike_orderlines_tbl %>% select(category_1)
  )

train_tbl <- bike_orderlines_tbl %>%
  slice(1:(nrow(.)/2))

test_tbl <- bike_orderlines_tbl %>%
  slice((nrow(.)/2 + 1):nrow(.))

train_tbl %>%
  bind_rows(test_tbl)

bike_orderlines_tbl %>% 
  select(order_date) %>% 
  mutate(order_date = as.character(order_date)) %>%
  
  # separate
  separate(col  = order_date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  ) %>%
  
  # unite
  unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
  mutate(order_date_united = as.Date(order_date_united))

install.packages("data.table")

library(data.table)

url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)

# Create a large .csv file
test_df <- data.frame(matrix(runif(10000000), nrow=1000000))
write.csv(test_df, 'test_df.csv', row.names = F)

# Time taken by read.csv to import
system.time({test_df_base <- read.csv("test_df.csv")})
# Time taken by read_csv to import
system.time({test_df_readr <- read_csv("test_df.csv")})
# Time taken by fread to import
system.time({test_dt <- fread("test_df.csv")})

test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)

## FROM[WHERE, SELECT/ORDER BY/UPDATE, GROUP BY]

covid_data_dt[i, j, by]

# Example (filter by year, sum cases, group by continent)
covid_data_dt[year_week == "2020-52", sum(cases_weekly), by = continentExp]

covid_data_dt[country == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "Juni"]

covid_data_dt[order(year_week, -countriesAndTerritories)]

# Return as a vector
covid_data_dt[,geoId]
# Select multiple columns
covid_data_dt[,c("geoId", "countriesAndTerritories")]

# Return as a data.table
covid_data_dt[,list(geoId)]
# Short form using .
covid_data_dt[,.(geoId)]
# Select multiple columns
covid_data_dt[,.(geoId, countriesAndTerritories)]

# Rename them directly
covid_data_dt[,.(CountryCode = geoId, country = countriesAndTerritories)]

# select columns named in a variable using the ..prefix
select_cols = c("cases_weekly", "deaths_weekly")
covid_data_dt[, ..select_cols]

colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")

# List all internal data sets
data()

# Load specified data sets
data("airquality")

# Solution 1
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]


# Solution 2
setDT(airquality)
airquality[!is.na(Ozone), .(Solar.R, Wind, Temp)]

covid_data_dt[,sum(deaths_weekly > 1000)]

# to list the observations put it in i
covid_data_dt[deaths_weekly > 1000]

covid_data_dt[, deaths_per_capita := deaths_weekly / popData2019]

covid_data_dt[,  `:=`(deaths_per_capita = deaths_weekly / popData2019,
                      cases_per_capita = cases_weekly / popData2019,
                      deaths_per_cases = deaths_weekly / cases_weekly)]

# To delete a column, assign it to NULL
covid_data_dt[, date := NULL]

covid_data_dt[,date := lubridate::ymd(dateRep)]

data("mtcars") # step not absolutely necessary
mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

covid_data_dt[country == "Germany" & lubridate::month(dateRep, label = T, abbr = F) == "April", 
              .(m_cases = mean(cases_weekly), 
                m_death = mean(deaths_weekly)
              )
]

covid_data_dt[country == "United_States_of_America" & 
                lubridate::month(dateRep, label = T, abbr = F) == "Mai" & deaths_weekly < 15000, 
              length(year_week)
]

covid_data_dt[country == "United_States_of_America" & 
                lubridate::month(dateRep, label = T, abbr = F) == "Mai" & deaths_weekly < 10000, 
              .N
]

covid_data_dt[deaths_weekly < 1000, .I, by = country]

covid_data_dt[,country[deaths_weekly > 1000 & cases_weekly < 1000]]

covid_data_dt[continent == "Europe",
              .(mean(cases_weekly), mean(deaths_weekly)),
              by = .(country, lubridate::month(dateRep, label = T, abbr = F))
]

library(magrittr) # to use the pipe
mtcars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]

covid_cases_means <- covid_data_dt[,.(m_cases  = mean(cases_weekly) %>% round(1), 
                                      m_deaths = mean(deaths_weekly) %>% round(1)), 
                                   by = .(country)
]

covid_data_dt[, .(
  m_cases  = round(mean(cases_weekly),  digits = 1), 
  m_deaths = round(mean(deaths_weekly), digits = 1)
), 
by = .(country)][order(-m_cases)]

covid_data_dt[, country, 
              .(
                death_gt_1k = deaths_weekly > 1000, 
                cases_lt_1k = cases_weekly < 1000
              )
]

covid_data_dt[country == "Germany", print(.SD),]


covid_data_dt[, lapply(.SD, sum), 
              by = .(year_week), 
              .SDcols = c("cases_weekly", "deaths_weekly")
]

setkey(covid_data_dt, dateRep, country)

# Create a new data.table
covid_data_EUR_dt <- covid_data_dt[ continent == "Europe", 
                                    lapply(.SD, function(x) {
                                      x %>% 
                                        mean() %>% 
                                        round(1)
                                    }
                                    ), 
                                    by = .(country), 
                                    .SDcols = c("cases_weekly", "deaths_weekly")
]

# Set key
setkey(covid_data_EUR_dt, country)
key(covid_data_EUR_dt)

# Create two data.tables from that
cd_dt1 <- covid_data_EUR_dt[, .(country, cases_weekly)]
cd_dt2 <- covid_data_EUR_dt[1:20, .(country, deaths_weekly)]

# Join them
cd_dt1[cd_dt2]


# Remove keys
setkey(cd_dt1, NULL)
setkey(cd_dt2, NULL)
# Join
cd_dt1[cd_dt2, on = "country"]
# If they had different colnames
cd_dt1[cd_dt2, on = c(colA = "colB")]

# Alternatively you can use the function merge()
# Inner Join
merge(cd_dt1, cd_dt2, by='country')
# Left Join
merge(cd_dt1, cd_dt2, by='country', all.x = T)
# Outer Join
merge(cd_dt1, cd_dt2, by='country', all = T)
# If they had different colnames use by.x="colA", by.y="colB"


cd_dt1[cd_dt2, on = "country", deaths := i.deaths_weekly]

m  = matrix(1,nrow=100000,ncol=100)
DF = as.data.frame(m)
DT = as.data.table(m)

system.time(for (i in 1:10000) DF[i,1] <- i)
## 591 seconds

system.time(for (i in 1:10000) DT[i,V1:=i])
## 2.4 seconds  ( 246 times faster, 2.4 is overhead in [.data.table )

system.time(for (i in 1:10000) set(DT,i,1L,i))
## 0.03 seconds  ( 19700 times faster, overhead of [.data.table is avoided )


