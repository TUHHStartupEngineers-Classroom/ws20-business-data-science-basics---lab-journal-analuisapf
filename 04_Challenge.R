library(tidyverse)
library(lubridate)
install.packages("ggrepel")
library(ggrepel)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

covid_data_1_tbl <- covid_data_tbl %>%
  select(continentExp,dateRep, cases_weekly) %>%
  
  group_by(continentExp, lubridate::month(dateRep, label = T, abbr = F)) %>%
  summarise(cases = sum(cases_weekly)) %>%
  ungroup() %>%
  
  rename(month = "lubridate::month(dateRep, label = T, abbr = F)") %>%
  
  group_by(continentExp) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  ungroup() %>%
  
  mutate(continentExp = fct_reorder2(continentExp, month, cum_cases))

covid_data_1_tbl

covid_data_1_tbl %>%
  mutate(continentExp_num = as.numeric(continentExp)) %>%
  arrange(continentExp)


covid_data_1_tbl %>%
  
  # Put the aes color mapping here, to apply it to geom_line and geom_point
  ggplot(aes(month, cases, color = continentExp)) +
  
  # Or you could do it locally in each geom 
  # (aes mapping only necessary if you map it to a column)
  geom_line(size = 1) + # geom_line(aes(color = category_1))
  geom_point(color = "dodgerblue", size = 5)

pacman::p_load("ggplot2")
# Put the aes color mapping here, to apply it to geom_line and geom_point
ggplot() +
  geom_line(data=covid_data_1_tbl, aes(x=month, y=cum_cases,group=continentExp, color = continentExp))+
  
  theme_light() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  ) +
  
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of April 2020, Europe had more cases than the USA",
    x = "Year 2020",
    y = "Cumulative Cases",
    fill = "",
    caption = ""
  )

# Challenge 2 ----
install.packages("maps")
library(maps)

world <- map_data("world")

covid_data_2_tbl <- covid_data_tbl %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "United States",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  )) %>%
  
  
  select(countryterritoryCode,countriesAndTerritories, deaths_weekly, popData2019) %>%
  mutate(mortality = deaths_weekly/popData2019) %>%
  
  group_by(countryterritoryCode) %>%
  summarise(mortality_total = sum(mortality)) %>%
  ungroup() %>%
  
  mutate(mortality_pct = scales::percent(mortality_total, accuracy = 0.0001))


  # left_join(y = world, by = c("countriesAndTerritories" = "region")) #%>%
  # distinct() %>%
  # 
  # group_by(countriesAndTerritories) %>%
  # summarise(mortality = sum(cases_weekly)/popData2019) %>%
  # mutate(mortality_pct = scales::percent(mortality)) %>%
  # ungroup() %>%
  # distinct() #%>%
  # mutate ()
  
class(covid_data_2_tbl$mortality_total)
class(covid_data_2_tbl$mortality_pct)
head(covid_data_2_tbl$mortality_pct)

# covid_data_2_tbl %>% 
#   ggplot() +
#   geom_map(aes(fill=mortality_pct, map_id = countriesAndTerritories), map = world)
# 
#   geom_map(aes(fill=mortality_pct, map_id = countriesAndTerritories), map = world)
#   geom_map(aes(mortality_pct, countriesAndTerritories, map_id = countriesAndTerritories, data = covid_data_2_tbl), map = world)


  
pacman::p_load("tmap", "sp", "spdplyr")
data("World")

# plot(World)
# class(World)

World <- as(World, "Spatial")
plot(World)
World@proj4string

World <- World %>%
  left_join(covid_data_2_tbl, by = c("iso_a3"="countryterritoryCode"))



tm_shape(World)+
  tm_polygons("mortality_total", palette="YlOrRd",style="cont", n=10)+
  tm_layout(legend.outside = TRUE) 


head(World$sovereignt)


