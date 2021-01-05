#API Requests ----

library(glue)
name <- "Fred"
glue('My name is {name}.')

library(httr)
resp <- GET("https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
resp

rawToChar(resp$content)

#R Lists ----

data_list <- list(strings= c("string1", "string2"), 
                  numbers = c(1,2,3), 
                  TRUE, 
                  100.23, 
                  tibble(
                    A = c(1,2), 
                    B = c("x", "y")
                  )
)

library(jsonlite)
resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()


content(resp, as = "text")
content(resp, as = "parsed")
content(resp)
