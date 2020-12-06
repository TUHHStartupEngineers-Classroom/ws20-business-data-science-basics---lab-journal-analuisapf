library(httr)
library(glue)

token <- Sys.getenv("apikey")


resp <- GET(glue("http://api.openweathermap.org/data/2.5/weather?q=London,uk&APPID={token}"))

resp

rawToChar(resp$content)
