library(RSQLite)
library(dplyr)

con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "DS_101/DS_101/00_data/02_chinook/Chinook_Sqlite.sqlite")

dbListTables(con)

tbl(con, "Album")

tbl(con, "Artist")

album_tbl <- tbl(con, "Album") %>% collect()

dbDisconnect(con)
con