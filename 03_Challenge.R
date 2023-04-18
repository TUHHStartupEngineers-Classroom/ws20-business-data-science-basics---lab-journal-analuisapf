library(vroom)
# Tidyverse
library(tidyverse)


# Data Table
library(data.table)
#install.packages("tictoc")
# Counter
library(tictoc)
col_types_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

setDT(patent_tbl)

col_types_assignee <- list(
  id = col_character(),
  type = col_double(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)

setDT(assignee_tbl)

col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

setDT(patent_assignee_tbl)

col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_double()
)

patent_uspc <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)

setDT(patent_uspc)



# 1: Patent Dominance: ----
# What US company / corporation has the most patents? List the 10 US companies with the most assigned/granted patents.


tic()
combined_data_ass_patass <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                       by.x = "id", by.y = "assignee_id", 
                       all.x = TRUE, 
                       all.y = TRUE)
toc()


combined_data_ass_patass %>% glimpse()
class(combined_data_ass_patass)

tic()
patent_dominance <- combined_data_ass_patass[!is.na(organization), .N, by = organization][order(-N),][1:10]
toc()

view(patent_dominance)

# pacman::p_load("tidylog")
# cdap2 <- combined_data_ass_patass %>% 
#   mutate(n=1) %>% 
#   group_by(id) %>% 
#   summarise_at("n", sum) %>% 
#   inner_join(combined_data_ass_patass) %>% 
#   group_by(id) %>% 
#   filter(row_number() == 1) %>% 
#   dplyr::select(organization, n) %>%
#   arrange(-n) %>% 
#   ungroup() %>% 
#   filter(row_number() %in% 1:10)

# 2: Recent patent acitivity: ----
# What US company had the most patents granted in 2019? List the top 10 companies with the most new granted patents for 2019.
# Adaptation: Month of May

tic()
combined_data_ass_patass_pat <- merge(x = combined_data_ass_patass, y = patent_tbl, 
                                  by.x = "patent_id", by.y = "id",
                                  all.x = TRUE, 
                                  all.y = TRUE)
toc()

#patent_assignee_tbl[(patent_id %like% "8636251")]

tic()
patent_activity <- combined_data_ass_patass_pat[(!is.na(organization) & lubridate::month(date, label = T, abbr = F) == "Mai"), .N, by = organization][order(-N),][1:10]
toc()


#3: Innovation in Tech: ----
# What is the most innovative tech sector? For the top 10 companies (worldwide) with the most patents, what are the top 5 USPTO tech main classes?

tic()
combined_data_ass_patass_uspc <- merge(x = combined_data_ass_patass, y = patent_uspc, 
                                      by = "patent_id",
                                      all.x = TRUE, 
                                      all.y = TRUE)
toc()

tic()
innovation_in_tech <- combined_data_ass_patass_uspc[!is.na(id), .N, by = organization][order(-N),][1:10]
toc()
