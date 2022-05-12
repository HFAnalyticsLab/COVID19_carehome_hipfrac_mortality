####
# Cleaning pseudonimysed CQC care home characteristics
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(data.table)


source("file_paths.R")
source("functions.R")

### What is it?
# A longitudinal record of care homes regulated by the CQC along with their characteristics,
# such as bed capacity, specialties, open and close date etc.

### Format
# - each row corresponds to a period of time (from start_date to end_date) for which the 
#  characteristics of one care home (ps_loc_id) were valid 
# - care homes can have multiple rows, that correspond to different time periods of different lengths

# Load raw data ---------------------------------------------------------------
# NB this refresh of the data seems to be encoded differently, had to use a different function
chchar_raw <- read.delim(str_c(raw_data_path, "pseudo_carehome_characteristics_Oct20.csv"),
                         sep = "¬", fileEncoding = "UTF-16") %>% 
  mutate_at(c("start_date", "end_date"), as_datetime, "%Y-%m-%d %H:%M:%S", tz = "GMT") 


max_enddate <- as_datetime("2021-02-28 23:59:59", "%Y-%m-%d %H:%M:%S",  tz = "GMT")


chchar_raw <- chchar_raw %>% 
  mutate(end_date = if_else(is.na(end_date), max_enddate, end_date))

# Tidying up start and end dates
chchar_raw <- chchar_raw %>% 
  mutate(end_date = if_else(hour(end_date) == 0, end_date - seconds(1), end_date),
         start_date = if_else(hour(start_date) == 23, start_date + seconds(1), start_date),
         start_date = floor_date(start_date, unit = "day"),
         end_date = floor_date(end_date, unit = "day"))

# unnesting rows with multiple care home IDs (this might correspond to different locations
# belonging to the same care homes)
chchar_raw <- chchar_raw %>% 
  mutate(ps_loc_id_list = strsplit(ps_loc_id_list, split = ","))


chchar <- chchar_raw %>% 
  unnest_dt(col = ps_loc_id_list)%>%
  mutate(ps_loc_id_list = as.numeric(ps_loc_id_list))

# check for gaps and overlap

chchar <- chchar %>% 
  filter(end_date >= start_date) %>% 
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(end_date),
         days_valid = count_days_covered(start_date, end_date)) %>%
  group_by(ps_loc_id_list) %>% 
  arrange(ps_loc_id_list, start_date, desc(days_valid)) %>% 
  mutate(days_gap_to_next = count_days_missing(end_date, lead(start_date)),
         days_overlap_with_previous = pmax(count_days_overlap(lag(end_date), start_date),
                                             count_days_overlap(lag(end_date, 2), start_date),
                                             count_days_overlap(lag(end_date, 3), start_date)))

tabyl(chchar$days_gap_to_next)
tabyl(chchar$days_overlap_with_previous) 


saveRDS(chchar,  str_c(processed_data_path, 'chchar.Rds'))
                  