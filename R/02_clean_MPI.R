####
# Cleaning pseudonimysed master patient index for care home residents
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(rlang)
library(data.table)

source("file_paths.R")
source("functions.R")


### What is it?
# A longitudinal record of care home resident characteristics

### Format
# - each row corresponds to a period of time for which the 
#  characteristics of one resident were valid 
# - residents can have multiple rows, that correspond to different time periods of different lengths

# Where do we want to get to?
# a monthly snap shot of the characteristics of each resident, over time 
# matched to care home characteristics at the time



# Load raw data ---------------------------------------------------------------

# CQC care home characteristics (clean monthly time series)
chchar <- readRDS(str_c(processed_data_path, 'chchar.Rds'))

mmpi <- read_csv(str_c(raw_data_path, "mmpisubset_pre57.csv"),
                 col_types = cols(pid = col_character(),
                                  tnrchid = col_character(),
                                  dob = col_date(format = "%d/%m/%Y"),
                                  datefrom = col_date(format = "%d/%m/%Y"),
                                  dateto = col_date(format = "%d/%m/%Y"),
                                  extdatefrom = col_date(format = "%d/%m/%Y"),
                                  extdateto = col_date(format = "%d/%m/%Y"),
                                  deathdate = col_date(format = "%d/%m/%Y"),
                                  deathmonth = col_date(format = "%d/%m/%Y"),
                                  extdatedeath = col_date(format = "%d/%m/%Y"),
                                  undeaddeathdate = col_date(format = "%d/%m/%Y"))) 


# Cleaning ----------------------------------------------------------------

mmpi_maxdate <- max(na.omit(mmpi$dateto))
saveRDS(mmpi_maxdate, str_c(processed_data_path, 'mmpi_maxdate.Rds')) 

mmpi <- mmpi %>% 
  select(pid, sex, dob, gppraccode, lsoa11pidpcd, ccg15pidlsoa, la11pidlsoa, datefrom,
         dateto, deathdate, deathmonth, tnrchid, undead, undeaddeathdate, uprn, 
         uprnparent) %>% 
  mutate(dateto_filled = case_when(is.na(dateto) & !is.na(deathdate) ~ deathdate,
                                   is.na(dateto) & is.na(deathdate) ~ ymd("2021-02-28"),  
                                   TRUE ~ dateto)) %>% 
  filter(dateto_filled >= ymd("2018-12-01"))

saveRDS(mmpi, str_c(processed_data_path, 'mmpi.Rds')) # just a backup 


# Remove extracts after patient death (set to last possible day a patient could have died)
mmpi <-  mmpi %>% 
  filter(is.na(deathdate) | datefrom <= deathdate) 

saveRDS(mmpi,  str_c(processed_data_path, 'mmpi.Rds'))
