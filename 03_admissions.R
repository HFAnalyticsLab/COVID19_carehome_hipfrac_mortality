####
# Process hospital admissions
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(data.table)
library(readxl)
library(ISOweek)

source("file_paths.R")
source("functions.R")

# Reference data ----------------------------------------------------------

study_period <- interval(ymd("2019-03-01"), ymd("2021-02-28"))

# Diagnoses --------------------------------------------------------------------

# SUS diagnosis codes from episodes table
# This was converted into a long-table and tagged with spell IDs by RB
# can be linked back to SUS spells using patient id and spell id
# this will be used to figure out if *any* diagnosis code was covid, as 
# the spells table only contains the primary diagnosis code


apcc_diag <- fread(str_c(raw_data_path, "cips_diagnoses.csv"),
                   colClasses = c("pid" = "character"))

apcc_diag_covid <- apcc_diag %>% 
  filter(diagcode == "U071"| diagcode == "U072") 

# IDs of spells that have a covid diagnosis code
apcc_diag_covid <- apcc_diag_covid %>% 
  distinct(cipsid, pid) %>% 
  mutate(covid = 1)

saveRDS(apcc_diag_covid,  str_c(processed_data_path, 'apce_diag_covid.Rds'))
rm(apcc_diag)

# Import data -------------------------------------------------------------

# SUS CIPS
apcc <- fread(str_c(raw_data_path, "flatfile.csv"),
              colClasses = c("pid" = "character")) 

# Need this table to stay in data.table format
# Convert column to dates
# This can't be done during the fread step as dates are in non-ISO format
apcc[, cipsstartdate := as.Date(cipsstartdate, "%d/%m/%Y")]
apcc[, cipsenddate := as.Date(cipsenddate, "%d/%m/%Y")]
apcc[, dischdate := as.Date(dischdate, "%d/%m/%Y")]


# MMPI
mmpi <- readRDS(str_c(processed_data_path, 'mmpi.Rds'))

# Care home details
chchar <- readRDS(str_c(processed_data_path, 'chchar.Rds'))


# Processing --------------------------------------------------------------

# Second step:
# Now we need to make sure that the patient had a care home id at the time of admission
# This is done using the interval join function from the data.table package

apcc[, cipsstartdate2 := cipsstartdate]

mmpi_dt <-  mmpi %>% 
  select(pid, sex, mmpi_datefrom = datefrom, mmpi_dateto = dateto_filled,
         mmpi_dob = dob, mmpi_deathdate = deathdate, mmpi_deathmonth = deathmonth, mmpi_tnrchid = tnrchid) %>% 
  as.data.table()

setkey(mmpi_dt, pid, mmpi_datefrom, mmpi_dateto)

apcc <- foverlaps(apcc, mmpi_dt, 
                  by.x = c("pid", "cipsstartdate", "cipsstartdate2"),
                  by.y = c("pid", "mmpi_datefrom", "mmpi_dateto"))

# Save row number to check against after de-duplication
num_apccs <- nrow(apcc)

# Un-nest multiple care home matches 
apcc <- apcc %>% 
  mutate(mmpi_tnrchid = strsplit(mmpi_tnrchid, split = ","))

apcc <- apcc %>% 
  mutate(multiple_tnrchid = if_else(!is.na(mmpi_tnrchid) & lengths(mmpi_tnrchid) > 1, 1, 0))
  
apcc <- apcc %>% 
  unnest_dt(col = mmpi_tnrchid)%>%
  mutate(mmpi_tnrchid = as.numeric(mmpi_tnrchid))

# Join with care home characteristics to de-duplicate

chchar_dt <-  chchar %>% 
  select(chchar_tnrchid = ps_loc_id_list, chchar_startdate = start_date, chchar_enddate = end_date,
         chchar_nursinghome = ch_nursing, chchar_beds = total_beds) %>% 
  as.data.table()

setkey(chchar_dt, chchar_tnrchid, chchar_startdate, chchar_enddate)

apcc <- foverlaps(apcc, chchar_dt, 
                  by.x = c("mmpi_tnrchid", "cipsstartdate", "cipsstartdate2"),
                  by.y = c("chchar_tnrchid", "chchar_startdate", "chchar_enddate"))


## Final data set

apcc_todedup <- apcc %>% 
  filter(multiple_tnrchid == 1)

apcc_keep <- apcc %>% 
  filter(multiple_tnrchid == 0)

# free up working space
rm(mmpi)
rm(chchar)
rm(chchar_dt)
rm(mmpi_dt)
rm(apcc)

apcc_todedup <- apcc_todedup %>%
  distinct(across(!matches("mmpi_tnrchid")) , .keep_all = TRUE)
  
# these are the ones we can't resolve by simple deduplication
# Use following method:
# 1. Pick care home that was open (eg match in care home table)
# 2. Pick the care home with earlier start date
# 3. if the same, pick nursing home over residential for now
apcc_todedup %>%   
  ungroup() %>% 
  filter(cipsstartdate %within% study_period) %>% 
  filter(!is.na(primdiag) & substr(primdiag, 1, 4) %in% c("S720", "S721","S722")) %>% 
  filter(floor(time_length(interval(mmpi_dob, cipsstartdate), "years")) >= 65) %>% 
  filter(admmthd != 25) %>% 
  filter(!is.na(chchar_startdate)) %>% 
  group_by(pid, cipsid) %>% 
  mutate(valid_matches = sum(!is.na(chchar_startdate))) %>% 
  arrange(pid, cipsid, chchar_startdate, desc(chchar_nursinghome)) %>% 
  select(mmpi_tnrchid, chchar_startdate, chchar_nursinghome, pid, cipsid, cipsstartdate, primdiag, valid_matches) %>% 
  filter(valid_matches >1) %>%
  distinct(pid, cipsid) %>%  View()

apcc_todedup <- apcc_todedup %>% 
  group_by(pid, cipsid) %>% 
  arrange(pid, cipsid, chchar_startdate, desc(chchar_nursinghome)) %>% 
  filter(row_number() == 1)       


apcc_clean <- apcc_keep %>% 
  bind_rows(apcc_todedup)

# check that we have the same number of cips as before unnesting
nrow(apcc_clean) == num_apccs

# Derive variables --------------------------------------------------------

# COVID admission
apcc_clean <- apcc_clean %>% 
  left_join(apcc_diag_covid, by = c("cipsid", "pid")) %>% 
  mutate(covid_any = if_else(is.na(covid), 0, covid),
         covid_prim = if_else(primdiag == "U071"| primdiag == "U072", 1, 0))

# care home admission
apcc_clean <- apcc_clean %>% 
  mutate(residence = case_when(!is.na(mmpi_tnrchid) & chchar_nursinghome == 0 ~ "residential",
                               !is.na(mmpi_tnrchid) & chchar_nursinghome == 1 ~ "nursing",
                               TRUE ~ "community"))

# hip fracture
apcc_clean <- apcc_clean %>% 
  mutate(hip_fracture = if_else((!is.na(primdiag) & substr(primdiag, 1, 4) %in% c("S720", "S721","S722")), 1, 0))
         
# hospital death
apcc_clean <- apcc_clean %>% 
  mutate(hospital_death = if_else((!is.na(dischdest) & dischdest == 79) | 
                                    (!is.na(dischmthd) & dischmthd == 4), 1, 0))

# 30 day readmission
apcc_clean <- apcc_clean %>% 
  group_by(pid) %>% 
  arrange(pid, cipsstartdate) %>% 
  mutate(has_readmission = if_else(hospital_death == 0 &
                                     lead(emergency) == 1 &
                                     lead(cipsstartdate) <= cipsenddate + 30, 1, 0),
         has_readmission = if_else(is.na(has_readmission), 0, has_readmission),
         is_readmission = if_else(lag(has_readmission == 1), 1, 0),
         is_readmission = if_else(is.na(is_readmission), 0, is_readmission))

# FOR SENSITIVITY ANALYSIS 
# 30 day readmission, excluding 0 day admissions (readmission has to be overnight)
# CAUTION: the variable is only properly created for hip ffracture admissions, which
# is what we care about.
apcc_clean <- apcc_clean %>% 
  left_join(apcc_clean %>% 
    ungroup() %>% 
    filter(hip_fracture == 1 | los > 0) %>% 
    group_by(pid) %>% 
    arrange(pid, cipsstartdate) %>% 
    mutate(has_ON_readmission = if_else(hospital_death == 0 &
                                           lead(emergency) == 1 &
                                          lead(cipsstartdate) <= cipsenddate + 30, 1, 0)) %>% 
    select(pid, cipsid, cipsstartdate, has_ON_readmission))
         
apcc_clean <- apcc_clean %>%
  mutate(has_ON_readmission = replace_na(has_ON_readmission, 0))

apcc_clean %>%  ungroup() %>%  filter(hip_fracture == 1) %>% tabyl(has_readmission, has_ON_readmission) %>%  adorn_title()

# some checks

hip_patients <- apcc_clean %>%  ungroup() %>% 
  filter(hip_fracture == 1) %>%  
  pull(pid) %>% unique()

apcc_clean %>% 
  ungroup() %>% 
  filter(pid %in% hip_patients) %>% 
  select(pid, cipsstartdate, cipsenddate, los, hip_fracture, has_readmission, has_ON_readmission) %>% 
  arrange(pid, cipsstartdate) %>% 
  View()


###

# Additional variables
apcc_clean <- apcc_clean %>% 
  mutate(cipsstartweek = format(cipsstartdate, format = "%V"), 
         # wday assumes Sunday == day 1, I want to shift weekstart dates to Monday
         cipsstartweekstart = cipsstartdate - (wday(cipsstartdate)-2),
         cipsendweek = format(cipsenddate, format = "%V"),
         cipsendweekstart = cipsenddate - (wday(cipsenddate)-2))



apcc_clean <- apcc_clean %>% 
  mutate(mmpi_age = floor(time_length(interval(mmpi_dob, cipsstartdate), "years"))) 
  
imd_labels <- c('1 least deprived', '2', '3', '4', '5 most deprived')
apcc_clean <- apcc_clean %>% 
  mutate(imd_quintile = cut(imddec2019, breaks = seq(0, 10, by = 2), labels = imd_labels),
         imd_quintile = fct_explicit_na(imd_quintile, na_level = "Missing")) 



saveRDS(apcc_clean, str_c(processed_data_path, "apcc_clean_unfiltered.Rds"))

# Exclusions --------------------------------------------------------------


# FOR DATA CLEANING FLOWCHART
# check number of CIPS between 1 March 2019 and 28 Feb 2021 (study period)
apcc_clean %>%  
  ungroup() %>% 
  filter(cipsstartdate %within% study_period) %>% 
  nrow()

# Age at admission 65 or over
apcc_clean <- apcc_clean %>% 
  filter(mmpi_age >= 65) 

# FOR DATA CLEANING FLOWCHART
# check number of CIPS between 1 March 2019 and 28 Feb 2021 (study period)
apcc_clean %>%  
  ungroup() %>% 
  filter(cipsstartdate %within% study_period) %>% 
  nrow()


# Exclude mental health admissions
apcc_clean <- apcc_clean %>% 
  filter(admmthd != 25)

# FOR DATA CLEANING FLOWCHART
# check number of CIPS between 1 March 2019 and 28 Feb 2021 (study period)
apcc_clean %>%  
  ungroup() %>% 
  filter(cipsstartdate %within% study_period) %>% 
  nrow()

# FOR DATA CLEANING FLOWCHART
# check number of *hip fracture* CIPS between 1 March 2019 and 28 Feb 2021 (study period)
apcc_clean %>%  
  ungroup() %>% 
  filter(hip_fracture == 1) %>% 
  filter(cipsstartdate %within% study_period) %>% 
  nrow()

saveRDS(apcc_clean, str_c(processed_data_path, "apcc_clean.Rds"))
