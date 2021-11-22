####
# Hip fracture descriptives
####

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(tableone)
library(data.table)
library(zoo)

source("file_paths.R")
source("functions.R")

# Reference data ----------------------------------------------------------

hip_ops_codes <- read_csv(str_c(ref_data_path, "hip_ops.csv"))

# procedures --------------------------------------------------------------------

# SUS procedures codes from episodes table
# This was converted into a long-table and tagged with spell IDs by RB
# can be linked back to SUS spells using patient id and spell id
# this will be used to figure out if patients with a hip fracture code
# also had a procedure


apcc_proc <- fread(str_c(raw_data_path, "cips_procedures.csv"),
                   colClasses = c("pid" = "character"))

apcc_proc_hipfrac <- apcc_proc %>% 
  semi_join(hip_ops_codes, by = c("proccode" = "OPCS"))

apcc_proc_hipfrac <- apcc_proc_hipfrac %>% 
  left_join(hip_ops_codes, by = c("proccode" = "OPCS"))

apcc_proc_hipfrac <- apcc_proc_hipfrac %>% 
  as_tibble()

# remove complete duplicates
apcc_proc_hipfrac <- apcc_proc_hipfrac %>% 
  distinct(cipsid, pid, hip_ops, Description)

#categorise ops into hierarchy: total > partial > osteopin > other > no procedure (including imaging_op.site)

apcc_proc_hipfrac <- apcc_proc_hipfrac %>% 
  mutate(hip_proc_hierarchy = case_when(hip_ops == "hip_op_total" ~ 1,
                                        hip_ops == "hip_op_partial" ~ 2,
                                        hip_ops == "hip_op_osteopin" ~ 3))

apcc_proc_hipfrac <- apcc_proc_hipfrac %>% 
  group_by(cipsid) %>% 
  arrange(hip_proc_hierarchy) %>% 
  filter(row_number()==1)

apcc_proc_hipfrac %>%  group_by(cipsid) %>% count() %>%  arrange(desc(n))

saveRDS(apcc_proc_hipfrac,  str_c(processed_data_path, 'apce_proc_hipfrac.Rds'))
rm(apcc_proc)

# Import data -------------------------------------------------------------

apcc_clean <- readRDS(str_c(processed_data_path, "apcc_clean.Rds"))

# Filter for hip fracture admissions
apcc_hip <- apcc_clean %>%
  filter(hip_fracture == 1)

# FOR DATA CLEANING FLOWCHART
# check number of CIPS between 1 March 2019 and 28 Feb 2021 (study period)
apcc_hip %>%  
  ungroup() %>% 
  filter(cipsstartdate %within% interval(ymd("2019-03-01"), ymd("2021-02-28"))) %>% 
  nrow()


hip_nrow_beforejoin <- nrow(apcc_hip)

## join with hip fracture procedure codes
apcc_hip <- apcc_hip %>% 
  as_tibble() %>% 
  left_join(apcc_proc_hipfrac[,c("pid", "cipsid", "hip_ops")], by = c("pid", "cipsid"))

nrow(apcc_hip) ==  hip_nrow_beforejoin


apcc_hip <- apcc_hip %>% 
  mutate(hip_ops = fct_explicit_na(hip_ops, "no procedure"),
         hip_ops = fct_relevel(hip_ops, "no procedure", after = 0L)) 

apcc_hip %>% tabyl(hip_ops)

# Derive additional vars ------------------
apcc_hip <- apcc_hip %>% 
  mutate(year_adm = case_when(cipsstartdate %within% interval(ymd("2020-03-01"), ymd("2021-02-28")) ~ "2020/21",
                              cipsstartdate %within% interval(ymd("2019-03-01"), ymd("2020-02-29")) ~ "2019/20",
                              TRUE ~ NA_character_),
         year_adm = factor(year_adm, levels = c("2019/20", "2020/21")),
         cipsstartmonth_dummy = `day<-`(cipsstartdate, 01),
         cipsstartmonth_dummy = if_else(month(cipsstartmonth_dummy) %in% c(3:12), 
                                        `year<-`(cipsstartmonth_dummy, 0003), 
                                        `year<-`(cipsstartmonth_dummy, 0004)))


apcc_hip <- apcc_hip %>% 
  mutate(dischdest_ch = if_else(!is.na(dischdest) & dischdest %in% c(54, 65, 85, 88) , 1 , 0))
    


# Create 10-year age band variable i
apcc_hip <-  apcc_hip %>% 
  mutate(age_band = cut(mmpi_age, 
                        breaks = c(seq(65, 95, by = 5), Inf), 
                        include.lowest = TRUE, right = FALSE))

apcc_hip %>%  tabyl(mmpi_age, age_band)

# Create Charson Score categories 
apcc_hip <-  apcc_hip %>% 
  mutate(i_charlson_h36_5cat = cut(i_charlson_h36, 
                                       breaks = c(0, 1, 2, 3, 4, Inf), 
                                       include.lowest = TRUE, right = FALSE))
apcc_hip %>%  tabyl(i_charlson_h36_5cat)

apcc_hip <- apcc_hip %>% 
  filter(!is.na(year_adm))
  
# save analysis file
saveRDS(apcc_hip, str_c(processed_data_path, "apcc_hip.Rds"))

# Hip fracture management --------------------------------------------------------------
apcc_hip %>% 
  group_by(year_adm, hip_ops) %>% 
  summarise(n = n()) %>% 
  mutate(pct = round(100*(n/sum(n)),1))  %>% 
  write.csv(str_c(results_path, 'counts/hipadmissions_procedures.csv'))


apcc_hip %>% 
  group_by(year_adm, cipsstartmonth_dummy, hip_ops) %>% 
  summarise(admissions = n()) %>% 
  ggplot(aes(x = cipsstartmonth_dummy, y = admissions, group = year_adm, color = year_adm)) +
  geom_line() +
  geom_point(shape= 21, fill = "white", stroke = 1) +
  facet_wrap("hip_ops", ncol= 2, scales = "free_y") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.margin = margin(c(0,0,0,0)),
        legend.justification = c(1,0),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
  labs(title = "Hip fracture admissions", x= "", y = "Admissions")

(apcc_hip %>% 
  group_by(year_adm, cipsstartmonth_dummy, hip_ops) %>% 
  summarise(admissions = n()) %>% 
  group_by(year_adm, cipsstartmonth_dummy) %>% 
  mutate(pct = round(100*admissions / sum(admissions), 1)) %>% 
  ggplot(aes(x = cipsstartmonth_dummy, y = pct, group = hip_ops, color = hip_ops)) +
    geom_line() +
    geom_point(shape= 21, fill = "white", stroke = 1) +  facet_wrap("year_adm", ncol= 2) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.margin = margin(c(0,0,0,0)),
        legend.justification = c(1,0),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
  labs(title = "Hip fracture admissions", x= "", y = "Admissions")) %>% 
    ggsave(str_c(results_path, "counts/hipadmissions_procedures_monthly.png"), ., device = "png", 
           dpi = 600, width = 6, height = 3.7)

apcc_hip %>% 
  group_by(year_adm, cipsstartmonth_dummy, hip_ops) %>% 
  summarise(admissions = n()) %>% 
  group_by(year_adm, cipsstartmonth_dummy) %>% 
  mutate(pct = round(100*admissions / sum(admissions), 1)) %>% 
  write.csv(str_c(results_path, 'counts/hipadmissions_procedures_monthly.csv'))

# Admission numbers -------------------------------------------------------


apcc_hip %>% 
  group_by(format(cipsstartmonth_dummy, "%b-%Y"), year_adm) %>% 
  summarise(admissions = n()) %>% 
  write_csv(str_c(results_path, "counts/hip_admissions_all_monthly.csv"))

apcc_hip %>% 
  group_by(residence, format(cipsstartmonth_dummy, "%b-%Y"), year_adm) %>% 
  summarise(admissions = n()) %>% 
  write_csv(str_c(results_path, "counts/hip_admissions_residence_monthly.csv"))

# plot monthly admissions
(apcc_hip %>% 
  group_by(cipsstartmonth_dummy, year_adm) %>% 
  summarise(admissions = n()) %>% 
  ggplot(aes(x = cipsstartmonth_dummy, y = admissions, group = year_adm, color = year_adm)) +
  geom_line() +
  geom_point(shape= 21, fill = "white", stroke = 1) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  ylim(0, 5800) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.margin = margin(c(0,0,0,0)),
        legend.justification = c(1,0),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
  labs(title = "Hip fracture admissions", x= "", y = "Admissions")) %>% 
  ggsave(str_c(results_path, "counts/hip_admissions_all_monthly.png"), ., device = "png", 
         dpi = 600, width = 3, height = 3.7)


# plot monthly admissions by residence
admissions <- apcc_hip %>% 
  group_by(residence, format(cipsstartmonth_dummy, "%b-%Y"), year_adm) %>% 
  summarise(admissions = n())  %>% 
  rename(date_dummy = `format(cipsstartmonth_dummy, "%b-%Y")`, year = year_adm) %>% 
  pivot_longer(c(-residence, - date_dummy, - year), names_to = "type", values_to = "count") %>% 
  mutate(date_dummy = as.Date(paste0("01-",date_dummy), "%d-%B-%Y"))

(admissions %>% 
    mutate(type = case_when(type == "admissions" ~ "Admissions",
                            type == "live_discharges" ~ "Live discharges"),
           residence = str_to_title(residence),
           residence = factor(residence, levels = c("Residential", "Nursing", "Community"))) %>% 
    ggplot(aes(x = date_dummy, y = count, group = year, color = year)) +
    geom_line(linetype = "dashed") +
    geom_point(shape= 21, fill = "white", stroke = 1) +
    scale_x_date(date_labels = "%b", date_breaks = "2 month") +
    facet_wrap(type ~ residence, scales = "free_y") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                   scientific = FALSE)) +
    expand_limits(y=0)+
    theme_bw() +
    scale_color_manual(values = brewer.pal(name ="RdGy", n = 11)[c(9,2)], guide = guide_legend(reverse = TRUE))+
    theme(legend.title = element_blank(),
          legend.position = "top",
          legend.margin = margin(c(0,0,0,0)),
          legend.justification = c(1,0),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title = element_blank(),
          strip.background = element_blank())) %>% 
  ggsave("graphs/hip_admissions_residence_monthly.png", ., device = "png", 
         dpi = 600, width = 8, height = 3.7)

# Admissions - rolling averages -------------------------------------------

admission_roll <- apcc_hip %>%
  group_by(year_adm, cipsstartdate) %>% 
  arrange(cipsstartdate) %>% 
  summarise(admissions = n()) %>% 
  mutate(admissions_7dayavg = sevendayavg(admissions),
         cipsstartdate_dummy = if_else(month(cipsstartdate) %in% c(3:12), 
                                       `year<-`(cipsstartdate, 0003), 
                                       `year<-`(cipsstartdate, 0004)))

write_csv(admission_roll, str_c(results_path, "counts/hip_admissions_all_7dayavg.csv"))

admission_roll <- admission_roll %>% 
  rename(date_dummy = cipsstartdate_dummy, year = year_adm) %>% 
  select(year, date_dummy, pct_died_in_hospital_7dayavg)

(admission_roll %>% 
    ggplot(aes(x = date_dummy, y = pct_died_in_hospital_7dayavg, group = year, color = year)) +
    geom_line() +
    scale_x_date(date_labels = "%b", date_breaks = "2 month") +
    expand_limits(y=0)+
    theme_bw() +
    ylab("Hospital mortality (%)") +
    scale_color_manual(values = brewer.pal(name ="RdGy", n = 11)[c(9,2)])+
    theme(legend.title = element_blank(),
          legend.position = "top",
          legend.margin = margin(c(0,0,0,0)),
          legend.justification = c(1,0),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank())) %>% 
  ggsave("graphs/hip_admissions_mortality_7dayavg.png", ., device = "png", 
         dpi = 600, width = 3.5, height = 3.5)


# cohort descriptives -----------------------------------------------------

vars_tosummarise <- c("sex", "mmpi_age", "age_band", "los", "covid_any", "region_name",
                      "imd_quintile", 'hip_ops',
                      "i_charlson_h36", "i_charlson_h36_5cat", 
                      "nr_frailty_h36", "ec_chf_h36", "ec_cpd_h36", 
                      "ec_hiv_h36", "ec_plegia_h36", "ec_stumourmets_h36", "c_dementia_h36", 
                      "c_diabchrcomp_h36", "c_livermild_h36", "c_livermodsev_h36", 
                      "c_renaldis_h36", "c_rheum_h36", "c_cancer_h36", "c_mi_h36", 
                      "c_cvd_h36", "f_anxdep_h36", "f_depend_h36", "f_fallsfract_h36", 
                      "f_incont_h36", "f_mobprob_h36", "f_pulcers_h36", "f_cogimpair_h36", 
                      "f_delirium_h36", "f_dementia_h36", "f_senility_h36", "dischdest_ch")

cat_vars_tosummarise <- c("age_band", 'sex',"region_name","covid_any", "imd_quintile", 'hip_ops',
                          "i_charlson_h36_5cat", 
                          "ec_chf_h36", "ec_cpd_h36", 
                          "ec_hiv_h36", "ec_plegia_h36", "ec_stumourmets_h36", "c_dementia_h36", 
                          "c_diabchrcomp_h36", "c_livermild_h36", "c_livermodsev_h36", 
                          "c_renaldis_h36", "c_rheum_h36", "c_cancer_h36", "c_mi_h36", 
                          "c_cvd_h36", "f_anxdep_h36", "f_depend_h36", "f_fallsfract_h36", 
                          "f_incont_h36", "f_mobprob_h36", "f_pulcers_h36", "f_cogimpair_h36", 
                          "f_delirium_h36", "f_dementia_h36", "f_senility_h36", "dischdest_ch")


# Table 1 for admissions
table1_adm_year_residence <- CreateTableOne(vars = vars_tosummarise, 
                                        data = apcc_hip %>% 
                                          filter(!is.na(year_adm)), 
                                        strata = c("year_adm",  'residence'), 
                                        factorVars = cat_vars_tosummarise,
                                        test = FALSE, includeNA = TRUE)

table1_adm_year_residence_csv <- print(table1_adm_year_residence, 
                                        noSpaces = TRUE,
                                        showAllLevels = FALSE,
                                        formatOptions = list(big.mark = ",")) 

write.csv(table1_adm_year_residence_csv, str_c(results_path, 'chars/hipadmissions_chars_year_residence.csv'))

table1_adm_year_residence_covid <- CreateTableOne(vars = vars_tosummarise, 
                                            data = apcc_hip %>% 
                                              filter(!is.na(year_adm) ) %>% 
                                              mutate(covid_any = if_else(year_adm == "2019/20", 0, covid_any)), 
                                            strata = c("year_adm", "covid_any", 'residence'), 
                                            factorVars = cat_vars_tosummarise,
                                            test = FALSE, includeNA = TRUE)

table1_adm_year_residence_covid_csv <- print(table1_adm_year_residence_covid, 
                                       noSpaces = TRUE,
                                       showAllLevels = FALSE,
                                       formatOptions = list(big.mark = ",")) 

write.csv(table1_adm_year_residence_covid_csv, str_c(results_path, 'chars/hipadmissions_chars_residence_covid.csv'))


table1_adm_monthly <- CreateTableOne(vars = vars_tosummarise, 
                                            data = apcc_hip %>% 
                                              filter(!is.na(year_adm)), 
                                            strata = c('cipsstartmonth_dummy',"year_adm"), 
                                            factorVars = cat_vars_tosummarise,
                                            test = FALSE, includeNA = TRUE)

table1_adm_monthly_csv <- print(table1_adm_monthly, 
                                       noSpaces = TRUE,
                                       showAllLevels = FALSE,
                                       formatOptions = list(big.mark = ",")) 

write.csv(table1_adm_monthly_csv, str_c(results_path, 'chars/hipadmissions_chars_monthly.csv'))

# Standardised mean variance ----------------------------------------------

table1_adm_year_residence_smd <- ExtractSmd(table1_adm_year_residence) %>%  
  as.data.frame() %>% 
  rownames_to_column(var = "variable") %>% 
  select(variable,
         `Residential: 2019/20 vs 2020/21` = `5 vs 6`,
         `Nursing: 2019/20 vs 2020/21` = `3 vs 4`,
         `Community: 2019/20 vs 2020/21` = `1 vs 2`) 

write.csv(table1_adm_year_residence_smd, str_c(results_path, 'chars/hipadmissions_chars_year_residence_SMD.csv'))



table1_adm_year_residence_covid_smd <- ExtractSmd(table1_adm_year_residence_covid) %>%  
  as.data.frame() %>% 
  rownames_to_column(var = "variable") %>% 
  select(variable,
         `Nursing: 2019/20 all vs 2020/21 No COVID-19` = `5 vs 6`,
         `Nursing: 2019/20 all vs 2020/21 COVID-19` = `5 vs 8`,
         `Nursing: 2020/21 No COVID-19 vs 2020/21 COVID-19` = `6 vs 8`,
         
         `Residential: 2019/20 all vs 2020/21 No COVID-19` = `9 vs 10`,
         `Residential: 2019/20 all vs 2020/21 COVID-19` = `9 vs 12`,
         `Residential: 2020/21 No COVID-19 vs 2020/21 COVID-19` = `10 vs 12`,
         
         `Community: 2019/20 all vs 2020/21 No COVID-19` = `1 vs 2`,
         `Community: 2019/20 all vs 2020/21 COVID-19` = `1 vs 4`,
         `Community: 2020/21 No COVID-19 vs 2020/21 COVID-19` = `2 vs 4`) 

write.csv(table1_adm_year_residence_covid_smd, str_c(results_path, 'chars/hipadmissions_chars_residence_covid_SMD.csv'))
