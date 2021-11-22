####
# Hip fracture outcomes
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

apcc_hip <- readRDS(str_c(processed_data_path, "apcc_hip.Rds"))

# Admission outcomes summaries  ---------------------------------------------------------

hip_all_monthly_adm <- apcc_hip %>% 
  group_by(cipsstartmonth_dummy, year_adm) %>% 
  summarise(admissions = n(),
            died_in_hospital = sum(hospital_death == 1),
            pct_died_in_hospital = round(100*(died_in_hospital/admissions),2),
            mean_los = round(mean(los), 1),
            sd_los = round(sd(los), 1),
            median_los = median(los),
            median_los_n = sum(los == median_los),
            discharged_alive = sum(hospital_death == 0),
            has_readmission = sum(has_readmission == 1),
            pct_has_readmission = round(100*(has_readmission/discharged_alive), 2))

hip_all_monthly_adm %>% 
  mutate(cipsstartmonth_dummy = format(cipsstartmonth_dummy, "%b-%Y")) %>% 
  write_csv(str_c(results_path, "outcomes/hip_admissions_all_outcomes.csv"))

(hip_all_monthly_adm %>% 
  ggplot(aes(x = cipsstartmonth_dummy, y = pct_died_in_hospital, color = year_adm, group = year_adm)) +
  geom_point() +
  geom_line()+
  theme_bw() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  expand_limits(y=0)+
  theme(legend.position = "top",
        legend.justification = c(1,0),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.margin = margin(c(0,0,0,0)),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
  labs(title = "Hip fracture admissions", x= "", y = "Mortality (%)")) %>%  
  ggsave(str_c(results_path, "outcomes/hip_mortality_all_monthly.png"), ., device = "png", 
       dpi = 600, width = 3, height = 3.7)

(hip_all_monthly_adm %>% 
    ggplot(aes(x = cipsstartmonth_dummy, y = mean_los, color = year_adm, group = year_adm)) +
    geom_point() +
    geom_line()+
    theme_bw() +
    expand_limits(y=0)+
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme(legend.position = "top",
          legend.justification = c(1,0),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.margin = margin(c(0,0,0,0)),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
    labs(title = "Hip fracture admissions", x= "", y = "Mean length of stay (days)")) %>%  
  ggsave(str_c(results_path, "outcomes/hip_losmean_all_monthly.png"), ., device = "png", 
         dpi = 600, width = 3, height = 3.7)

#By residence
hip_residence_monthly_adm <- apcc_hip %>% 
  group_by(residence, cipsstartmonth_dummy, year_adm) %>% 
  summarise(admissions = n(),
            died_in_hospital = sum(hospital_death == 1),
            pct_died_in_hospital = round(100*(died_in_hospital/admissions),2),
            mean_los = round(mean(los), 1),
            sd_los = round(sd(los), 1),
            median_los = median(los),
            median_los_n = sum(los == median_los),
            discharged_alive = sum(hospital_death == 0),
            has_readmission = sum(has_readmission == 1),
            pct_has_readmission = round(100*(has_readmission/discharged_alive), 2))

hip_residence_monthly_adm %>% 
  mutate(cipsstartmonth_dummy = format(cipsstartmonth_dummy, "%b-%Y")) %>% 
  write_csv(str_c(results_path, "outcomes/hip_admissions_residence_outcomes.csv"))

(hip_residence_monthly_adm %>% 
  ggplot(aes(x = cipsstartmonth_dummy, y = pct_died_in_hospital, color = year_adm, group = year_adm)) +
  geom_point() +
  geom_line()+
  theme_bw() +
    expand_limits(y=0)+
  facet_wrap("residence", scale = "free_y") +
  scale_x_date(date_labels = "%b", date_breaks = "2 month") +
  theme(legend.position = "top",
        legend.justification = c(1,0),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.margin = margin(c(0,0,0,0)),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
  labs(title = "Hip fracture admissions", x= "", y = "Mortality (%)")) %>%  
  ggsave(str_c(results_path, "outcomes/hip_mortality_residence_monthly.png"), ., device = "png", 
       dpi = 600, width = 6, height = 4)


(hip_residence_monthly_adm %>% 
    ggplot(aes(x = cipsstartmonth_dummy, y = mean_los, color = year_adm, group = year_adm)) +
    geom_point() +
    geom_line()+
    theme_bw() +
    facet_wrap("residence") +
    expand_limits(y=0)+
    scale_x_date(date_labels = "%b", date_breaks = "2 month") +
    theme(legend.position = "top",
          legend.justification = c(1,0),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.margin = margin(c(0,0,0,0)),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
    labs(title = "Hip fracture admissions", x= "", y = "Mean length of stay (days)")) %>%  
  ggsave(str_c(results_path, "outcomes/hip_losmean_residence_monthly.png"), ., device = "png", 
         dpi = 600, width = 6, height = 4)



# Admissions outcomes - 7 day averages ------------------------------------

admission_outcome_all_roll <- apcc_hip %>%
  group_by( year_adm, cipsstartdate) %>% 
  arrange(cipsstartdate) %>% 
  summarise(admissions = n(),
            died_in_hospital = sum(hospital_death == 1),
            mean_los = round(mean(los), 1),
            sd_los = round(sd(los), 1),
            median_los = median(los),
            median_los_n = sum(los == median_los),
            discharged_alive = sum(hospital_death == 0),
            has_readmission = sum(has_readmission == 1),
            pct_has_readmission = round(100*(has_readmission/discharged_alive), 2),) %>% 
  mutate(admissions_7dayavg = sevendayavg(admissions, rounding_digits = 1),
         died_in_hospital_7dayavg = sevendayavg(died_in_hospital , rounding_digits = 1),
         pct_died_in_hospital_7dayavg= round(100*(died_in_hospital_7dayavg/admissions_7dayavg),2),
         cipsstartdate_dummy = if_else(month(cipsstartdate) %in% c(3:12), 
                                       `year<-`(cipsstartdate, 0003), 
                                       `year<-`(cipsstartdate, 0004)))

write_csv(admission_outcome_all_roll, str_c(results_path, "outcomes/hip_admissions_all_outcomes_7dayavg.csv"))

(admission_outcome_all_roll %>% 
    ggplot(aes(x = cipsstartdate_dummy, y = pct_died_in_hospital_7dayavg, color = year_adm, group = year_adm)) +
    geom_line()+
    theme_bw() +
    expand_limits(y=0)+
    scale_x_date(date_labels = "%b", date_breaks = "2 month") +
    theme(legend.position = "top",
          legend.justification = c(1,0),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.margin = margin(c(0,0,0,0)),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
    labs(title = "Hip fracture admissions\n(7-day average)", x= "", y = "Mortality (%)")) %>%  
  ggsave(str_c(results_path, "outcomes/hip_mortality_all_monthly_7dayavg.png"), ., device = "png", 
         dpi = 600, width = 3, height = 4)


admission_outcome_residence_roll <- apcc_hip %>%
  group_by(residence, year_adm, cipsstartdate) %>% 
  arrange(cipsstartdate) %>% 
  summarise(admissions = n(),
            died_in_hospital = sum(hospital_death == 1),
            mean_los = round(mean(los), 1),
            sd_los = round(sd(los), 1),
            median_los = median(los),
            median_los_n = sum(los == median_los),
            discharged_alive = sum(hospital_death == 0),
            has_readmission = sum(has_readmission == 1),
            pct_has_readmission = round(100*(has_readmission/discharged_alive), 2),) %>% 
  mutate(admissions_7dayavg = sevendayavg(admissions, rounding_digits = 1),
         died_in_hospital_7dayavg = sevendayavg(died_in_hospital , rounding_digits = 1),
         pct_died_in_hospital_7dayavg = round(100*(died_in_hospital_7dayavg/admissions_7dayavg),2),
         cipsstartdate_dummy = if_else(month(cipsstartdate) %in% c(3:12), 
                                       `year<-`(cipsstartdate, 0003), 
                                       `year<-`(cipsstartdate, 0004)))

write_csv(admission_outcome_residence_roll, str_c(results_path, "outcomes/hip_admissions_residence_outcomes_7dayavg.csv"))

(admission_outcome_residence_roll %>% 
    ggplot(aes(x = cipsstartdate_dummy, y = pct_died_in_hospital_7dayavg, color = year_adm, group = year_adm)) +
    geom_line()+
    theme_bw() +
    expand_limits(y=0)+
    facet_wrap("residence", scale = "free_y") +
    scale_x_date(date_labels = "%b", date_breaks = "2 month") +
    theme(legend.position = "top",
          legend.justification = c(1,0),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.margin = margin(c(0,0,0,0)),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1)) +
    labs(title = "Hip fracture admissions\n(7-day average)", x= "", y = "Mortality (%)")) %>%  
  ggsave(str_c(results_path, "outcomes/hip_mortality_residence_monthly_7dayavg.png"), ., device = "png", 
         dpi = 600, width = 6, height = 4)

# Table 2 for admissions ----
vars_tosummarise <- c("hospital_death", "los")

cat_vars_tosummarise <- c("hospital_death")

table2_adm_year_residence <- CreateTableOne(vars = vars_tosummarise, 
                                            data = apcc_hip %>% 
                                              filter(!is.na(year_adm)), 
                                            strata = c("year_adm",  'residence'), 
                                            factorVars = cat_vars_tosummarise,
                                            test = FALSE, includeNA = TRUE)

table2_adm_year_residence_csv <- print(table2_adm_year_residence, 
                                       noSpaces = TRUE,
                                       showAllLevels = FALSE,
                                       formatOptions = list(big.mark = ",")) 

write.csv(table2_adm_year_residence_csv, str_c(results_path, 'outcomes/table2_adm_year_residence.csv'))

table2_2020adm_residence_covid <- CreateTableOne(vars = vars_tosummarise, 
                                            data = apcc_hip %>% 
                                              filter(!is.na(year_adm) & year_adm == "2020/21"), 
                                            strata = c("covid_any",  'residence'), 
                                            factorVars = cat_vars_tosummarise,
                                            test = FALSE, includeNA = TRUE)

table2_2020adm_residence_covid_csv <- print(table2_2020adm_residence_covid, 
                                       noSpaces = TRUE,
                                       showAllLevels = FALSE,
                                       formatOptions = list(big.mark = ",")) 

write.csv(table2_2020adm_residence_covid_csv, str_c(results_path, 'outcomes/table2_2020adm_residence_covid.csv'))
