## Subgroup analysis
# Exclude patients with COVID


library(tidyverse)
library(tidylog)
library(lubridate)
library(survival)
library(survminer)
library(gridExtra)
library(Publish)
library(RColorBrewer)
source("file_paths.R")
source("functions.R")

set.seed(123)
apcc_hip <- readRDS(str_c(processed_data_path, "apcc_hip.Rds"))



## Outcome variables for cause-specific hazard models
## Status Death
# 0 = censored (doesn't exist)
# 1 = discharge (competing event, censored)
# 2 = hospital death (cause of interest)

# Status Discharge
# 0 = censored (doesn't exist)
# 1 = death (competing event)
# 2 = discharge (cause of interest)
apcc_hip <-  apcc_hip %>% 
  filter(!is.na(year_adm)) %>%
  filter(covid_any == FALSE) %>% 
  mutate(status_death = if_else(hospital_death == 1, 2, 1),
         status_discharge = if_else(hospital_death == 0, 2, 1)) %>% 
  ungroup()


# Cox model -----------------------------------------------------------------------

mortality_model <- coxph(Surv(time = los, event = status_death)  ~ sex + age_band + year_adm   
                         + imd_quintile  + hip_ops 
                         + f_anxdep_h36 + f_cogimpair_h36 + f_depend_h36
                         + f_fallsfract_h36 + f_incont_h36 + f_mobprob_h36
                         + f_pulcers_h36 
                         + ec_chf_h36 + c_dementia_h36 + ec_cpd_h36 + c_rheum_h36 
                         + c_livermild_h36 + c_diabchrcomp_h36 + c_livermodsev_h36 
                         + ec_plegia_h36 + c_renaldis_h36 + c_cancer_h36 + ec_stumourmets_h36
                         + residence,  data = apcc_hip) 

mortality_model %>% 
  broom::tidy(., exponentiate = TRUE) %>%
  cbind(exp(confint(mortality_model)))  %>% 
  left_join(broom::tidy(mortality_model, exponentiate = FALSE) %>%
              cbind(confint(mortality_model)), by = "term", suffix = c("_exp", "")) %>% 
  mutate(across(c("estimate_exp", "2.5 %_exp", "97.5 %_exp"), ~round(.x, 2))) %>% 
  mutate(across(c( "estimate", "2.5 %", "97.5 %"), ~round(.x, 3))) %>% 
  select(term, estimate_exp, `2.5 %_exp`, `97.5 %_exp`,p.value , estimate, `2.5 %`, `97.5 %`) %>% 
  write_csv(str_c(results_path, 'models/mortality_coxmodel_COVIDneg_interaction_allvars.csv'))



discharge_model <- coxph(Surv(time = los, event = status_discharge)  ~ sex + age_band + year_adm   
                         + imd_quintile  + hip_ops 
                         + f_anxdep_h36 + f_cogimpair_h36 + f_depend_h36
                         + f_fallsfract_h36 + f_incont_h36 + f_mobprob_h36
                         + f_pulcers_h36 
                         + ec_chf_h36 + c_dementia_h36 + ec_cpd_h36 + c_rheum_h36 
                         + c_livermild_h36 + c_diabchrcomp_h36 + c_livermodsev_h36 
                         + ec_plegia_h36 + c_renaldis_h36 + c_cancer_h36 + ec_stumourmets_h36
                         + residence,  data = apcc_hip)

discharge_model%>% 
  broom::tidy(., exponentiate = TRUE) %>%  
  cbind(exp(confint(discharge_model))) %>% 
  left_join(broom::tidy(discharge_model, exponentiate = FALSE) %>%
              cbind(confint(discharge_model)), by = "term", suffix = c("_exp", "")) %>% 
  mutate(across(c("estimate_exp", "2.5 %_exp", "97.5 %_exp"), ~round(.x, 2))) %>% 
  mutate(across(c( "estimate", "2.5 %", "97.5 %"), ~round(.x, 3))) %>% 
  select(term, estimate_exp, `2.5 %_exp`, `97.5 %_exp`,p.value , estimate, `2.5 %`, `97.5 %`) %>% 
  write_csv(str_c(results_path, 'models/discharge_coxmodel_COVIDneg_interaction_allvars.csv'))

# combined graph
(regressionTable(mortality_model)$`year_adm` %>%
    filter(grepl("21", Units)) %>% 
    mutate(outcome = "Hospital death") %>% 
    bind_rows(regressionTable(discharge_model)$`year_adm` %>%
                filter(grepl("21", Units)) %>% 
                mutate(outcome = "Discharge")) %>% 
    mutate(outcome = factor(outcome, levels = c("Hospital death", "Discharge")),
           HR_label = if_else(Pvalue <0.05, 
                              str_c(format(round(HazardRatio, 2), nsmall = 2), " (", 
                                    format(round(Lower, 2), nsmall = 2), ", ", 
                                    format(round(Upper, 2), nsmall = 2), ")*"),
                              str_c(format(round(HazardRatio, 2), nsmall = 2), " (", 
                                    format(round(Lower, 2), nsmall = 2), ", ", 
                                    format(round(Upper, 2), nsmall = 2), ")"))) %>% 
    ggplot(aes(y = fct_rev(outcome), x = HazardRatio, label = HR_label)) +
    geom_point(color = brewer.pal(name = "RdGy", n = 11)[c(2)], shape = 15) +
    geom_linerange(aes(xmin = Lower, xmax = Upper), color = brewer.pal(name = "RdGy", n = 11)[c(2)]) +
    geom_vline(xintercept = 1, colour = "black", size = 0.25)  + 
    geom_text(aes(x = 1.7), hjust = 0, size = 3) +
    scale_x_log10(breaks = c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6), labels = scales::number_format(accuracy = .1)) +
    theme_bw() +
    coord_cartesian(xlim = c(0.6, 1.6), clip = "off") +
    xlab(bquote(HR[CS]~"with 95% CI"))+
    theme(panel.border =  element_rect(size = 0.25),
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 9), 
          axis.ticks.x = element_line(size = 0.25),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = unit(c(1,7,1,1), "lines"),
          strip.text = element_text(hjust = 0, size = 11),
          strip.background = element_blank(),
          axis.text = element_text(color = "black")))%>%  
  ggsave("graphs/Death_Discharge_HR_COVIDneg_rel_2019.png", ., device = "png", 
         dpi = 600, width = 4.2, height = 2.2)


# Schoenfeld residuals: check proportional hazards assumption -----------------------------------
# For SDC reasons, only show Schoenfeld residual fit + Confidence bands at two standard errors

cox.zph(mortality_model)

ggsave(str_c(results_path, "models/Schoenfeld_death_COVIDneg.png"), 
       arrangeGrob(grobs = ggcoxzph(cox.zph(mortality_model), 
                                    resid = FALSE, title = ""
       )),  
       dpi = 600, width = 12, height = 15)

ggsave(str_c(results_path, "models/Schoenfeld_discharge_COVIDneg.png"), 
       arrangeGrob(grobs = ggcoxzph(cox.zph(discharge_model), 
                                    resid = FALSE, title = ""
       )),  
       dpi = 600, width = 12, height = 15)



