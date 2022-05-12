### 
# Regression analysis
# 1. Lasso
# 2. Fit null model and full model
# 2. Check functional form of variables
# 4. Model fit

###

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(survival)
library(survminer)
library(gridExtra)
library(Publish)
library(RColorBrewer)
source("file_paths.R")
source("functions.R")

apcc_hip <- readRDS(str_c(processed_data_path, "apcc_hip.Rds"))

# Outcomes variable -----------------------------------------------------------


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
  mutate(status_death = if_else(hospital_death == 1, 2, 1),
         status_discharge = if_else(hospital_death == 0, 2, 1),
         sex = factor(sex)) %>% 
  ungroup()


# Checking interactions ------------------------------------------------------------

## Outcome: Mortality
# COVID * age
apcc_hip %>% 
  group_by(age_band, covid_any) %>% 
  summarise(admissions = n(),
            died_in_hospital = sum(hospital_death == 1),
            pct_died_in_hospital = round(100*(died_in_hospital/admissions),2)) %>% 
  ggplot(aes(x = factor(covid_any), y = pct_died_in_hospital, group = age_band, color = age_band)) +
  geom_line() +
  geom_point()


# COVID * sex
apcc_hip %>% 
  group_by(sex, covid_any) %>% 
  summarise(admissions = n(),
            died_in_hospital = sum(hospital_death == 1),
            pct_died_in_hospital = round(100*(died_in_hospital/admissions),2)) %>% 
  ggplot(aes(x = factor(covid_any), y = pct_died_in_hospital, group = factor(sex), color = factor(sex))) +
  geom_line() +
  geom_point()

# COVID * charlson
apcc_hip %>% 
  group_by(i_charlson_h36_5cat, covid_any) %>% 
  summarise(admissions = n(),
            died_in_hospital = sum(hospital_death == 1),
            pct_died_in_hospital = round(100*(died_in_hospital/admissions),2)) %>% 
  ggplot(aes(x = factor(covid_any), y = pct_died_in_hospital, group = i_charlson_h36_5cat, color = i_charlson_h36_5cat)) +
  geom_line() +
  geom_point()

# Model --------------------------------------------------------------

mortality_model <- coxph(Surv(time = los, event = status_death)  ~ sex + age_band + year_adm   
                         + covid_any + hip_ops 
                         + f_anxdep_h36 + f_cogimpair_h36 + f_depend_h36
                         + f_fallsfract_h36 + f_incont_h36 + f_mobprob_h36
                         + f_pulcers_h36 
                         + ec_chf_h36 + c_dementia_h36 + ec_cpd_h36 + c_rheum_h36 
                         + c_livermild_h36 + c_diabchrcomp_h36 + c_livermodsev_h36 
                         + ec_plegia_h36 + c_renaldis_h36 + c_cancer_h36 + ec_stumourmets_h36
                         + covid_any*age_band + covid_any*sex + residence
                         + year_adm*residence,  data = apcc_hip) 


# is the interaction term admission year * residence improving the model?
mortality_model_noint_year_res <- coxph(Surv(time = los, event = status_death)  ~ sex + age_band + year_adm   
                                        + covid_any + hip_ops 
                                        + f_anxdep_h36 + f_cogimpair_h36 + f_depend_h36
                                        + f_fallsfract_h36 + f_incont_h36 + f_mobprob_h36
                                        + f_pulcers_h36 
                                        + ec_chf_h36 + c_dementia_h36 + ec_cpd_h36 + c_rheum_h36 
                                        + c_livermild_h36 + c_diabchrcomp_h36 + c_livermodsev_h36 
                                        + ec_plegia_h36 + c_renaldis_h36 + c_cancer_h36 + ec_stumourmets_h36
                                        + covid_any*age_band + covid_any*sex + residence,
                                        data = apcc_hip) 

anova(mortality_model_noint_year_res, mortality_model) %>% 
  write_csv(str_c(results_path, 'models/mortality_anova.csv'))
# no improvement, therefore remove

# also removed interactions as they had no effect on the result

mortality_model <- coxph(Surv(time = los, event = status_death)  ~ sex + age_band + year_adm   
                         + covid_any + hip_ops 
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
  write_csv(str_c(results_path, 'models/mortality_coxmodel_allvars.csv'))


(regressionTable(mortality_model)$`year_adm` %>%
    filter(grepl("21", Units)) %>% 
    mutate(year_adm = "2020/21") %>%
    ggplot(aes(y = year_adm, x = HazardRatio)) +
    geom_point(color = brewer.pal(name = "RdGy", n = 11)[c(2)]) +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, color = brewer.pal(name = "RdGy", n = 11)[c(2)]) +
    geom_vline(xintercept = 1, colour = "black", linetype = "longdash", size = 0.25)  + 
    scale_x_log10(labels = scales::number_format(accuracy = 0.1)) +
    theme_bw() +
    ggtitle("Hospital death") +
    xlab(bquote(HR[CS]~" with 95% CI"))+
    theme(legend.title =  element_blank(),
          legend.justification = c(1,0),
          legend.position = "top",
          axis.title.y = element_blank(), 
          axis.line.y = element_blank(), 
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(size = 0.25),
          plot.title = element_text(margin = margin(0,0,5,0), size = 16),
          plot.title.position = "plot"))%>%  ggsave("graphs/Death_HR_rel_2019.png", ., device = "png", 
                                                    dpi = 600, width = 4, height = 4.2)

regressionTable(mortality_model)$`year_adm` %>%
  filter(grepl("21", Units)) %>% 
  mutate(year_adm = "2020/21") %>% 
  write_csv(str_c(results_path, "models/Death_HR_rel_2019.csv"))


discharge_model <- coxph(Surv(time = los, event = status_discharge)  ~ sex + age_band + year_adm   
                         + covid_any + hip_ops 
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
  write_csv(str_c(results_path, 'models/discharge_coxmodel_allvars.csv'))


(regressionTable(discharge_model)$`year_adm` %>%
    filter(grepl("21", Units)) %>% 
    separate(Variable, into = c("residence", "temp"), sep = ":") %>% 
    mutate( year_adm = "2020/21") %>% 
    ggplot(aes(y = Variable, x = HazardRatio)) +
    geom_point(color = brewer.pal(name = "RdGy", n = 11)[c(2)]) +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, color = brewer.pal(name = "RdGy", n = 11)[c(2)]) +
    geom_vline(xintercept = 1, colour = "black", linetype = "longdash", size = 0.25)  + 
    scale_x_log10(labels = scales::number_format(accuracy = 0.1)) +
    theme_bw() +
    ggtitle("Live discharge") +
    xlab(bquote(HR[CS]~" with 95% CI"))+
    theme(legend.title =  element_blank(),
          legend.justification = c(1,0),
          legend.position = "top",
          axis.title.y = element_blank(), 
          axis.line.y = element_blank(), 
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(size = 0.25),
          plot.title = element_text(margin = margin(0,0,5,0), size = 16),
          plot.title.position = "plot"))%>%  
  ggsave("graphs/Discharge_HR_rel_2019.png", ., device = "png", 
         dpi = 600, width = 4, height = 4.2)

regressionTable(discharge_model)$`year_adm` %>%
  filter(grepl("21", Units)) %>% 
  mutate(year_adm = "2020/21") %>% 
  write_csv(str_c(results_path, "models/Discharge_HR_rel_2019.csv"))

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
    scale_x_log10(breaks = c(0.8, 1.0, 1.2, 1.4, 1.6), labels = scales::number_format(accuracy = .1)) +
    theme_bw() +
    coord_cartesian(xlim = c(0.8, 1.6), clip = "off") +
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
  ggsave("graphs/Death_Discharge_HR_rel_2019.png", ., device = "png", 
         dpi = 600, width = 4.2, height = 2.2)

# Unadjusted models for comparison ----
mortality_model_uni <- coxph(Surv(time = los, event = status_death)  ~ year_adm + residence 
                             + year_adm*residence,  data = apcc_hip) 


mortality_model_uni %>% 
  broom::tidy(., exponentiate = TRUE) %>%
  cbind(exp(confint(mortality_model_uni)))  %>% 
  mutate(across(c("estimate", "2.5 %", "97.5 %"), ~round(.x, 2))) %>% 
  select(term, estimate, p.value, `2.5 %`, `97.5 %`) %>% 
  write_csv(str_c(results_path, 'models/mortality_coxmodel_univariate.csv'))

discharge_model_uni <- coxph(Surv(time = los, event = status_discharge)  ~  year_adm + residence 
                             + year_adm*residence,  data = apcc_hip)

discharge_model_uni%>% 
  broom::tidy(., exponentiate = TRUE) %>%  
  cbind(exp(confint(discharge_model_uni)))  %>% 
  mutate(across(c("estimate", "2.5 %", "97.5 %"), ~round(.x, 2))) %>% 
  select(term, estimate, p.value, `2.5 %`, `97.5 %`) %>% 
  write_csv(str_c(results_path, 'models/discharge_coxmodel_univariate.csv'))

# Influential observations ------------------------------------------------------

# detect outliers
ggcoxdiagnostics(mortality_model, type = "deviance", ggtheme = theme_bw())


ggsave(str_c(results_path, "models/Deviance_lasso_death.png"), 
       ggcoxdiagnostics(mortality_model, type = "deviance", 
                        ox.scale = "observation.id",
                        ggtheme = theme_bw(), sline.se = FALSE),  
       dpi = 600, width = 5, height = 5)

# Pattern symmetric around 0?
ggcoxdiagnostics(mortality_model, type = "dfbeta", ggtheme = theme_bw())

ggsave(str_c(results_path, "models/Dfbeta_lasso_death.png"), 
       ggcoxdiagnostics(mortality_model, type = "dfbeta", 
                        ox.scale = "observation.id",
                        ggtheme = theme_bw(), sline.se = FALSE),  
       dpi = 600, width = 12, height = 15)

# check functional form of variables --------------------------------------
# martingale residuals (linearity of predictor with log hazard)

ggcoxfunctional(Surv(time = los, event = status_discharge)  ~ mmpi_age, data = apcc_hip)
# makes sense to bin up to 95



# Schoenfeld residuals: check proportional hazards assumption -----------------------------------
# For SDC reasons, only show Schoenfeld residual fit + Confidence bands at two standard errors

cox.zph(mortality_model)

ggsave(str_c(results_path, "models/Schoenfeld_death.png"), 
       arrangeGrob(grobs = ggcoxzph(cox.zph(mortality_model), 
                                    resid = FALSE, title = ""
       )),  
       dpi = 600, width = 12, height = 15)

ggsave(str_c(results_path, "models/Schoenfeld_discharge.png"), 
       arrangeGrob(grobs = ggcoxzph(cox.zph(discharge_model), 
                                    resid = FALSE, title = ""
       )),  
       dpi = 600, width = 12, height = 15)





# Interaction models, for supplements -------------------------------------

mortality_model_int <- coxph(Surv(time = los, event = status_death)  ~ sex + age_band + year_adm   
                         + covid_any + hip_ops 
                         + f_anxdep_h36 + f_cogimpair_h36 + f_depend_h36
                         + f_fallsfract_h36 + f_incont_h36 + f_mobprob_h36
                         + f_pulcers_h36 
                         + ec_chf_h36 + c_dementia_h36 + ec_cpd_h36 + c_rheum_h36 
                         + c_livermild_h36 + c_diabchrcomp_h36 + c_livermodsev_h36 
                         + ec_plegia_h36 + c_renaldis_h36 + c_cancer_h36 + ec_stumourmets_h36
                         + residence + residence*year_adm,  data = apcc_hip) 


mortality_model_int %>% 
  broom::tidy(., exponentiate = TRUE) %>%
  cbind(exp(confint(mortality_model_int)))  %>% 
  left_join(broom::tidy(mortality_model_int, exponentiate = FALSE) %>%
              cbind(confint(mortality_model_int)), by = "term", suffix = c("_exp", "")) %>% 
  mutate(across(c("estimate_exp", "2.5 %_exp", "97.5 %_exp"), ~round(.x, 2))) %>% 
  mutate(across(c( "estimate", "2.5 %", "97.5 %"), ~round(.x, 3))) %>% 
  select(term, estimate_exp, `2.5 %_exp`, `97.5 %_exp`,p.value , estimate, `2.5 %`, `97.5 %`) %>% 
  write_csv(str_c(results_path, 'models/mortality_coxmodel_interaction_allvars.csv'))

discharge_model_int <- coxph(Surv(time = los, event = status_discharge)  ~ sex + age_band + year_adm   
                         + covid_any + hip_ops 
                         + f_anxdep_h36 + f_cogimpair_h36 + f_depend_h36
                         + f_fallsfract_h36 + f_incont_h36 + f_mobprob_h36
                         + f_pulcers_h36 
                         + ec_chf_h36 + c_dementia_h36 + ec_cpd_h36 + c_rheum_h36 
                         + c_livermild_h36 + c_diabchrcomp_h36 + c_livermodsev_h36 
                         + ec_plegia_h36 + c_renaldis_h36 + c_cancer_h36 + ec_stumourmets_h36
                         + residence + residence*year_adm,  data = apcc_hip)

discharge_model_int %>% 
  broom::tidy(., exponentiate = TRUE) %>%  
  cbind(exp(confint(discharge_model_int))) %>% 
  left_join(broom::tidy(discharge_model_int, exponentiate = FALSE) %>%
              cbind(confint(discharge_model_int)), by = "term", suffix = c("_exp", "")) %>% 
  mutate(across(c("estimate_exp", "2.5 %_exp", "97.5 %_exp"), ~round(.x, 2))) %>% 
  mutate(across(c( "estimate", "2.5 %", "97.5 %"), ~round(.x, 3))) %>% 
  select(term, estimate_exp, `2.5 %_exp`, `97.5 %_exp`,p.value , estimate, `2.5 %`, `97.5 %`) %>% 
  write_csv(str_c(results_path, 'models/discharge_coxmodel_interaction_allvars.csv'))

