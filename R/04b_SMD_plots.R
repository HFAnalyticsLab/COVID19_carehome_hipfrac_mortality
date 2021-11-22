## Plot standardised mean variances for study population

library(tidyverse)
library(tidylog)
library(RColorBrewer)
library(readxl)

# Plots created after release of results

label_lookup <- read_excel("smd_lookup.xlsx", trim_ws = FALSE)

# Admissions --------------------------------------------------------------

admissions_chars_residence_smd <- read_csv("releases/2021_10_20/characteristics/hipadmissions_chars_year_residence_SMD.csv")


admissions_chars_residence_smd <- admissions_chars_residence_smd %>% 
  full_join(label_lookup[,c("Characteristic", "Label")], by = c("variable" = "Characteristic")) %>% 
  filter(!is.na(Label)) %>%
  mutate(Label = factor(Label, levels = label_lookup$Label)) %>% 
  select(-X1, - variable) %>% 
  pivot_longer(-Label, names_to = "comparison", values_to = "SMD")  %>% 
  mutate(comparison = factor(comparison, levels = c("Community: 2019/20 vs 2020/21",
                                                    "Residential: 2019/20 vs 2020/21",
                                                    "Nursing: 2019/20 vs 2020/21")))


(admissions_chars_residence_smd %>% 
    ggplot(aes(x = fct_rev(Label), y = SMD*100, group = comparison, fill = comparison)) +
    geom_point(shape = 21, stroke = 0.25, size = 2) +
    coord_flip() +
    ylab("Absolute standardised mean difference (%)") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0),
          legend.margin = margin(0,0,-5,0)) +
    scale_fill_manual(values = brewer.pal(name = "RdGy", n = 11)[c(1,3,5)],
                       guide = guide_legend(ncol = 1)) +
    geom_hline(yintercept = 10, linetype = "dashed", alpha = 0.3)) %>% 
  ggsave('graphs/hipadmissions_chars_year_residence_SMD.png', ., device = "png", 
         dpi = 600, width = 4.5, height = 5)


# Admissions 2020 split by covid  --------------------------------------------------------------

admissions_chars_residence_covid_SMD <- read_csv("releases/2021_10_20//characteristics/hipadmissions_chars_residence_covid_SMD.csv")


admissions_chars_residence_covid_SMD <- admissions_chars_residence_covid_SMD %>% 
  full_join(label_lookup[,c("Characteristic", "Label")], by = c("variable" = "Characteristic")) %>% 
  filter(!is.na(Label)) %>%
  mutate(Label = factor(Label, levels = label_lookup$Label)) %>% 
  select(-X1, - variable) %>% 
  pivot_longer(-Label, names_to = "comparison", values_to = "SMD") %>% 
  filter(Label != "COVID-19") 

(admissions_chars_residence_covid_SMD %>% 
    ggplot(aes(x = fct_rev(Label), y = SMD*100, group = comparison, color = comparison)) +
    geom_point() +
    coord_flip() +
    ylab("Absolute standardised mean difference (%)") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          legend.justification = c(1,0)) +
    scale_color_manual(values = brewer.pal(name ="Set1", n = 9),
                       guide = guide_legend(ncol = 1)) +
    geom_hline(yintercept = 10, linetype = "dashed", alpha = 0.3)) %>% 
  ggsave('graphs/hipadmissions_chars_2020_residence_covid_SMD.png', ., device = "png", 
         dpi = 600, width = 6, height = 8)

