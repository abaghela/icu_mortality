library(tidyverse)
library(magrittr)
library(functionjunction)
source("./scripts/helper_functions.R")

### Read in data 
dat <- read_csv("./raw_data/widsdatathon2020/training_v2.csv")
dict <- read_csv("./raw_data/widsdatathon2020/WiDS Datathon 2020 Dictionary.csv")
  
# Edit primary outcome and identifiers
dat %<>% 
  mutate(patient_id = as.character(patient_id)) %>% 
  mutate(hospital_death = ifelse(hospital_death == 1, "died", "survived")) %>% 
  mutate(hospital_death = factor(hospital_death, levels = c("survived", "died")))

### Remove sparse columns


### Look at important variables 
# Univariate comparison of select variables between died/survived
var_t_test <-  dat %>% {
  bind_cols(
    select(., one_of("patient_id", "hospital_death")),
    select_if(., is.numeric)
    )} %>% 
  select(-one_of("encounter_id",  "hospital_id", "icu_id")) %>% 
  pivot_longer(-c("patient_id","hospital_death"), names_to = "variable") %>% 
  group_by(variable) %>%
  group_modify(~ broom::tidy(lm(value ~ hospital_death, data = .x, na.action = na.omit)))

# Look at variable correlations
# dat %>%
#   select_if(is.numeric) %>%
#   as.matrix() %>%
#   cor(use = "pairwise.complete.obs") %>%
#   na2zero() %>%
#   pheatmap::pheatmap()

# Plot some key H1 (parameter measured within first hour of ICU stay)
vars_to_plot <- c("h1_lactate_max", "h1_creatinine_max", "h1_wbc_max", "h1_glucose_max", "h1_pao2fio2ratio_max")
dat %>% 
  dplyr::select(one_of("patient_id", "hospital_death", vars_to_plot )) %>% 
  pivot_longer(cols = vars_to_plot) %>% 
  ggplot(aes_string(x = "hospital_death", y = "value", color = "hospital_death")) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(vars(name), nrow = 1,scales = "free") + 
  theme(legend.title = element_blank()) +
  scale_color_brewer(palette="Dark2") + 
  xlab("") + ylab("")

var_t_test %>% 
  filter(!term == c("(Intercept)")) %>% 
  filter(p.value <= 0.01) %>% View()
dict %>% View()
