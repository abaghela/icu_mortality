library(tidyverse)
library(magrittr)
library(functionjunction)
source("./scripts/helper_functions.R")

# Read in data 
dat <- read_csv("./raw_data/widsdatathon2020/training_v2.csv")

# Get an idea of each variable 
dat %>% head() %>% View()
dat %>% glimpse()
dat %>% 
  psych::describe()
dat %>% 
  group_by(hospital_death) %>% 
  summarize(n = n()) 
dat %>% nrow()
dat$patient_id %>% unique() %>% length()

# Univariate comparison of select variables between died/survived
var_t_test <- dat %>% 
  select_if(is.numeric) %>% 
  select(-one_of("encounter_id",  "hospital_id", "icu_id")) %>% 
  mutate(patient_id = as.character(patient_id)) %>% 
  mutate(hospital_death = ifelse(hospital_death == 1, "died", "survived")) %>% 
  pivot_longer(-c("patient_id","hospital_death"), names_to = "variable") %>% 
  mutate(hospital_death = factor(hospital_death, levels = c("survived", "died"))) %>% 
  group_by(variable) %>%
  group_modify(~ broom::tidy(lm(value ~ hospital_death, data = .x, na.action = na.omit)))

var_t_test %>% 
  filter(!term == c("(Intercept)")) %>% 
  filter(p.value <= 0.01)

# Look at variable correlations
dat %>% 
  select_if(is.numeric) %>% 
  as.matrix() %>% 
  cor(use = "pairwise.complete.obs") %>% 
  na2zero() %>% 
  pheatmap::pheatmap()

# Remove patients with missing data 
dat_filt <- dat %>% filt_sparse_rows_cols()

dat_filt %>% 
  group_by(hospital_death) %>% 
  summarize(n = n())

# Impute

# Input to ML. Lets use a Neural Network using Python code. 