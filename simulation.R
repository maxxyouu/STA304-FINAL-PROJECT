# causal factor of that trigger acne breakout and inflammation
# SIMULATING DATA

library(tidyverse)
library(purrr)
sample_size <- 100000
set.seed(853)

data <-
  tibble(
    unique_person_id = c(1:sample_size),
    age = runif(n = sample_size,
                min = 18,
                max = 30),
    heredity = rbernoulli(n=sample_size, 0.75),
    workout = rbernoulli(n=sample_size, 0.5),
    rice_consumption = rnorm(n=sample_size, mean = 26, sd = 4),
    bread_consumption = rnorm(n=sample_size, mean = 37, sd = 4)
  )
set.seed(853)

# people who workout regularly most likely have intake of protein shake to gain muscle mass
data <- data %>% mutate(protein_shake_intake = if_else(workout == 1, rbernoulli(1, 0.7), rbernoulli(1, 0.2)),
                        college_students = if_else(age <= 25, rbernoulli(1, 0.70), rbernoulli(1, 0.2))) %>%
  dplyr::select(-workout)

# assign the treatment
data_cp <- data %>% mutate(
  age_num = case_when(
    age < 25 ~ 2,
    age < 30 ~ 1,
    TRUE ~ 0),
  heredity_num = case_when(
    heredity == TRUE ~ 2,
    TRUE ~ 0),
  rice_num = case_when(
    rice_consumption >= 30 ~ 2,
    rice_consumption >= 28 ~ 1,
    rice_consumption >= 26 ~ 0,
    TRUE ~ 0),
  bread_num = case_when(
    bread_consumption >= 42 ~ 2,
    bread_consumption >= 40 ~ 1,
    bread_consumption >= 37 ~ 0,
    TRUE ~ 0),
  protein_num = case_when(
    protein_shake_intake == TRUE ~ 2,
    TRUE ~ 0),
  college_num = case_when(
    college_students == TRUE ~ 2,
    TRUE ~ 0)
) %>% 
  rowwise() %>%
  mutate(sum_num = sum(age_num, heredity_num, rice_num, bread_num, protein_num, college_num),
         softmax_prob = exp(sum_num)/exp(12),
         high_hormone_level = sample(
           x = c(0:1),
           size = 1,
           replace = TRUE,
           prob = c(1-softmax_prob, softmax_prob)
         )
  ) %>% 
  ungroup()

# remove the unnecessary columns
data_cp <- data_cp %>% dplyr::select(-age_num, -heredity_num, -rice_num, -bread_num, -protein_num, -college_num, -sum_num)

# assigned the outcome
data_cp <- # data_cp
  data_cp %>% 
  mutate(acne_breakout_prob = if_else(high_hormone_level == 1, 0.8, 0.2)) %>%
  rowwise() %>% 
  mutate(acne_breakout_bool = rbernoulli(n=1, acne_breakout_prob) # sample the acne breakout outcome
  ) %>% 
  ungroup() %>% 
  dplyr::select(-acne_breakout_prob) %>%
  mutate(acne_breakout = case_when( acne_breakout_bool == TRUE ~ 1, TRUE ~ 0),
         heredity_c = case_when( heredity == TRUE ~ 1, TRUE ~ 0),
         protein_shake_intake_c = case_when( protein_shake_intake == TRUE ~ 1, TRUE ~ 0),
         college_students_c = case_when( college_students == TRUE ~ 1, TRUE ~ 0)) %>%
  mutate_at(vars(heredity_c, protein_shake_intake_c, college_students_c, high_hormone_level), ~as.factor(.)) %>%
  dplyr::select(-heredity, -protein_shake_intake, -college_students, -unique_person_id, -softmax_prob, -acne_breakout_bool)
