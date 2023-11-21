# load useful packages
library(tidyverse)
library(tidymodels)
library(vip)
library(RColorBrewer) 


# Set protocols and themes
theme_set(theme_minimal(base_size = 12))
tidymodels_prefer()
doParallel::registerDoParallel()

# Load the data
salary_potential <- read_csv("data/salary_potential.csv")

tuition_cost <- read_csv("data/tuition_cost.csv")

tuition_income <- read_csv("data/tuition_income.csv")

diversity_school <- read_csv("data/diversity_school.csv")


glimpse(salary_potential)

glimpse(tuition_cost)

glimpse(tuition_income)

glimpse (diversity_school)


salary_potential <- salary_potential %>%
  mutate(log_early_career_pay = log(early_career_pay),
         log_mid_career_pay = log(mid_career_pay))

# Model for pay ~ stem_percent & pay ~ school
salary_potential_split <- initial_split(salary_potential, strata = state_name)
salary_potential_train <- training(salary_potential_split)
salary_potential_test <- testing(salary_potential_split)


# Early Pay by stem_percent
# ---------
early_pay_intervals <- reg_intervals(log_early_career_pay ~ stem_percent + state_name, data= salary_potential_train,
                                  keep_reps = TRUE)

p_early_coeffs_by_stem <- early_pay_intervals %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor")

p_early_coeffs_by_stem


# Mid Pay by stem_percent
# -------
mid_pay_intervals <- reg_intervals(log_mid_career_pay ~ stem_percent + state_name, data= salary_potential_train,
                               keep_reps = TRUE)

p_mid_coeffs_by_stem <- mid_pay_intervals %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor")

p_mid_coeffs_by_stem


# # Early Pay by college
# # ---------
# early_pay_intervals_by_college <- reg_intervals(log_early_career_pay ~ name, data= salary_potential_train,
#                                      keep_reps = TRUE)
# 
# p_early_coeffs_by_college <- early_pay_intervals_by_college %>%
#   ggplot(aes(x=.estimate,y=term)) +
#   geom_point(size=2) +
#   geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) +
#   geom_vline(xintercept = 0.0,linetype = "dashed") +
#   labs(x="Coefficient Estimate",y="Predictor")
# 
# p_early_coeffs_by_college
# 
# 
# # Mid Pay by college
# # -------
# mid_pay_intervals_by_college <- reg_intervals(log_mid_career_pay ~ name + state_name, data= salary_potential_train,
#                                    keep_reps = TRUE)
# 
# p_mid_coeffs_by_college <- mid_pay_intervals_by_college %>%
#   ggplot(aes(x=.estimate,y=term)) +
#   geom_point(size=2) +
#   geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) +
#   geom_vline(xintercept = 0.0,linetype = "dashed") +
#   labs(x="Coefficient Estimate",y="Predictor")
# 
# p_mid_coeffs_by_college





# Model for pay ~ category (race) + total enrollment

# Join salary_potential and diversity_school data-sets
salary_diversity <- merge(salary_potential, diversity_school, by = "name", all = FALSE)

glimpse(salary_diversity)

salary_diversity_split <- initial_split(salary_diversity, strata = state_name)
salary_diversity_train <- training(salary_diversity_split)
salary_diversity_test <- testing(salary_diversity_split)


# Early Pay
# ---------
early_pay_intervals_by_category <- reg_intervals(log_early_career_pay ~ category + total_enrollment +state_name, 
                                                 data= salary_diversity_train,
                                                 keep_reps = TRUE)

p_early_coeffs_by_category <- early_pay_intervals_by_category %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor")

p_early_coeffs_by_category



# Mid Pay
# -------
mid_pay_intervals_by_category <- reg_intervals(log_mid_career_pay ~ category + total_enrollment + state_name, 
                                                 data= salary_diversity_train,
                                                 keep_reps = TRUE)

p_mid_coeffs_by_category <- mid_pay_intervals_by_category %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor")

p_mid_coeffs_by_category




# Model for pay ~ net_cost + total_price + income_lvl

# Join salary_potential and tuition_income data-sets
salary_income <- merge(salary_potential, tuition_income, by = "name", all = FALSE)

glimpse(salary_income)

salary_income_split <- initial_split(salary_income, strata = state_name)
salary_income_train <- training(salary_income_split)
salary_income_test <- testing(salary_income_split)


# Early Pay by net_cost and total_price
# ---------
early_pay_intervals_by_cost <- reg_intervals(log_early_career_pay ~ net_cost + total_price, 
                                             data= salary_income_train,
                                             keep_reps = TRUE)

p_early_coeffs_by_cost <- early_pay_intervals_by_cost %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor")

p_early_coeffs_by_cost


# Early Pay by income bracket
# ---------
early_pay_intervals_by_bracket <- reg_intervals(log_early_career_pay ~ income_lvl,
                                             data= salary_income_train,
                                             keep_reps = TRUE)

p_early_coeffs_by_bracket <- early_pay_intervals_by_bracket %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor")

p_early_coeffs_by_bracket



# Early Pay by both
# ---------
early_pay_intervals_by_both <- reg_intervals(log_early_career_pay ~ net_cost + total_price + income_lvl,
                                                data= salary_income_train,
                                                keep_reps = TRUE)

p_early_coeffs_by_both <- early_pay_intervals_by_both %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor")

p_early_coeffs_by_both




# Model for pay ~ type

# Join salary_potential and tuition_cost data-sets
salary_cost <- merge(salary_potential, tuition_cost, by = "name", all = FALSE)

glimpse(salary_cost)

salary_cost_split <- initial_split(salary_cost, strata = state_name)
salary_cost_train <- training(salary_cost_split)
salary_cost_test <- testing(salary_cost_split)


# Early Pay by type
# ---------
early_pay_intervals_by_type <- reg_intervals(log_early_career_pay ~ type, data= salary_cost_train, keep_reps = TRUE)

p_early_coeffs_by_type <- early_pay_intervals_by_type %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor")

p_early_coeffs_by_type





# Graphs 

p_early_coeffs_by_stem
p_mid_coeffs_by_stem

p_early_coeffs_by_college

p_early_coeffs_by_category
p_mid_coeffs_by_category

p_early_coeffs_by_enrollment
p_mid_coeffs_by_enrollment

p_early_coeffs_by_cost
p_early_coeffs_by_bracket
p_early_coeffs_by_both

p_early_coeffs_by_type

