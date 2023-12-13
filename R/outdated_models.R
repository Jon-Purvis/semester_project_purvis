# load useful packages
library(tidyverse)
library(tidymodels)
library(parttree)
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


glimpse(salary_potential)

glimpse(tuition_cost)

glimpse(tuition_income)


salary_potential <- salary_potential %>%
  mutate(log_early_career_pay = log(early_career_pay),
         log_mid_career_pay = log(mid_career_pay))

# Model for pay ~ stem_percent
salary_potential_split <- initial_split(salary_potential, strata = state_name)
salary_potential_train <- training(salary_potential_split)
salary_potential_test <- testing(salary_potential_split)

# Linear Regression

# ---------
# Early Pay
# ---------
library(tidymodels)
library(broom)  # Make sure to load broom for tidy()

lin_reg_spec <- linear_reg()

lin_reg_fit <- fit(lin_reg_spec, log_early_career_pay ~ stem_percent + state_name, data = salary_potential_train)

# Print the model fit
print(lin_reg_fit)

# Extract and print the tidy coefficients
tidy_coeffs <- tidy(lin_reg_fit)
print(tidy_coeffs)

# Plot the coefficients
p_attend_coeffs <- tidy_coeffs %>%
  ggplot(aes(x = estimate, y = term)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin = conf.int[,"lower"], xmax = conf.int[,"upper"]), width = 0.1) + 
  geom_vline(xintercept = 0.0, linetype = "dashed") +
  labs(x = "Coefficient Estimate", y = "Predictor")

print(p_attend_coeffs)




# KNN

# ---------
# Early Pay 
# ---------
early_pay_rec <- recipe(early_career_pay ~ stem_percent + state_name, data = salary_potential_train ) %>%
  step_normalize(all_numeric_predictors())

early_pay_spec <- nearest_neighbor(neighbors = 5) %>%
  set_engine("kknn") %>%
  set_mode("regression")

early_pay_wf <- workflow() %>%
  add_recipe(early_pay_rec) %>%
  add_model(early_pay_spec)

early_pay_fit <- fit(early_pay_wf, salary_potential_train)

early_pay_fit %>%
  predict(salary_potential_test) %>%
  cbind(salary_potential_test) %>%
  rmse(early_career_pay, .pred)


early_pay_fit %>%
  predict(salary_potential_test) %>%
  cbind(salary_potential_test) %>%
  ggplot(aes(x=early_career_pay, y = .pred)) +
  geom_point(color="darkblue") + 
  geom_abline(linetype="dashed") + 
  labs(x = "Actual early pay", y = "Predicted early pay")


# -------
# Mid Pay
# -------
mid_pay_rec <- recipe(mid_career_pay ~ stem_percent + state_name, data = salary_potential_train ) %>%
  step_normalize(all_numeric_predictors())

mid_pay_spec <- nearest_neighbor(neighbors = 5) %>%
  set_engine("kknn") %>%
  set_mode("regression")

mid_pay_wf <- workflow() %>%
  add_recipe(mid_pay_rec) %>%
  add_model(mid_pay_spec)

mid_pay_fit <- fit(mid_pay_wf, salary_potential_train)

mid_pay_fit %>%
  predict(salary_potential_test) %>%
  cbind(salary_potential_test) %>%
  rmse(mid_career_pay, .pred)

mid_pay_fit %>%
  predict(salary_potential_test) %>%
  cbind(salary_potential_test) %>%
  ggplot(aes(x=mid_career_pay, y = .pred)) +
  geom_point(color="darkblue") + 
  geom_abline(linetype="dashed") + 
  labs(x = "Actual mid pay", y = "Predicted mid pay")



# Model for pay ~ net_cost

salary_and_income_df <- merge(salary_potential, tuition_income, by='name', all=FALSE)

glimpse(salary_and_income_df)

salary_and_income_df_split <- initial_split(salary_and_income_df, strata = state_name)
salary_and_income_df_train <- training(salary_and_income_df_split)
salary_and_income_df_test <- testing(salary_and_income_df_split)


# Early Pay (Linear Regression)

lin_reg_spec2 <- linear_reg()

lin_reg_fit2 <- fit(lin_reg_spec, log_early_career_pay ~ stem_percent + state_name, data = salary_potential_train)

lin_reg_fit

tidy(lin_reg_fit)


# ---------
# Early Pay (KNN)
# ---------
early_pay_rec2 <- recipe(early_career_pay ~ net_cost + state_name, data = salary_and_income_df_train ) %>%
  step_normalize(all_numeric_predictors())

early_pay_spec2 <- nearest_neighbor(neighbors = 5) %>%
  set_engine("kknn") %>%
  set_mode("regression")

early_pay_wf2 <- workflow() %>%
  add_recipe(early_pay_rec2) %>%
  add_model(early_pay_spec2)

early_pay_fit2 <- fit(early_pay_wf2, salary_and_income_df_train)

early_pay_fit2 %>%
  predict(salary_and_income_df_test) %>%
  cbind(salary_and_income_df_test) %>%
  rmse(early_career_pay, .pred)


early_pay_fit2 %>%
  predict(salary_and_income_df_test) %>%
  cbind(salary_and_income_df_test) %>%
  ggplot(aes(x=early_career_pay, y = .pred)) +
  geom_point(color="darkblue") + 
  geom_abline(linetype="dashed") + 
  labs(x = "Actual early pay", y = "Predicted early pay")


# -------
# Mid Pay
# -------
mid_pay_rec2 <- recipe(mid_career_pay ~ net_cost + state_name, data = salary_and_income_df_train ) %>%
  step_normalize(all_numeric_predictors())

mid_pay_spec2 <- nearest_neighbor(neighbors = 5) %>%
  set_engine("kknn") %>%
  set_mode("regression")

mid_pay_wf2 <- workflow() %>%
  add_recipe(mid_pay_rec2) %>%
  add_model(mid_pay_spec2)

mid_pay_fit2 <- fit(mid_pay_wf2, salary_and_income_df_train)

mid_pay_fit2 %>%
  predict(salary_and_income_df_test) %>%
  cbind(salary_and_income_df_test) %>%
  rmse(mid_career_pay, .pred)

mid_pay_fit2 %>%
  predict(salary_and_income_df_test) %>%
  cbind(salary_and_income_df_test) %>%
  ggplot(aes(x=mid_career_pay, y = .pred)) +
  geom_point(color="darkblue") + 
  geom_abline(linetype="dashed") + 
  labs(x = "Actual mid pay", y = "Predicted mid pay")

