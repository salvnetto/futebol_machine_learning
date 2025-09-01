library(tidymodels)
tidymodels_prefer() #resolve conflicts

set.seed(123)

rand_forest(trees = 1000, min_n = 5) |>
  set_engine("ranger", verbose = TRUE) |>
  set_mode("regression") |>
  translate()



data(ames)
ames <- ames %>%
  mutate(Sale_Price = log10(Sale_Price))

ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)


lm_model <- linear_reg() |>
  set_engine("lm")

lm_form_fit <- lm_model |> # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)
lm_form_fit %>% extract_fit_engine()
lm_form_fit %>% extract_fit_engine() |> vcov()
lm_form_fit %>% extract_fit_engine() |> summary()

tidy(lm_form_fit)

ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)

ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(lm_form_fit, ames_test_small)) %>%
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))


## WORKFLOWS

lm_model <-
  linear_reg() |>
  set_engine("lm")

lm_wflow <-
  workflow() |>
  add_model(lm_model) |>
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_fit <-
  fit(lm_wflow, ames_train)


predict(lm_fit, ames_test %>% slice(1:3))

lm_fit %>% update_formula(Sale_Price ~ Longitude)


lm_wflow <- lm_wflow %>%
  remove_formula() %>%
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

predictors = c(ends_with("tude"))
predictors = everything()

fit(lm_wflow, ames_train)


library(multilevelmod)
multilevel_spec <-
  linear_reg() %>%
  set_engine("lmer")

multilevel_workflow <- workflow() %>%
  # Pass the data along as-is:
  add_variables(outcome = distance, predictors = c(Sex, age, Subject)) %>%
  add_model(multilevel_spec,
            # This formula is given to the model
            formula = distance ~ Sex + (age | Subject))

multilevel_fit <- fit(multilevel_workflow, data = Orthodont)
multilevel_fit


library(censored)
parametric_spec <-
  survival_reg()

parametric_workflow <-
  workflow() %>%
  add_variables(outcome = c(fustat, futime), predictors = c(age, rx)) %>%
  add_model(parametric_spec, formula = Surv(futime, fustat) ~ age + strata(rx))
parametric_fit <- fit(parametric_workflow, data = ovarian)
parametric_fit



location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
  )

library(workflowsets)
location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models
location_models$info[[1]]
extract_workflow(location_models, id = "coords_lm")

location_models <-
  location_models %>%
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models
location_models$fit[[1]]



final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res
fitted_lm_wflow <- extract_workflow(final_lm_res)
fitted_lm_wflow

collect_metrics(final_lm_res)
collect_predictions(final_lm_res) %>% slice(1:5)


## RECIPES
simple_ames <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_dummy(all_nominal_predictors())  #all_numeric_predictors(), all_numeric(), all_predictors(), and all_outcomes()
simple_ames


lm_wflow %>% add_recipe(simple_ames)
lm_wflow <- lm_wflow %>% remove_variables() %>% add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
predict(lm_fit, ames_test %>% slice(1:3))

lm_fit %>% extract_recipe(estimated = TRUE)

lm_fit %>%
  # This returns the parsnip object:
  extract_fit_parsnip() %>%
  # Now tidy the linear model object:
  tidy() %>% slice(1:5)


#Finally, when using predict(workflow, new_data), no model or preprocessor parameters
#like those from recipes are reestimated using the values in new_data.
#Take centering and scaling using step_normalize() as an example.
#Using this step, the means and standard deviations from the appropriate columns
#are determined from the training set; new samples at prediction time are standardized
#using these values from training when predict() is invoked.

#step_unknown() can be used to change missing values to a dedicated factor level.
#if we anticipate that a new factor level may be encountered in future data, step_novel() can allot a new level for this purpose.
#Additionally, step_other() can be used to analyze the frequencies of the factor levels in the training set and convert infrequently occurring values to a catch-all level of “other,” with a threshold that can be specified.

ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01, id = "my_id") %>%
  step_dummy(all_nominal_predictors()) |>
  # Gr_Liv_Area is on the log scale from a previous step
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_")) |>
  step_ns(Latitude, Longitude, deg_free = 20)
  #step_pca(matches("(SF$)|(Gr_Liv)"))
  #step_downsample(outcome_column_name)

tidy(ames_rec)

lm_wflow <-
  workflow() %>%
  add_model(lm_model) %>%
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

estimated_recipe <- lm_fit %>% extract_recipe(estimated = TRUE)
tidy(estimated_recipe, id = "my_id")
tidy(estimated_recipe, number = 2)
tidy(estimated_recipe, number = 3)


ames_rec %>% update_role(Year_Built, new_role = "street address")


## EVALUATING

ames_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res
ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))
ames_test_res

ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) +
  # Create a diagonal line:
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

rmse(ames_test_res, truth = Sale_Price, estimate = .pred)
ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)

f_meas(two_class_example, truth, predicted, event_level = "second")
two_class_curve <- roc_curve(two_class_example, truth, Class1) two_class_curve
roc_auc(two_class_example, truth, Class1)
autoplot(two_class_curve)

#gain_curve(), lift_curve(), and pr_curve().

## RESAMPLING (CV)

#most blackbox are low bias, statistcal are high bias
set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10)
ames_folds
ames_folds$splits[[1]] %>% analysis() %>% dim() #assessment()

vfold_cv(ames_train, v = 10, repeats = 5)
mc_cv(ames_train, prop = 9/10, times = 20)

set.seed(1002)
val_set <- validation_split(ames_train, prop = 3/4)
val_set
bootstraps(ames_train, times = 5)

model_spec %>% fit_resamples(formula, resamples, ...)
model_spec %>% fit_resamples(recipe, resamples, ...)
workflow %>% fit_resamples( resamples, ...)


keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
set.seed(1003)
lm_res <-
  lm_wflow %>%
  fit_resamples(resamples = ames_folds, control = keep_pred)
lm_res

collect_metrics(lm_res)
assess_res <- collect_predictions(lm_res)
assess_res
#collect_predictions(object, summarize = TRUE).
assess_res %>% ggplot(aes(x = Sale_Price, y = .pred)) + geom_point(alpha = .15) + geom_abline(color = "red") + coord_obs_pred() + ylab("Predicted")

over_predicted <-
  assess_res %>%
  mutate(residual = Sale_Price - .pred) %>%
  arrange(desc(abs(residual))) %>%
  slice(1:2)
over_predicted

ames_train %>%
  slice(over_predicted$.row) %>%
  select(Gr_Liv_Area, Neighborhood, Year_Built, Bedroom_AbvGr, Full_Bath)


val_res <- lm_wflow %>% fit_resamples(resamples = val_set)
val_res
collect_metrics(val_res)



parallel::detectCores(logical = TRUE)


## COMPARE MODELS WITH RESAMPLING
library(tidymodels)
tidymodels_prefer()

basic_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal_predictors())

interaction_rec <-
  basic_rec %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )

spline_rec <-
  interaction_rec %>%
  step_ns(Latitude, Longitude, deg_free = 50)

preproc <-
  list(basic = basic_rec,
       interact = interaction_rec,
       splines = spline_rec
  )

lm_models <- workflow_set(preproc, list(lm = linear_reg()), cross = FALSE)
lm_models


lm_models <-
  lm_models %>%
  workflow_map("fit_resamples",
               # Options to `workflow_map()`:
               seed = 1101, verbose = TRUE,
               # Options to `fit_resamples()`:
               resamples = ames_folds, control = keep_pred)
lm_models

collect_metrics(lm_models) %>% filter(.metric == "rmse")


## TUNING
rand_forest(trees = 2000, min_n = 10) %>%                   # <- main arguments
  set_engine("ranger", regularization.factor = 0.5)         # <- engine-specific

neural_net_spec <-
  mlp(hidden_units = tune()) %>%
  set_mode("regression") %>%
  set_engine("keras")


ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train)  %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = tune()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
  step_ns(Longitude, deg_free = tune("longitude df")) %>%
  step_ns(Latitude,  deg_free = tune("latitude df"))

recipes_param <- extract_parameter_set_dials(ames_rec)
recipes_param


wflow_param <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(neural_net_spec) %>%
  extract_parameter_set_dials()
wflow_param

