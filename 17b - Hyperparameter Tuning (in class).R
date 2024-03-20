library(tidyverse)
library(tidymodels)


# Setup w/ Credit Data --------------------------------------------------------

cr_data <- read_csv('https://www.dropbox.com/scl/fi/vykejw5ud9ejjvcc442gd/credit_small.csv?rlkey=zuyurxikxickgdjchh6681j91&dl=1') %>% 
  mutate(status = as.factor(status))
cr_data %>% glimpse()

# Model setup:
set.seed(42)
cr_split <- initial_split(cr_data, strata = status)
cr_training <- cr_split %>% training()
cr_testing <- cr_split %>% testing()

# Let's create a recipe that:
#    - imputes missing numeric values
#    - log transforms assets, debt, income, price, expenses
#    - normalizes all numeric predictors
#    - dummy codes all categories
#    - downsamples the bad/good status counts
cr_rec <- recipe(status ~ ., 
                 data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = 1)

cr_training %>% count(status)
cr_rec %>% prep() %>% juice() %>% count(status)

# Now let's setup a model spec (rpart decision tree), workflow, and do a cross validation.
dt_spec <- decision_tree() %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

dt_wkfl <- workflow() %>% 
  add_model(dt_spec) %>% 
  add_recipe(cr_rec)

set.seed(42)
cr_folds <- vfold_cv(cr_training, strata = status, v = 10)

cv_fit <- dt_wkfl %>% 
  fit_resamples(resamples = cr_folds)

cv_fit %>% collect_metrics(summarize = F)
# Next, a tunable model specification, recipe, and workflow:
dt_spec_tune <- decision_tree(
  tree_depth = tune(),
  min_n = tune(),
  cost_complexity = tune() 
) %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

cr_rec_tune <- recipe(status ~ ., 
                 data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = tune())


cr_wkfl_tune <-  workflow() %>% 
  add_model(dt_spec_tune) %>% 
  add_recipe(cr_rec_tune)

# Extract parameters, create a grid to search, then search:

cr_grid <- grid_random(extract_parameter_set_dials(cr_wkfl_tune), size = 10)

doParallel::registerDoParallel(cores = 7)

grid_search_results <-  cr_wkfl_tune %>% 
  tune_grid(resamples = cr_folds,
            grid = cr_grid)

# Look at the results, selecting the best one and using those to finalize.
grid_search_results %>% show_best()
grid_search_results %>% collect_metrics()

winners <- grid_search_results %>% select_best(metric = 'roc_auc')

cr_wkfl_final <- cr_wkfl_tune %>% 
  finalize_workflow(winners)

all_done <- cr_wkfl_final %>% 
  last_fit(split = cr_split)


