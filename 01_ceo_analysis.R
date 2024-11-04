
library(tidymodels)
library(workflowsets)
library(finetune) # <- Racing Methods
library(vip)

## Parallel Processing / Job Management ----
library(parallel)
library(doParallel)
library(job)

## EDA & Cleaning ----
library(DataExplorer)
library(janitor)

## Time Series ----
library(timetk)
library(lubridate)

## Finance / Stock Data ----
library(tidyquant)

## Core ----
library(tidyverse)




# COLLECT DATA ----

## CEO Compensation ----
ceo_compensation_raw_tbl <- read_csv("00_data/ceo_compensation_2008.csv")
ceo_compensation_raw_tbl %>% glimpse()

# High Missing
ceo_compensation_raw_tbl %>% 
  profile_missing() %>%
  arrange(desc(pct_missing)) 

ceo_compensation_tbl <- ceo_compensation_raw_tbl %>%
  clean_names() %>% 
  mutate(x6_year_annual_total_return = parse_number(x6_year_annual_total_return)) %>%
  mutate(total_return_during_tenure = parse_number(total_return_during_tenure)) %>%
  select(-starts_with("x2008_"), -x5_year_compensation_total, -x6_year_average_compensation)

ceo_compensation_tbl %>% glimpse()

## Stock Data ----

### LONG RUNNING SCRIPT ----
# - Run as Job: See Addins > Run Selection as Job
# stock_prices_tbl <- ceo_compensation_tbl %>%
#   pull(ticker) %>%
#   tq_get(get = "stock.prices", from = "2004-01-01", to = "2009-01-01")

# stock_prices_tbl %>% write_rds("00_data/stock_prices.rds")

stock_prices_tbl <- read_rds("00_data/stock_prices.rds") %>%
  distinct()

stock_prices_tbl %>%
  filter(symbol == "AAPL") %>%
  plot_time_series(date, adjusted)
  
## Stock Performance ----

performance_by_year_tbl <- stock_prices_tbl %>%
  
  # Summarize by symbol and time
  group_by(symbol) %>%
  summarize_by_time(
    .by = "year",
    n = n(),
    total_performance = (last(adjusted) - first(adjusted) ) / first(adjusted)
  ) %>%
  ungroup() %>%
  
  # Pivot Yearly Performance
  mutate(year = str_glue("performance_{year(date)}")) %>%
  select(symbol, year, total_performance) %>%
  pivot_wider(
    id_cols     = symbol, 
    names_from  = year,
    values_from = total_performance
  ) %>%
  
  # Calculate aggregated metrics
  rowwise() %>%
  mutate(
    performance_mean    = mean(c_across(starts_with("perf")), na.rm = T),
    performance_median  = median(c_across(starts_with("perf")), na.rm = T),
    performance_stdev   = sd(c_across(starts_with("perf")), na.rm = T),
    performance_missing = sum(is.na(c_across(starts_with("perf"))), na.rm = TRUE)
  ) %>%
  ungroup()

performance_by_year_tbl %>% glimpse()

# Stock Return Similarity Analysis ----

stock_returns_wide_tbl <- stock_prices_tbl %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  mutate(daily_returns = diff_vec(adjusted, lag = 1, silent = TRUE) / lag_vec(adjusted, lag = 1)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols     = date,
    names_from  = symbol, 
    values_from = daily_returns,
    values_fill = 0
  ) %>%
  drop_na() %>%
  select(-date)

stock_returns_wide_tbl %>%
  cor() %>%
  as_tibble()

kmeans_obj <- stock_returns_wide_tbl %>% 
  as.matrix() %>%
  t() %>%
  scale() %>%
  kmeans(centers = 10, iter.max = 20)

stock_similarity_tbl <- kmeans_obj$cluster %>% 
  enframe(name = "symbol", value = "kmeans_group")

## Join Data ----

ceo_compensation_joined_tbl <- ceo_compensation_tbl %>%
  left_join(performance_by_year_tbl, by = c("ticker" = "symbol")) %>%
  left_join(stock_similarity_tbl, by = c("ticker" = "symbol")) %>%
  filter(!is.na(total_2008_compensation))

# ceo_compensation_joined_tbl %>% write_rds("01_results/ceo_compensation_joined_tbl.rds")


ceo_compensation_joined_tbl %>% glimpse()

ceo_compensation_joined_tbl %>%
  glimpse() %>%
  profile_missing() %>%
  arrange(desc(pct_missing)) %>%
  View()



# MODELING DATA ----

## Recipes ----

### * Base ----
recipe_base <- recipe(total_2008_compensation ~ ., data = ceo_compensation_joined_tbl) %>%
  step_rm(ceo, company, ticker) %>%
  step_mutate_at(kmeans_group, fn = factor) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  # step_unknown(kmeans_group) %>%
  step_dummy(all_nominal(), one_hot = TRUE) 

recipe_base %>% prep() %>% juice() %>% glimpse()

### * Drop Columns w/ Missing Values ----

recipe_no_missing <- recipe_base %>%
  step_select(where(function (x) !any(is.na(x))))

recipe_no_missing %>% prep() %>% juice() %>% glimpse()

### * Impute Mean ----

recipe_impute_mean <- recipe_base %>%
  step_impute_mean(all_predictors())

recipe_impute_mean %>% prep() %>% juice() %>% glimpse()

### * Impute KNN ----

recipe_impute_knn <- recipe_base %>%
  step_impute_knn(all_predictors(), neighbors = tune())


## Models ----

glmnet_spec <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

xgboost_spec <- boost_tree(
  mode = "regression",
  trees = 500, 
  learn_rate = tune(),
  min_n      = tune()
) %>%
  set_engine("xgboost")

svm_spec <- svm_rbf(mode = "regression", cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_engine("kernlab")

## Resamples ----

set.seed(123)
resample_spec <- ceo_compensation_joined_tbl %>%
  vfold_cv(v = 5)



# WORKFLOWSET ----

## Setup the Workflow Set ----
wflwset_setup <- workflow_set(
  preproc = list( 
    no_missing  = recipe_no_missing, 
    impute_mean = recipe_impute_mean,
    impute_knn  = recipe_impute_knn
  ),
  models = list(
    glmnet  = glmnet_spec, 
    xgboost = xgboost_spec, 
    svm     = svm_spec
  ),
  cross = TRUE
) 

wflwset_setup

## Run the Workflow Map ----
cores <- parallel::detectCores(logical = FALSE)
cores

clusters <- makePSOCKcluster(cores)
registerDoParallel(clusters)

# Long running script
# - Run as Job: See Addins > Run Selection as Job
set.seed(123)
if (exists("wflwset_tune_results")) rm("wflwset_tune_results")
wflwset_tune_results <- wflwset_setup %>% 
  workflow_map(
    fn        = "tune_race_anova", 
    resamples = resample_spec, 
    grid      = 15, 
    metrics   = metric_set(rmse, rsq), 
    verbose   = TRUE
  )

doParallel::stopImplicitCluster()

# wflwset_tune_results %>% write_rds("01_results/wflwset_tune_results.rds")
wflwset_tune_results <- read_rds("01_results/wflwset_tune_results.rds")

## Inspect Workflowset Results ----

### * Visually Compare All Models ----
?autoplot.workflow_set

autoplot(wflwset_tune_results) + 
  scale_color_tq() +
  theme_tq() 
  

### * Get Rankings ----

wflwset_tune_results %>% 
  rank_results(rank_metric = "rmse") %>%
  filter(.metric == "rmse")

### * Visualize the Best Parameters ----


autoplot(wflwset_tune_results, id = "impute_mean_xgboost", metric = "rmse")
autoplot(wflwset_tune_results, id = "impute_knn_xgboost", metric = "rmse")



#  FINALIZE MODEL ----

## Get Best Parameters ----
params_best_model <- wflwset_tune_results %>%
  pull_workflow_set_result(id = "impute_mean_xgboost") %>% 
  select_best(metric = "rmse")

## Fit the Final Model ----
wflw_fit_final <- wflwset_tune_results %>%
  pull_workflow("impute_mean_xgboost") %>%
  finalize_workflow(params_best_model) %>%
  fit(ceo_compensation_joined_tbl)

## Make Predictions ----
predictions_tbl <- wflw_fit_final %>%
  predict(new_data = ceo_compensation_joined_tbl) %>%
  bind_cols(ceo_compensation_joined_tbl) %>%
  select(.pred, total_2008_compensation, everything())

# predictions_tbl %>% write_rds("01_results/predictions.rds")

predictions_tbl %>%
  ggplot(aes(total_2008_compensation, .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue")

# IMPORTANT PARAMETERS ----

importance_tbl <- vip::vi(wflw_fit_final$fit$fit$fit)

importance_tbl %>% write_rds("01_results/importance_tbl.rds")

