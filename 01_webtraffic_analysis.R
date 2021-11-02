

# LIBRARIES ----

library(sparklyr)
library(modeltime)
library(modeltime.ensemble)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
library(janitor)

# MAIN DATA ----

google_analytics_tbl <- read_rds("data/google_analytics_by_page_daily.rds") %>%
  clean_names

google_analytics_tbl

# EXTERNAL DATA ----
# - Possible Regressors

# * Product Events ----
product_events_tbl <- read_rds("data/product_events.rds")
product_events_tbl

product_events_extended_tbl <- product_events_tbl %>%
  rowid_to_column("event_id") %>%
  group_by(event_id) %>%
  mutate(date = list(tk_make_timeseries(date, by = 'day', length_out = 4))) %>%
  unnest(date) %>%
  ungroup()

# * Webinars ----
webinars_tbl <- read_rds("data/webinars.rds") %>%
  mutate(event_date = ymd_hms(event_date) %>% as_date())
webinars_tbl 

webinars_extended_tbl <- webinars_tbl %>%
  group_by(id) %>%
  mutate(date = list(tk_make_timeseries(event_date, by = 'day', length_out = 2))) %>%
  select(-event_date) %>%
  unnest(date) %>%
  ungroup()

# DATA VISUALIZATION ----

n <- 4

top_pages <- google_analytics_tbl %>%
  group_by(page_path) %>%
  summarise(total_page_views = sum(page_views)) %>%
  arrange(-total_page_views) %>%
  slice(1:n)

top_pages$page_path


google_analytics_filtered_tbl <- google_analytics_tbl %>%
  filter(page_path %in% top_pages$page_path)
  

google_analytics_filtered_tbl %>%
  group_by(page_path) %>%
  plot_time_series(date, page_views)

# * Plot Regressors ----
google_analytics_filtered_tbl %>%
  select(date, page_path, page_views) %>%
  mutate(is_product_launch = date %in% product_events_extended_tbl$date %>% as.numeric()) %>%
  mutate(is_webinar = date %in% webinars_extended_tbl$date %>% as.numeric()) %>%
  group_by(page_path) %>%
  plot_time_series_regression(
    .date_var = date, 
    .formula = page_views ~ 
      wday(date, label = TRUE) +
      month(date, label = TRUE) +
      is_product_launch +
      is_webinar,
    .show_summary = TRUE
  )


# DATA PREPARATION ----

nested_data_tbl <- google_analytics_filtered_tbl %>%
  select(date, page_path, page_views) %>%
  group_by(page_path) %>%
  
  # Extend Time Series
  extend_timeseries(
    .id_var = page_path, 
    .date_var = date, 
    .length_future = 28
  ) %>%
  
  # Add External Regressors
  mutate(is_product_launch = date %in% product_events_extended_tbl$date %>% as.numeric()) %>%
  mutate(is_webinar = date %in% webinars_extended_tbl$date %>% as.numeric()) %>%
  
  # Nest
  nest_timeseries(
    .id_var = page_path, 
    .length_future = 28
  ) %>%
  
  split_nested_timeseries(
    .length_test = 28
  )

nested_data_tbl

# NESTED MODELTIME WORKFLOW ----

# * Prophet ----
rec_prophet <- recipe(page_views ~ ., extract_nested_train_split(nested_data_tbl)) 

wflw_prophet <- workflow() %>%
  add_model(
    prophet_reg("regression", seasonality_yearly = TRUE) %>% 
      set_engine("prophet")
  ) %>%
  add_recipe(rec_prophet)

wflw_prophet %>% fit(extract_nested_train_split(nested_data_tbl))

# * XGBoost ----

rec_xgb <- recipe(page_views ~ ., extract_nested_train_split(nested_data_tbl)) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

wflw_xgb <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(rec_xgb)

wflw_xgb %>% fit(extract_nested_train_split(nested_data_tbl))

# * Smooth: Exponential Smoothing with Regressors ----

show_engines('exp_smoothing')

wflw_ets_smooth <- workflow() %>%
  add_model(exp_smoothing(seasonal_period = 7) %>% set_engine("smooth_es")) %>%
  add_recipe(rec_prophet)

wflw_ets_smooth %>% fit(extract_nested_train_split(nested_data_tbl))

# * Smooth: ADAM 

wflw_adam <- workflow() %>%
  add_model(adam_reg() %>% set_engine("auto_adam")) %>%
  add_recipe(rec_prophet)

wflw_adam %>% fit(extract_nested_train_split(nested_data_tbl))


# SPARK: GET READY FOR TRAINING -----  

conf <- list()
conf$`sparklyr.cores.local` <- 12
conf$`sparklyr.shell.driver-memory` <- "16G"
conf$spark.memory.fraction <- 0.9

sc <- sparklyr::spark_connect(master = "local[12]", config = conf)

spark_web(sc) # localhost:4040

parallel_start(sc, .method = 'spark')

# PARALLEL FITTING WITH SPARK ----

nested_modeltime_tbl <- nested_data_tbl %>%
  modeltime_nested_fit(
    model_list = list(
      wflw_prophet, 
      wflw_xgb,
      wflw_ets_smooth,
      wflw_adam
    ),
    control    = control_nested_fit(
      verbose  = TRUE,
      allow_par = TRUE
    )
  )

nested_modeltime_tbl %>% 
  extract_nested_error_report()

nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  group_by(page_path) %>%
  table_modeltime_accuracy()

nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(page_path) %>%
  plot_modeltime_forecast()


# ENSEMBLING NESTED ----

bad_page_paths <- nested_modeltime_tbl %>% 
  extract_nested_error_report() %>%
  pull(page_path) %>%
  unique()

# * Average Ensemble ----

nested_modeltime_ensembles_tbl <- nested_modeltime_tbl %>%
  filter(!page_path %in% bad_page_paths) %>%
  ensemble_nested_average(
    type    = "mean",
    control = control_nested_fit(
      verbose   = TRUE, 
      allow_par = TRUE
    )
    
  )

nested_modeltime_ensembles_tbl %>% 
  extract_nested_modeltime_table(1) 

nested_modeltime_ensembles_tbl %>%
  extract_nested_test_accuracy() %>%
  group_by(page_path) %>%
  table_modeltime_accuracy()

# * Weighted Ensemble ----

nested_modeltime_ensembles_2_tbl <- nested_modeltime_ensembles_tbl %>%
  filter(!page_path %in% bad_page_paths) %>%
  ensemble_nested_weighted(
    
    loadings = c(3,1),
    
    model_ids = 1:4,
    
    metric = "rmse",
    
    control = control_nested_fit(
      verbose   = TRUE, 
      allow_par = TRUE
    )
    
  )

nested_modeltime_ensembles_2_tbl %>% extract_nested_error_report()

nested_modeltime_ensembles_2_tbl %>%
  extract_nested_test_accuracy() %>%
  group_by(page_path) %>%
  table_modeltime_accuracy()


# SELECT BEST MODEL ----

best_nested_modeltime_tbl <- nested_modeltime_ensembles_2_tbl %>%
  modeltime_nested_select_best(metric = "rmse")

best_nested_modeltime_tbl %>% extract_nested_modeltime_table(1)


# REFIT AND FORECAST ----

nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_fit(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(page_path) %>%
  plot_modeltime_forecast()


# CONCLUSIONS ----

# 1. Time Series - Incredibly important to businesses. 
#  Modeltime has ability to:
#    - Perform Iteration at Scale with Spark
#    - Add External Regressors
#    - Ensemble the best models
#    - Even use lots of different models (experimentation is key)
#  So, you should learn Modeltime! 

# 2. Production is key!
# - You're going to make analyses that 
#   your company will want to use.
# - But, you need to package it up in a format they can use: Shiny!
# So, you should learn Shiny!

# How are you going to do this? I have a solution. 


