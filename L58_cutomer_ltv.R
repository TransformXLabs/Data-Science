library(tidymodels)
library(vip)
library(tidyverse)
library(timetk)
library(lubridate)

# 1.0 DATA PREPARATION ----

# * Data Import ----
cdnow_raw_tbl <- vroom::vroom(
    file = "data/CDNOW_master.txt",
    delim = " ",
    col_names = FALSE
)

# * Data Cleanup -----
cdnow_tbl <- cdnow_raw_tbl %>%
    select(X2, X3, X5, X8) %>%
    set_names(
        c("customer_id", "date", "quantity", "price")
    ) %>%
    mutate(date = ymd(as.character(date))) %>%
    drop_na()


# 2.0 COHORT ANALYSIS ----
# - Only the customers that have joined at the specific business day

# * Get Range of Initial Purchases ----
cdnow_first_purchase_tbl <- cdnow_tbl %>%
    group_by(customer_id) %>%
    slice_min(date) %>%
    ungroup()

cdnow_first_purchase_tbl %>%
    pull(date) %>%
    range()

# "1997-01-01" "1998-06-26"

# * Set Cohort Span ----
#   - Set initial purchase: 1997-01-01 1997-03-31
ids_in_cohort <- cdnow_first_purchase_tbl %>%
    filter_by_time(
        .start_date = "1997-01",
        .end_date   = "1997-03"
    ) %>%
    distinct(customer_id) %>%
    pull(customer_id)

cdnow_cohort_tbl <- cdnow_tbl %>%
    filter(customer_id %in% ids_in_cohort)

# * Visualize: Total Cohort Purchases ----

cdnow_cohort_tbl %>%
    summarize_by_time(
        total_price = sum(price, na.rm = TRUE),
        .by   = "month"
    ) %>%
    plot_time_series(date, total_price, .y_intercept = 0)


# * Visualize: Individual Customer Purchases ----
n    <- 1:10
ids  <- unique(cdnow_cohort_tbl$customer_id)[n]

cdnow_cohort_tbl %>%
    filter(customer_id %in% ids) %>%
    group_by(customer_id) %>%
    plot_time_series(
        date, price,
        .y_intercept = 0,
        .smooth      = FALSE,
        .facet_ncol  = 2,
        .interactive = FALSE,
        .title = "Customer Purchase Behavior"
    ) +
    geom_point(color = "#2c3e50")


# 3.1 SPLITTING (2-Stages) ----

# ** Stage 1: Random Splitting by Customer ID ----

set.seed(123)
ids_train <- cdnow_cohort_tbl %>%
    pull(customer_id) %>%
    unique() %>%
    sample(size = round(0.8*length(.))) %>%
    sort()

split_1_train_tbl <- cdnow_cohort_tbl %>%
    filter(customer_id %in% ids_train)

split_1_test_tbl  <- cdnow_cohort_tbl %>%
    filter(!customer_id %in% ids_train)

# ** Stage 2: Time Splitting ----

splits_2_train <- time_series_split(
    split_1_train_tbl,
    assess     = "90 days",
    cumulative = TRUE
)

splits_2_train %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, price)

splits_2_test <- time_series_split(
    split_1_test_tbl,
    assess     = "90 days",
    cumulative = TRUE
)

splits_2_test %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, price)

# 3.2 FEATURE ENGINEERING (RFM) ----

# ** Make in-sample targets from training data ----
targets_train_tbl <- testing(splits_2_train) %>%
    group_by(customer_id) %>%
    summarise(
        spend_90_total = sum(price),
        spend_90_flag    = 1
    )

# ** Make out-sample targets from testing(splits_2) ----
targets_test_tbl <- testing(splits_2_test) %>%
    group_by(customer_id) %>%
    summarise(
        spend_90_total = sum(price),
        spend_90_flag    = 1
    )

# ** Make Training Data ----
max_date_train <- training(splits_2_train) %>%
    pull(date) %>%
    max()

train_tbl <- training(splits_2_train) %>%
    group_by(customer_id) %>%
    summarise(
        recency   = (max(date) - max_date_train) / ddays(1),
        frequency = n(),
        price_sum   = sum(price, na.rm = TRUE),
        price_mean  = mean(price, na.rm = TRUE)
    ) %>%
    left_join(
        targets_train_tbl
    ) %>%
    replace_na(replace = list(
            spend_90_total = 0,
            spend_90_flag  = 0
        )
    ) %>%
    mutate(spend_90_flag = as.factor(spend_90_flag))

# ** Make Testing Data ----
test_tbl <- training(splits_2_test) %>%
    group_by(customer_id) %>%
    summarise(
        recency     = (max(date) - max_date_train) / ddays(1),
        frequency   = n(),
        price_sum   = sum(price, na.rm = TRUE),
        price_mean  = mean(price, na.rm = TRUE)
    ) %>%
    left_join(
        targets_test_tbl
    ) %>%
    replace_na(replace = list(
        spend_90_total = 0,
        spend_90_flag  = 0
    )
    ) %>%
    mutate(spend_90_flag = as.factor(spend_90_flag))

# 3.3 RECIPES ----

# ** Model 1: 90-Day Spend Prediction ----
recipe_spend_total <- recipe(spend_90_total ~ ., data = train_tbl) %>%
    step_rm(spend_90_flag, customer_id)

# ** Model 2: 90-Day Spend Probability ----
recipe_spend_prob <- recipe(spend_90_flag ~ ., data = train_tbl) %>%
    step_rm(spend_90_total, customer_id)

recipe_spend_prob %>% prep() %>% juice() %>% glimpse()

summary(recipe_spend_prob)

# 3.4 MODELS ----

# ** Model 1: 90-Day Spend Prediction ----
wflw_spend_total_xgb <- workflow() %>%
    add_model(
        boost_tree(
            mode = "regression"
        ) %>%
            set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spend_total) %>%
    fit(train_tbl)

# ** Model 2: 90-Day Spend Probability ----
wflw_spend_prob_xgb <- workflow() %>%
    add_model(
        boost_tree(
            mode = "classification"
        ) %>%
            set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spend_prob) %>%
    fit(train_tbl)

# 3.5 TEST SET EVALUATION ----

# * Make Test Predictions ----

predictions_test_tbl <-  bind_cols(

        predict(wflw_spend_total_xgb, test_tbl) %>%
            rename(.pred_total = .pred),

        predict(wflw_spend_prob_xgb, test_tbl, type = "prob") %>%
            select(.pred_1) %>%
            rename(.pred_prob = .pred_1)
    ) %>%
    bind_cols(test_tbl) %>%
    select(starts_with(".pred"), starts_with("spend_"), everything())

# * Model Test Accuracy ----

predictions_test_tbl %>%
    yardstick::mae(spend_90_total, .pred_total)

predictions_test_tbl %>%
    yardstick::roc_auc(spend_90_flag, .pred_prob, event_level = "second")

predictions_test_tbl %>%
    yardstick::roc_curve(spend_90_flag, .pred_prob, event_level = "second")%>%
    autoplot()

# 3.6 FEATURE IMPORTANCE ----

# * Probability Model ----
vip(wflw_spend_prob_xgb$fit$fit)

# * Spend Model ----
vip(wflw_spend_total_xgb$fit$fit)

# 3.7 SAVE WORK ----

fs::dir_create("artifacts")

wflw_spend_prob_xgb %>% write_rds("artifacts/model_prob.rds")
wflw_spend_total_xgb %>% write_rds("artifacts/model_spend.rds")

vi_model(wflw_spend_prob_xgb$fit$fit) %>% write_rds("artifacts/vi_prob.rds")
vi_model(wflw_spend_total_xgb$fit$fit) %>% write_rds("artifacts/vi_spend.rds")

all_tbl <- bind_rows(train_tbl, test_tbl)
predictions_all_tbl <- bind_cols(
        predict(wflw_spend_total_xgb, all_tbl) %>%
            rename(.pred_total = .pred),
        predict(wflw_spend_prob_xgb, all_tbl, type = "prob") %>%
            select(.pred_1) %>%
            rename(.pred_prob = .pred_1)
    ) %>%
    bind_cols(all_tbl) %>%
    select(starts_with(".pred"), starts_with("spend_"), everything())

predictions_all_tbl %>% write_rds("artifacts/predictions_all_tbl.rds")


# 4.0 HOW WE CAN USE THIS INFORMATION? ----

predictions_test_tbl %>%
    arrange(desc(.pred_prob))

predictions_test_tbl %>%
    filter(
        recency    > -90,
        .pred_prob < 0.2
    ) %>%
    arrange(.pred_prob)

predictions_test_tbl %>%
    arrange(desc(.pred_total)) %>%
    filter(
        spend_90_total == 0
    )
