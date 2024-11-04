# Time Series Analysis & Prediction
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(timetk)
library(lubridate)

# Core
library(tidyverse)
library(gt)
library(gtExtras)
library(readxl)
library(janitor)
library(paletteer)

# DATA ----

cashflow_raw_tbl <- read_excel(
    path  = "data/financial-statements-exxon-mobil.xlsx",
    sheet = 1,
    skip  = 1
)

# 1.0 DATA PREPARATION ----

# * Cash Flow Wide Format ----
cashflow_wide_tbl <- cashflow_raw_tbl %>%
    clean_names() %>%
    rowid_to_column("item_id") %>%
    rename(item_name = in_million_usd) %>%
    mutate(group_id = c(1,1,1,1,1, 2,2,2,2, 3,3,3,3,3, 4,4,4)) %>%
    mutate(item_type = c(
        rep("input", 4), "output",
        rep("input", 3), "output",
        rep("input", 4), "output",
        "output", "input", "output"
    )) %>%
    select(group_id, starts_with("item"), everything()) %>%
    mutate(across(starts_with("fy_"), ~ replace_na(., 0)))

cashflow_wide_tbl

# * Cashflow Table -----
cashflow_wide_tbl %>%

    rowwise() %>%
    mutate(trend = list(c_across(fy_09:fy_18))) %>%
    ungroup() %>%

    # group_by(group_id) %>%
    gt() %>%
    gtExtras::gt_plt_sparkline(trend) %>%
    tab_header(
        title = "Cash Flow Statement",
        subtitle = "Exxon Mobil (FY2009 - FY2018)"
    ) %>%
    tab_spanner(
        label = "Fiscal Year (Values in Millions)",
        columns = fy_09:fy_18,
    ) %>%
    cols_label(
        item_id = "Item No.",
        item_name = "Item Name",
        item_type = "Item Type",
        fy_09 = "2009",
        fy_10 = "2010",
        fy_11 = "2011",
        fy_12 = "2012",
        fy_13 = "2013",
        fy_14 = "2014",
        fy_15 = "2015",
        fy_16 = "2016",
        fy_17 = "2017",
        fy_18 = "2018",
        trend = "Trend"
    ) %>%
    fmt_currency(
        columns  = fy_09:fy_18,
        decimals = 0,
        accounting = TRUE
    ) %>%
    cols_align(align = "center") %>%
    gtExtras::gt_highlight_rows(
        rows = item_type == "output",
        fill = "lightgrey"
    ) %>%
    cols_hide(columns = c(item_type, group_id)) %>%
    tab_options(
        heading.title.font.size = 20,
        table.font.size = 13,
        heading.subtitle.font.size = 12,
        column_labels.font.weight = "bold",
    )

# * Cash Flow Long Format ----

cashflow_long_tbl <- cashflow_wide_tbl %>%
    pivot_longer(
        cols     = starts_with("fy_"),
        names_to = "fiscal_year_id"
    ) %>%
    mutate(
        item_id  = as.factor(item_id),
        group_id = as.factor(group_id)
    ) %>%
    mutate(date = lubridate::make_date(year = str_glue("20{str_remove(fiscal_year_id, 'fy_')}")))

cashflow_long_tbl

# 2.0 TIME SERIES ANALYSIS -----


# * Trelliscope Plot ----
cashflow_long_tbl %>%
    group_by(group_id, item_id, item_name) %>%
    plot_time_series(
        date, value,
        .facet_ncol  = 2,
        .facet_nrow  = 2,
        .y_intercept = 0,
        .trelliscope = TRUE
    )

# 3.0 ITERATIVE FORECASTING ----

# * Nested Data Format -----
nested_data_tbl <- cashflow_long_tbl %>%
    select(item_id, date, value) %>%
    extend_timeseries(
        .id_var        = item_id,
        .date_var      = date,
        .length_future = 3
    ) %>%

    # ** Add x-regs ----
    mutate(
        category_oil_shock = case_when(
            year(date) <= 2014            ~ "pre-oil-shock",
            year(date) %in% c(2015, 2016) ~ "oil-shock",
            year(date) > 2016             ~ "post-oil-shock"
        ) %>%
            as.factor()
    ) %>%

    nest_timeseries(
        .id_var        = item_id,
        .length_future = 3
    ) %>%
    split_nested_timeseries(
        .length_test   = 3
    )

nested_data_tbl

extract_nested_train_split(nested_data_tbl)

extract_nested_test_split(nested_data_tbl)


# 4.0 MAKING TIME SERIES MODELS ----

# ** recipes ----

recipe_xreg_simple <- recipe(value ~ ., data = extract_nested_train_split(nested_data_tbl))

recipe_xreg_simple %>% prep() %>% juice() %>% glimpse()

recipe_xreg_ml <- recipe_xreg_simple %>%
    step_timeseries_signature(date) %>%
    step_select(value, date_index.num, date_year, category_oil_shock) %>%
    step_dummy(category_oil_shock, one_hot = TRUE)

recipe_xreg_ml %>% prep() %>% juice() %>% glimpse()

# ** ARIMA ----

model_arima <- arima_reg(seasonal_period = 1) %>%
    set_engine("auto_arima")

wflw_arima <- workflow() %>%
    add_model(model_arima) %>%
    add_recipe(recipe_xreg_simple) %>%
    fit(extract_nested_train_split(nested_data_tbl))

wflw_arima

# ** Exponential Smoothing (ets) ----

model_ets <- exp_smoothing(seasonal_period = 1) %>%
    set_engine("ets")

wflw_ets <- workflow() %>%
    add_model(model_ets) %>%
    add_recipe(recipe_xreg_simple) %>%
    fit(extract_nested_train_split(nested_data_tbl))

wflw_ets


# 5.0 HYPERPARAMETER TUNING FOR ITERATIVE FORECASTING ----

# * xgboost - model list -----

model_list_xgboost <- tibble(
    learn_rate = c(0.010, 0.100, 0.350, 0.500)
) %>%
    create_model_grid(
        f_model_spec = boost_tree,
        engine_name  = "xgboost",
        mode         = "regression"
    ) %>%
    pluck(".models") %>%
    map( function(model) {
        workflow() %>%
            add_model(model) %>%
            add_recipe(recipe_xreg_ml) %>%
            fit(extract_nested_train_split(nested_data_tbl))
    })

model_list_xgboost

# * glmnet -----

model_list_glmnet <- tibble(
    penalty = c(0, 0.01, 0.1)
) %>%
    create_model_grid(
        f_model_spec = linear_reg,
        engine_name  = "glmnet",
        mode         = "regression"
    ) %>%
    pluck(".models") %>%
    map( function(model) {
        workflow() %>%
            add_model(model) %>%
            add_recipe(recipe_xreg_ml) %>%
            fit(extract_nested_train_split(nested_data_tbl))
    })

model_list_glmnet

# 6.0 MODELTIME NESTED WORKFLOW ----

# * Make Submodels ----
# Finished in: 1.4182 mins.
nested_modeltime_tbl <- modeltime_nested_fit(

    # Nested data
    nested_data = nested_data_tbl,

    # Individual models
    wflw_arima,
    wflw_ets,

    # Hyper parameter lists
    model_list = c(model_list_xgboost, model_list_glmnet),

    control = control_nested_fit(verbose = TRUE)
)

# nested_modeltime_tbl %>% write_rds("model_output/nested_modeltime_tbl.rds")
nested_modeltime_tbl <- read_rds("model_output/nested_modeltime_tbl.rds")

# * Checking Submodels ----
nested_modeltime_tbl %>%
    extract_nested_test_accuracy()

nested_modeltime_tbl %>%
    extract_nested_test_forecast() %>%
    group_by(item_id) %>%
    plot_modeltime_forecast(
        .facet_ncol  = 2,
        .interactive = TRUE,
        .trelliscope = TRUE
    )

# 7.0 MODELTIME NESTED ENSEMBLES -----

# * Make Ensemble ----

# Finished in: 35.52419 secs.
nested_ensemble_tbl <- nested_modeltime_tbl %>%
    ensemble_nested_weighted(
        loadings = c(3, 2, 1),
        metric   = "rmse",
        control  = control_nested_fit(verbose = TRUE)
    )

# nested_ensemble_tbl %>% write_rds("model_output/nested_ensemble_tbl.rds")
nested_ensemble_tbl <- read_rds("model_output/nested_ensemble_tbl.rds")


# * Check Ensemble -----

nested_ensemble_tbl %>%
    extract_nested_test_accuracy() %>%
    group_by(item_id) %>%
    table_modeltime_accuracy(.interactive = FALSE) %>%
    gtExtras::tab_style_by_grp(
        column = rmse,
        fn     = min,
        cell_fill("lightblue")
    )

# 8.0 SELECT BEST FORECASTS ----

# * Select best ----
nested_best_tbl <- nested_ensemble_tbl %>%
    modeltime_nested_select_best(
        metric                = "rmse",
        minimize              = TRUE,
        filter_test_forecasts = TRUE
    )

# * Check best ----
nested_best_tbl %>%
    extract_nested_best_model_report() %>%
    table_modeltime_accuracy(.interactive = FALSE)

nested_best_tbl %>%
    extract_nested_test_forecast() %>%
    group_by(item_id) %>%
    plot_modeltime_forecast(
        .facet_ncol  = 1,
        .interactive = TRUE,
        .trelliscope = TRUE
    )

# 9.0 REFIT & FUTURE FORECAST ----

# * Refit ----
#   Finished in: 12.43 secs.
nested_best_refit_tbl <- nested_best_tbl %>%
    modeltime_nested_refit(
        control = control_nested_refit(verbose = TRUE)
    )

# nested_best_refit_tbl %>% write_rds("model_output/nested_best_refit_tbl.rds")
nested_best_refit_tbl <- read_rds("model_output/nested_best_refit_tbl.rds")

nested_best_refit_tbl %>%
    extract_nested_future_forecast() %>%
    left_join(
        cashflow_wide_tbl %>%
            select(item_id, item_name) %>%
            mutate(item_id = as.factor(item_id))
    ) %>%
    group_by(item_id, item_name) %>%
    plot_modeltime_forecast(
        .facet_ncol  = 1,
        .interactive = TRUE,
        .trelliscope = TRUE,
        .legend_show = FALSE
    )

# 10.0 PROFORMA CASH FLOW STATEMENT ----

# * Cash Flow Wide ----
cashflow_wide_tbl

cashflow_long_tbl

# * Join Cashflow and Proforma ----
proforma_tbl <- nested_best_refit_tbl %>%
    extract_nested_future_forecast() %>%
    filter(.key == "prediction") %>%
    select(item_id, .index, .value) %>%
    mutate(
        fiscal_year_id = year(.index) %>%
            as.character() %>%
            str_sub(3),
        fiscal_year_id = str_glue("fy_{fiscal_year_id}") %>% as.character()
    ) %>%
    select(-.index) %>%
    pivot_wider(
        names_from = fiscal_year_id,
        values_from = .value
    )

cashflow_wide_proforma_tbl <- cashflow_wide_tbl %>%
    mutate(item_id = as.factor(item_id)) %>%
    left_join(proforma_tbl)

# * Forecast Reconciliation ----

cols_ <- str_c("fy_", str_pad(9:21, width = 2, side = 'left', pad = "0"))

sums_output_1 <- colSums(cashflow_wide_proforma_tbl[1:4, which(names(cashflow_wide_proforma_tbl) %in% cols_)])
cashflow_wide_proforma_tbl[5, 5:17] <- sums_output_1 %>% unname() %>% as.list()

sums_output_2 <- colSums(cashflow_wide_proforma_tbl[6:8, which(names(cashflow_wide_proforma_tbl) %in% cols_)])
cashflow_wide_proforma_tbl[9, 5:17] <- sums_output_2 %>% unname() %>% as.list()

sums_output_3 <- colSums(cashflow_wide_proforma_tbl[10:13, which(names(cashflow_wide_proforma_tbl) %in% cols_)])
cashflow_wide_proforma_tbl[14, 5:17] <- sums_output_3 %>% unname() %>% as.list()

sums_output_4 <- colSums(cashflow_wide_proforma_tbl[15:16, which(names(cashflow_wide_proforma_tbl) %in% cols_)])
cashflow_wide_proforma_tbl[17, 5:17] <- sums_output_4 %>% unname() %>% as.list()


# * Final Proforma Cash Flow Table ----

cashflow_wide_proforma_tbl %>%
    # group_by(group_id) %>%
    gt() %>%

    tab_header(
        title = "Cash Flow Statement",
        subtitle = "Exxon Mobil (FY2009 - FY2018)"
    ) %>%
    tab_spanner(
        label = "Fiscal Year (Values in Millions)",
        columns = fy_09:fy_18,
    ) %>%
    tab_spanner(
        label = "Pro-Forma (Values in Millions)",
        columns = fy_19:fy_21,
    ) %>%
    cols_label(
        item_id = "Item No.",
        item_name = "Item Name",
        item_type = "Item Type",
        fy_09 = "2009",
        fy_10 = "2010",
        fy_11 = "2011",
        fy_12 = "2012",
        fy_13 = "2013",
        fy_14 = "2014",
        fy_15 = "2015",
        fy_16 = "2016",
        fy_17 = "2017",
        fy_18 = "2018",

        fy_19 = "2019",
        fy_20 = "2020",
        fy_21 = "2021"
    ) %>%
    fmt_currency(
        columns    = fy_09:fy_21,
        decimals   = 0,
        accounting = TRUE
    ) %>%
    cols_align(align = "center") %>%
    gtExtras::gt_highlight_rows(
        rows = item_type == "output",
        fill = "lightgrey"
    ) %>%
    cols_hide(columns = c(item_type, group_id)) %>%
    tab_options(
        heading.title.font.size = 20,
        table.font.size = 13,
        heading.subtitle.font.size = 12,
        column_labels.font.weight = "bold",
    )
