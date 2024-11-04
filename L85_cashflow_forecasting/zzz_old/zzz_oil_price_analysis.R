


# 3.0 ECONOMIC ANALYSIS: OIL PRICE CORRELATION ----

# https://fred.stlouisfed.org/series/DCOILWTICO
oil_prices_d_tbl <- tq_get(
    "DCOILWTICO",
    get  = "economic.data",
    from = "2009-01-01",
    to   = "2018-12-31"
)

# oil_prices_tbl %>% write_csv("data/oil_prices.csv")

oil_prices_d_tbl <- read_csv("data/oil_prices.csv")

oil_prices_d_tbl %>%
    plot_time_series(
        date, price,
        .y_intercept = 0
    )

# Convert to yearly
oil_prices_y_tbl <- oil_prices_d_tbl %>%
    summarise_by_time(
        date,
        .by = "year",
        price_oil = mean(price, na.rm = TRUE)
    ) %>%
    mutate(
        category_oil_shock = case_when(
            year(date) <= 2014 ~ "pre oil shock",
            year(date) %in% c(2015, 2016) ~ "oil_shock",
            year(date) > 2016 ~ "post oil shock"
        )
    ) %>%
    mutate(category_oil_shock = as_factor(category_oil_shock))

oil_prices_y_tbl

# * IMPORTANT: Check correlation to oil prices ----

cashflow_oil_correlation_tbl <- cashflow_inputs_long_tbl %>%
    select(item_id, item_name, date, value) %>%
    left_join(oil_prices_y_tbl) %>%
    group_by(item_id, item_name) %>%
    summarise(
        correlation_to_oil_prices = cor(value, price_oil),
        median_item_value = median(value),
        median_oil_price = median(price_oil)
    ) %>%
    ungroup() %>%
    arrange(-abs(correlation_to_oil_prices))

cashflow_oil_correlation_tbl

cashflow_oil_correlation_tbl %>%
    gt() %>%
    tab_header(
        title = "Correlation of Cash Flow Items to Oil Prices",
        subtitle = "Exxon Mobil (Cash Flow Items FY2009 - FY2018)"
    ) %>%
    cols_label(
        item_id = "Item ID",
        item_name = "Item Name",
        correlation_to_oil_prices = "Correlation to Oil Prices",
        median_item_value = "Median Item Value",
        median_oil_price = "Median Oil Price"
    ) %>%
    fmt_number(
        correlation_to_oil_prices,
        decimals = 2
    ) %>%
    fmt_currency(
        column = "median_item_value",
        decimals = 0
    ) %>%
    fmt_currency(
        column = "median_oil_price",
        decimals = 2
    ) %>%
    cols_align(align = "center") %>%
    data_color(
        columns = "correlation_to_oil_prices",
        colors = scales::col_numeric(
            palette = colorRampPalette(colors = c("red", "white", "blue"))(16),
            domain = NULL
        )
    )

# * Future Frame Oil Price ----

oil_future_y_tbl <- oil_prices_y_tbl %>%
    future_frame(.length_out = 3) %>%
    mutate(category_oil_shock = "post oil shock")

oil_future_y_tbl

# 4.0 SAVE CHECKPOINT ARTIFACTS ----



# 5.0 FORECAST: OIL PRICE ----

# train/test ----

splits <- time_series_split(
    oil_prices_y_tbl,
    assess     = 3,
    cumulative = TRUE
)


splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, price_oil)


# * arima ----

recipe_simple <- recipe(price_oil ~ ., data = training(splits) )

model_arima <- arima_reg(seasonal_period = 1) %>%
    set_engine("auto_arima")

wflw_oil_arima <- workflow() %>%
    add_model(model_arima) %>%
    add_recipe(recipe_simple) %>%
    fit(training(splits))

# * ets ----

model_ets <-exp_smoothing(seasonal_period = 1) %>%
    set_engine("ets")

wflw_oil_ets <- workflow() %>%
    add_model(model_ets) %>%
    add_recipe(recipe_simple) %>%
    fit(training(splits))

# * prophet ----

recipe_simple <- recipe(price_oil ~ ., data = training(splits) )

model_prophet <- prophet_reg() %>%
    set_engine("prophet")

wflw_oil_prophet <- workflow() %>%
    add_model(model_prophet) %>%
    add_recipe(recipe_simple) %>%
    fit(training(splits))

# * xgboost ----

recipe_ml <- recipe(price_oil ~ ., data = training(splits)) %>%
    step_timeseries_signature(date) %>%
    step_select(price_oil,
                category_oil_shock,
                date_index.num, date_year) %>%
    step_dummy(category_oil_shock, one_hot = TRUE)

recipe_ml %>% prep() %>% juice()

model_xgb <- boost_tree(mode = "regression") %>%
    set_engine("xgboost")

wflw_oil_xgb <- workflow() %>%
    add_model(model_xgb) %>%
    add_recipe(recipe_ml) %>%
    fit(training(splits))

# * glm ----

model_glm <- linear_reg(penalty = 0.1) %>%
    set_engine("glmnet")

wflw_oil_glm <- workflow() %>%
    add_model(model_glm) %>%
    add_recipe(recipe_ml) %>%
    fit(training(splits))


# * Modeltime Submodels ----

oil_submodels_tbl <- modeltime_table(
    wflw_oil_arima,
    wflw_oil_ets,
    wflw_oil_prophet,
    wflw_oil_xgb,
    wflw_oil_glm
) %>%
    modeltime_calibrate(testing(splits))

oil_submodels_tbl %>% modeltime_accuracy()

oil_submodel_forecast_tbl <- oil_models_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = training(splits)
    )

oil_submodel_forecast_tbl %>%
    plot_modeltime_forecast(
        .title = "Oil Price Forecast: 2019-2021"
    )


# * Modeltime Ensemble ----

oil_model_ensemble_tbl <- oil_submodels_tbl %>%
    slice(-3) %>%
    ensemble_average(type = "mean") %>%
    modeltime_table() %>%
    modeltime_calibrate(testing(splits))


oil_ensemble_forecast_tbl <- oil_model_ensemble_tbl %>%
    modeltime_forecast(
        new_data      = testing(splits),
        actual_data   = training(splits),
        conf_interval = 0.90
    )

oil_ensemble_forecast_tbl

oil_ensemble_forecast_tbl %>%
    plot_modeltime_forecast(
        .title = "Oil Price Forecast: 2019-2021"
    )

# * Refit ----

oil_ensemble_refit_tbl <- oil_model_ensemble_tbl %>%
    modeltime_refit(oil_prices_y_tbl)

oil_final_forecast_tbl <- oil_ensemble_refit_tbl %>%
    modeltime_forecast(
        new_data      = oil_future_y_tbl,
        actual_data   = oil_prices_y_tbl,
        conf_interval = 0.90
    )

oil_final_forecast_tbl %>%
    plot_modeltime_forecast(
        .title = "Oil Price Forecast: 2019-2021"
    )

oil_final_forecast_formatted_tbl <- oil_final_forecast_tbl %>%
    select(.index, .value, .key, .conf_lo, .conf_hi) %>%
    set_names(c('date', 'oil_price', 'estimate_type', 'oil_price_l90', 'oil_price_h90'))

oil_final_forecast_formatted_tbl

oil_final_forecast_formatted_tbl


# 6.0 FORECAST: EXXON CASHFLOW 3-YEAR PRO-FORMA ----

# * Data Preparation ----
extended_data_tbl <- cashflow_inputs_long_tbl %>%

    # remove unnecessary column
    select(-fiscal_year_id, -group_id, -item_name, -item_type) %>%
    mutate(item_id = as_factor(item_id)) %>%

    # extend into the future every time series by 3 years
    group_by(item_id) %>%
    future_frame(date, .length_out = "3 years", .bind_data = TRUE) %>%
    ungroup() %>%

    # join oil prices
    left_join(
        oil_forecast_formatted_tbl %>% select(-estimate_type)
    ) %>%
    select(item_id, date, value, oil_price)

extended_data_tbl %>% tail(12)

data_prepared_tbl <- extended_data_tbl %>% filter(!is.na(value))

future_data_tbl <- extended_data_tbl %>% filter(is.na(value))



# * Train/Test ----

splits <- time_series_split(
    data_prepared_tbl,
    assess     = 3,
    cumulative = TRUE
)


splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, value)

# * Modeling ----



# ZZZ - OLD ----



splits <- time_series_split(
    oil_prices_y_tbl,
    assess     = 3,
    cumulative = TRUE
)


splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, price_oil)





# * recipes ----

recipe_arima <- recipe(price_oil ~ ., data = training(splits))

recipe_ml <- recipe(price_oil ~ ., data = training(splits)) %>%
    step_timeseries_signature(date) %>%
    step_select(price_oil,
                # flag_shock_oil,
                date_index.num, date_year)

recipe_ml %>% prep() %>% juice() %>% glimpse()

# * arima ----

model_arima <- arima_reg() %>%
    set_engine("auto_arima")

wflw_arima <- workflow() %>%
    add_model(model_arima) %>%
    add_recipe(recipe_arima) %>%
    fit(training(splits))

# * ets ----

model_ets <- exp_smoothing() %>%
    set_engine("ets")

wflw_ets <- workflow() %>%
    add_model(model_ets) %>%
    add_recipe(recipe_arima) %>%
    fit(training(splits))

# * xgboost ----

model_xgb <- boost_tree(mode = "regression") %>%
    set_engine("xgboost")

wflw_xgb <- workflow() %>%
    add_model(model_xgb) %>%
    add_recipe(recipe_ml) %>%
    fit(training(splits))

# * glmnet ----

model_glm <- linear_reg(mode = "regression", penalty = 0.1) %>%
    set_engine("glmnet")

wflw_glm <- workflow() %>%
    add_model(model_glm) %>%
    add_recipe(recipe_ml) %>%
    fit(training(splits))





# * Modeltime Workflow ----

calibration_tbl <- modeltime_table(
    wflw_arima,
    wflw_ets,
    wflw_xgb,
    wflw_glm
) %>%
    modeltime_calibrate(testing(splits))

calibration_tbl %>%
    modeltime_accuracy()

calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = oil_prices_y_tbl
    ) %>%
    plot_modeltime_forecast()
