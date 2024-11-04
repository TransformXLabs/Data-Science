# 1.0 LIBRARIES ----

# Database
library(DBI)
library(RSQLite)

# Time Series Forecasting
library(tsibble)
library(fable)
# remotes::install_github("mitchelloharawild/fable.prophet")
library(fable.prophet)

#Visualization
library(plotly)

# Core
library(tidyverse)
library(tidyquant)
library(lubridate)

# 2.0 READ SQL ----

con <- dbConnect(SQLite(), "data/google_analytics.sqlite")

DBI::dbListTables(con)

ga_page_views_tbl <- tbl(con, "ga_page_views") %>% collect()

dbDisconnect(con)

# Issue - SQLite doesn't have a "Date" class (gets converted to numeric)
# - Don't freak out! Everything will be OK.

ga_page_views_tbl <- ga_page_views_tbl %>%
    mutate(date = as_date(date))


# 3.0 DATA PREP ----
# - dplyr (101)

ga_page_views_tbl %>% distinct(pagePath)

# 3.1 Top 10 Pages by Total Search & Organic Search ----

ga_page_views_summary_tbl <- ga_page_views_tbl %>%
    group_by(pagePath) %>%
    summarize(
        pageViews = SUM(pageViews),
        organicSearches = SUM(organicSearches)
    ) 

top_10_page_views_summary_tbl <- ga_page_views_summary_tbl %>%
    arrange(desc(pageViews)) %>%
    slice(1:10)

top_10_page_views_summary_tbl

top_10_organic_searches_summary_tbl <- ga_page_views_summary_tbl %>%
    arrange(desc(organicSearches)) %>%
    slice(1:10)

top_10_organic_searches_summary_tbl


# 3.2 Pare Down Page Path Options ----

ga_page_views_tidy_tbl <- ga_page_views_tbl %>%
    filter(pagePath %in% c(
        top_10_organic_searches_summary_tbl %>% pull(pagePath),
        top_10_page_views_summary_tbl %>% pull(pagePath)
    )) %>%
    pivot_longer(cols = pageViews:organicSearches, names_to = "search_type") %>%
    select(date, everything()) %>%
    arrange(date, pagePath, search_type)

ga_page_views_tidy_tbl



# 4.0 PROPHET & FABLE ----

# 4.1 Setup ---- 
page_path_options   <- ga_page_views_tidy_tbl %>% pull(pagePath) %>% unique()
page_path_selection <- page_path_options[1]
forecast_horizon    <- 30

# 4.2 Convert to TSibble ----
ga_page_views_filtered_tsbl <- ga_page_views_tidy_tbl %>%
    filter(pagePath == page_path_selection) %>%
    as_tsibble(
        key   = c(pagePath, search_type), 
        index = date
    )

# 4.3 Prophet Model ----
page_views_mable <- ga_page_views_filtered_tsbl %>%
    model(
        PROPHET = prophet(log(value) ~ season(period = "week"))
    )

page_views_mable

# 4.4 Prophet Forecast ----
g <- page_views_mable %>%
    forecast(h = forecast_horizon) %>%
    
    autoplot(ga_page_views_filtered_tsbl) +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Prophet Model: With Weekly Seasonality",
        x = "", y = "Traffic (Users)"
    ) 

g

# Wrong plot!! (Don't freak out)
ggplotly(g)


# 4.5 Plotly Visualization ----

train_tbl <- ga_page_views_filtered_tsbl %>% as_tibble()

page_views_fable <- page_views_mable %>% forecast(h = forecast_horizon)

# Hilo Object
hl <- page_views_fable %>%
    mutate(interval_95 = hilo(value, 95)) %>%
    slice(1) %>%
    pull(interval_95) 

hl

# Purrr Plucking!
future_tbl <- page_views_fable %>%
    mutate(
        lo_95 = quantile(value, 0.025),
        hi_95 = quantile(value, 0.975)
    ) %>%
    as_tibble()

# Plotly DataViz
g <- bind_rows(
    train_tbl %>% mutate(.mean = value) %>% select(-value), 
    future_tbl
) %>%
    mutate(.model = ifelse(is.na(.model), "actual", .model) %>% as_factor()) %>%
    mutate(search_type = as.factor(search_type) %>% fct_rev()) %>%
    ggplot(aes(date, .mean, color = .model)) +
    geom_ribbon(aes(ymin = lo_95, ymax = hi_95), alpha = 0.5, color = "white", fill = "dodgerblue") +
    geom_line() +
    facet_wrap(~ pagePath + search_type, ncol = 1, scales = "free_y") +
    theme_tq() +
    scale_color_tq() +
    labs(x = "", y = "")

ggplotly(g)
