# 1.0 LIBRARIES ----

# API - ACCESS GA
library(googleAnalyticsR)

# DATABASE - SAVE GA
library(DBI)
library(RSQLite)

#Visualization
library(plotly)

# Core
library(tidyverse)
library(tidyquant)
library(lubridate)



# 2.0 GETTING DATA ----
# - Resource: https://code.markedmondson.me/googleAnalyticsR/

# Authentication
ga_auth()

# Setup (for reproducibility)
end_date     <- ymd("2020-03-24")
start_date   <- end_date - 30

# Access GA's API
sessions_tbl = google_analytics(
    viewId = config::get("googleanalytics-gtm-view-1"),	# replace this with your view ID
    
    # Date Range
    date_range = c(start_date, end_date),
    
    # Measuring
    metrics = "sessions",
    dimensions = "date"
) %>% as_tibble()

sessions_tbl



# 3.0 DIMENSIONS & METRICS -----
# - Resource: https://ga-dev-tools.appspot.com/dimensions-metrics-explorer/

ga_sessions_users_transactions_tbl = google_analytics(
    viewId = config::get("googleanalytics-gtm-view-1"), # replace this with your view ID
    date_range = c(today() - 30, today()),
    
    # Metrics (Values)
    metrics = c("sessions", "users", "transactions"),
    
    # Dimensions (Segments)
    dimensions = c("date"),
    
    # Anti-sample & Max - Get the full data with multiple API calls 
    anti_sample = TRUE, 
    max = -1
) %>%
    as_tibble()

ga_sessions_users_transactions_tbl



# 4.0 VISUALIZATION ----
# - dplyr, ggplot2 + plotly (101)

ga_sessions_users_transactions_tidy_tbl <- ga_sessions_users_transactions_tbl %>%
    pivot_longer(cols = sessions:transactions) %>%
    arrange(date) %>%
    group_by(name) %>%
    mutate(growth = CUMULATIVE_SUM(value)) %>%
    ungroup() %>%
    arrange(name, date)

ga_sessions_users_transactions_tidy_tbl

# 4.1 Visualize Counts ----
g <- ga_sessions_users_transactions_tidy_tbl %>%
    ggplot(aes(date, value, color = name)) +
    geom_line(size = 1) +
    facet_wrap(~ name, scales = "free_y", ncol = 1) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Google Analytics - Sessions, Transactions, Users over Time")

g

ggplotly(g)


# 4.2 Visualize Growth ----
g <- ga_sessions_users_transactions_tidy_tbl %>%
    ggplot(aes(date, growth, color = name)) +
    geom_line(size = 1) +
    facet_wrap(~ name, scales = "free_y", ncol = 1) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Google Analytics - Sessions, Transactions, Users over Time")

g

ggplotly(g)



# 5.0 DATABASE ----
# - Learning Labs 21, 22, & 23 (SQL Series Labs)

# 5.1 Access Page Path Data ----
ga_page_views_tbl = google_analytics(
    viewId = config::get("googleanalytics-gtm-view-1"), # replace this with your view ID
    date_range = c("2019-09-01", "2020-03-16"),
    
    # Metrics (Values)
    metrics = c("pageViews", "organicSearches"),
    
    # Dimensions (Segments)
    dimensions = c("pagePath", "date"),
    
    # Anti-sample & Max - Get the full data with multiple API calls 
    anti_sample = TRUE, 
    max = -1
) %>%
    as_tibble()

ga_page_views_tbl

# 5.2 Write Data to SQLite ----

con <- dbConnect(SQLite(), "data/google_analytics_2.sqlite")

dbListTables(con)

dbWriteTable(con, "ga_page_views", ga_page_views_tbl)

dbListTables(con)

# 5.3 Dplyr Database Backend ----
# - Lab 21, 22, & 23 (SQL Series) 

tbl(con, "ga_page_views")

tbl(con, "ga_page_views") %>% collect()

