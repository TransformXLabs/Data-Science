library(tidyverse)
library(tidyquant)
library(timetk)
library(extraDistr)
library(furrr)
library(correlationfunnel)
library(gt)
library(plotly)


extraDistr::rtriang(n = 10000, a = -1, b = 1, c = 0.5) %>% hist()


# INPUTS ----

START_DATE     <- "2021-10-01"

AVERAGE_ROAS   <- 3.5

N_MONTHS_LEARN <- 5
N_MONTHS_SCALE <- 3
N_MONTHS_TOTAL <- 24

ADSPEND_LEARN  <- 3000
ADSPEND_SCALE  <- 30000

ANNUAL_DISCOUNT_RATE <- 0.10

# 1.0 CREATE VECTORS FROM INPUTS ----

# * Periods and ROAS ----
period_vec <- 1:N_MONTHS_TOTAL

# * Month ----

month_vec <- tk_make_timeseries(
    start_date = START_DATE, 
    by         = "month", 
    length_out = N_MONTHS_TOTAL
)

# * ROAS Profile ----

roas_vec <- c(
    rep(0, N_MONTHS_LEARN),
    rep(AVERAGE_ROAS, N_MONTHS_SCALE),
    rep(AVERAGE_ROAS, N_MONTHS_TOTAL - N_MONTHS_LEARN - N_MONTHS_SCALE)
)

# * FB Adspend ----

# ** Interpolation ----
approx(
    x      = 1:4,
    y      = c(3000, NA, NA, 30000),
    xout   = 2:4,
    method = "linear"
)

# ** Apply interpolation ----

interpolation_vec <- approx(
    x      = 1:(N_MONTHS_SCALE + 1),
    y      = c(ADSPEND_LEARN, rep(NA, N_MONTHS_SCALE - 1), ADSPEND_SCALE),
    xout   = 2:(N_MONTHS_SCALE+1),
    method = "linear"
) %>%
    pluck("y")

fb_adspend_vec <- c(
    rep(ADSPEND_LEARN, N_MONTHS_LEARN),
    interpolation_vec,
    rep(ADSPEND_SCALE, N_MONTHS_TOTAL - N_MONTHS_LEARN - N_MONTHS_SCALE)
)

# 2.0 REVENUE PROFILE ----

# * Revenue Profile ----

revenue_profile_tbl <- tibble(
    period          = period_vec,
    date            = month_vec,
    roas            = roas_vec,
    fb_adspend      = fb_adspend_vec
) %>% 
    mutate(revenue = fb_adspend * roas)

# * Visualize the Revenue Profile ----
revenue_profile_tbl %>%
    plot_time_series(period, revenue, .smooth = F)


# 3.0 COST PROFILE ----

# * Tidy Cost Analysis ---- 
cost_profile_tbl <- revenue_profile_tbl %>%
    
    # Adspend Cost
    mutate(cost_fb_adspend = fb_adspend) %>%
    
    # Consultant 1: Earns 10% of revenue *or* $3000/month
    mutate(
        cost_consultant_1 = pmax(
            rep(3000, length(period)),
            0.10 * revenue
        )
    ) %>%
    
    # Consultant 2: Earns 5% of revenue *and* $3500/month
    mutate(
        cost_consultant_2 = 3500 + 0.05*revenue
    ) %>%
    
    # Platform Costs: 8% of Revenue
    mutate(cost_platform = 0.08 * revenue) %>%
    
    # Total Project Cost
    mutate(cost_total = cost_fb_adspend + cost_consultant_1 + cost_consultant_2 + cost_platform)

cost_profile_tbl

# 4.0 PROFIT PROFILE ----

# * Tidy Profit Analysis ----

profit_profile_tbl <- cost_profile_tbl %>%
    mutate(profit = revenue - cost_total) %>%
    mutate(profit_cum = cumsum(profit))

profit_profile_tbl %>% glimpse()
    
# * Project Summaries ----

?tidyquant::NPV

project_summary_tbl <- profit_profile_tbl %>%
    summarise(
        estimated_adspend = sum(fb_adspend),
        estimated_revenue = sum(revenue),
        estimated_profit  = sum(profit),
        npv               = NPV(profit, rate = ANNUAL_DISCOUNT_RATE / 12)
    ) %>%
    mutate(
        roas          = estimated_revenue / estimated_adspend,
        profit_margin = estimated_profit / estimated_revenue,
        discount_rate = ANNUAL_DISCOUNT_RATE
    )

# 5.0 CONVERT TO FUNCTIONS ----

source("functions/adspend_profile.R")

# * Adspend Profile ----
adspend_profile_tbl <- create_adspend_profile(
    start_date       = "2021-10-01",
    average_roas     = 3.5,
    n_months_total   = 24,
    n_months_learn   = 5,
    n_months_scale   = 3,
    adspend_learn    = 3000,
    adspend_scale    = 30000
)

# * Project Summary ----

adspend_profile_tbl %>% summarize_adspend_project()

# * Visualizations ----

adspend_profile_tbl %>% plot_revenue_profit() 

adspend_profile_tbl %>% plot_cum_profit()

adspend_profile_tbl %>% plot_profit_breakdown()

# 6.0 RISK ANALYSIS -----

# * Intro to Distributions ----

# ** Normal Distribution ----
set.seed(123)
tibble(
    roas = rnorm(n = 10000, mean = 3.5, sd = 1)
) %>%
    ggplot(aes(roas)) + 
    geom_histogram(fill = "#2c3e50") +
    theme_tq()

# ** Uniform ----
set.seed(123)
tibble(
    roas = runif(n = 10000, min = 1, max = 5)
) %>%
    ggplot(aes(roas)) + 
    geom_histogram(fill = "#2c3e50") +
    theme_tq()

# ** Triangular Distribution ----
set.seed(123)
tibble(
    roas =  extraDistr::rtriang(n = 10000, a = 1, b = 6, c = 3.5)
) %>%
    ggplot(aes(roas)) + 
    geom_histogram(fill = "#2c3e50") +
    theme_tq()

# ** Discrete Distributions ----

set.seed(123)
tibble(
    n_months_learn =  extraDistr::rtriang(n = 10000, a = 3, b = 6, c = 5) %>% round()
) %>%
    ggplot(aes(n_months_learn)) + 
    geom_histogram(fill = "#2c3e50") +
    theme_tq()

# * Simulation ----

N_SIMULATIONS <- 100

set.seed(123)
simulation_grid_tbl <- tibble(
    average_roas    = rtriang(n = N_SIMULATIONS, a = 1, b = 5, c = 3.5),
    n_months_learn  = rtriang(n = N_SIMULATIONS, a = 3, b = 6, c = 5) %>% round(),
    n_months_scale  = rtriang(n = N_SIMULATIONS, a = 1, b = 3, c = 3) %>% round()
)

plan(multisession, workers = 12)

t0 <- Sys.time()
simulation_results_tbl <- simulation_grid_tbl %>%
    mutate(adspend_results = future_pmap(
        .l = list(average_roas, n_months_learn, n_months_scale), 
        .f = function(average_roas, n_months_learn, n_months_scale) {
            
            adspend_profile_tbl <- create_adspend_profile(
                start_date       = "2021-10-01",
                average_roas     = average_roas,
                n_months_total   = 24,
                n_months_learn   = n_months_learn,
                n_months_scale   = n_months_scale,
                adspend_learn    = 3000,
                adspend_scale    = 30000
            )
            
            ret <- adspend_profile_tbl %>% summarize_adspend_project()
            
            return(ret)
        
    }))
t1 <- Sys.time()
t1-t0

simulation_results_unnested_tbl <- simulation_results_tbl %>% 
    unnest(adspend_results)

# * Evaluate Simulation Results ----

simulation_results_unnested_tbl

g <- simulation_results_unnested_tbl %>%
    ggplot(aes(npv)) +
    geom_histogram(fill = "#2C3E50", color = "white") +
    geom_vline(aes(xintercept = median(npv)), color = "red", size = 1, linetype = 2) +
    geom_vline(aes(xintercept = quantile(npv, 0.05)), color = "blue", size = 1, linetype = 2) +
    geom_vline(aes(xintercept = quantile(npv, 0.95)), color = "blue", size = 1, linetype = 2) +
    theme_tq()

ggplotly(g)

probs <- c(0.05, 0.5, 0.95)
simulation_results_unnested_tbl %>%
    summarise(
        q = probs,
        x = quantile(npv, probs = probs)
    )



# * Feature Importance -----

simulation_results_unnested_tbl %>%
    select(npv, average_roas, n_months_learn, n_months_scale) %>%
    binarize() %>%
    glimpse() %>%
    correlate(2) %>%
    plot_correlation_funnel()

# 7.0 SHINY APP ----

adspend_wide_tbl <- adspend_profile_tbl %>%
    pivot_longer(cols = -c(date)) %>%
    pivot_wider(id_cols = name, names_from = c(date), values_from = value) %>%
    mutate(name = str_replace_all(name, "_", " ") %>% str_to_title()) %>%
    mutate(name = ifelse(name == "Roas", "ROAS", name)) %>%
    mutate(name = str_replace(name, "Fb", "FB")) %>%
    mutate(group = case_when(
        str_detect(name, "Period") ~ "0-Info",
        str_detect(name, "^Cost") ~ "2-Costs",
        str_detect(name, "^Profit") ~ "3-Profit",
        TRUE ~ "1-Revenue"
        
    )) %>%
    select(group, everything()) 

adspend_wide_tbl %>%
    group_by(group) %>%
    gt(rowname_col = "name") %>%
    tab_spanner(
        label = "Learn",
        id = "learn",
        columns = 2:(N_MONTHS_LEARN+2)
    ) %>%
    tab_spanner(
        label = "Scale",
        id = "scale",
        columns = (N_MONTHS_LEARN+3):(N_MONTHS_LEARN+N_MONTHS_SCALE+2)
    ) %>%
    tab_spanner(
        label = "Automate",
        id = "automate",
        columns = (N_MONTHS_LEARN+N_MONTHS_SCALE+3):(N_MONTHS_TOTAL+2)
    ) %>%
    fmt_currency(
        columns = everything(), 
        rows    = c(3, 4:11), 
        decimals = 0,
        accounting = TRUE
    ) %>%
    tab_style(
        style = cell_fill(color = "red", alpha = 0.9),
        locations = cells_column_spanners("learn")
    ) %>%
    tab_style(
        style = cell_fill(color = "yellow", alpha = 0.9),
        locations = cells_column_spanners("scale")
    ) %>%
    tab_style(
        style = cell_fill(color = "green", alpha = 0.9),
        locations = cells_column_spanners("automate")
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
    ) %>%
    tab_options(
        column_labels.font.size = px(10),
        column_labels.font.weight = "bold",
        table.font.size = px(12L)
    )