



# LIBRARIES ----

library(tidyverse)
library(tidyquant)
library(timetk)
library(plotly)
library(extraDistr)

# ADSPEND PROFILE ----

create_adspend_profile <- function(
    start_date       = "2021-10-01",
    average_roas     = 3.5,
    n_months_total   = 24,
    n_months_learn   = 5,
    n_months_scale   = 3,
    adspend_learn    = 3000,
    adspend_scale    = 30000
) {
    
    # INPUTS ----
    
    START_DATE     <- start_date
    AVERAGE_ROAS   <- average_roas
    N_MONTHS_LEARN <- n_months_learn
    N_MONTHS_SCALE <- n_months_scale
    N_MONTHS_TOTAL <- n_months_total
    
    ADSPEND_LEARN  <- adspend_learn
    ADSPEND_SCALE  <- adspend_scale
    
    # CREATE VECTORS FROM INPUTS ----
    
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
    
    # REVENUE PROFILE ----
    
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
    
    
    # COST PROFILE ----
    
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

    
    # PROFIT PROFILE ----
    
    # * Tidy Profit Analysis ----
    
    profit_profile_tbl <- cost_profile_tbl %>%
        mutate(profit = revenue - cost_total) %>%
        mutate(profit_cum = cumsum(profit))
    
    return(profit_profile_tbl)
    
}

# PROJECT SUMMARY ----

summarize_adspend_project <- function(adspend_profile, annual_discount_rate = 0.10) {
    
    ANNUAL_DISCOUNT_RATE <- annual_discount_rate
    
    adspend_profile %>%
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
}

# PLOTTING UTILITIES ----

plot_revenue_profit <- function(adspend_profile, .interactive = TRUE) {
    
    adspend_profile %>%
        select(date, revenue, profit) %>%
        pivot_longer(cols = revenue:profit) %>%
        plot_time_series(
            date, value, 
            .color_var = name, 
            .smooth = FALSE,
            .title = "Revenue & Profit Margin", 
            .y_intercept = 0,
            .interactive = .interactive
        )
}

plot_cum_profit <- function(adspend_profile, .interactive = TRUE) {
    
    adspend_profile %>%
        select(date, profit_cum) %>%
        pivot_longer(cols = profit_cum) %>%
        plot_time_series(
            date, value, 
            .color_var = name, 
            .smooth = FALSE,
            .title = "Cumulative Profit",
            .y_intercept = 0,
            .interactive = .interactive
        )
}

plot_profit_breakdown <- function(adspend_profile, alpha = 0.7, .interactive = TRUE) {
    
    g <- adspend_profile %>%
        select(date, profit, cost_fb_adspend, cost_consultant_1, cost_consultant_2, cost_platform) %>%
        pivot_longer(cols = -date) %>%
        mutate(name = as_factor(name)) %>%
        ggplot(aes(date, value, fill = name, color = name)) +
        geom_area(alpha = alpha) +
        # geom_area(fill = "#FFFFFF00") +
        scale_fill_tq() +
        scale_color_tq() +
        theme_tq() +
        labs(title = "Cumulative Profit" , x = NULL, y = NULL)
    
    if (.interactive) {
        ggplotly(g)
    } else {
        g
    }
    
    
    
}

