library(shiny)
library(shinythemes)
library(plotly)
library(shinycssloaders)

# Finance
library(tidyquant)

library(timetk)    
library(lubridate)

# Preprocessing
library(recipes)

# Accuracy
library(yardstick)

# Core
library(tidyverse)
library(janitor)

# Python
library(reticulate)
virtualenv_dir <- "tensorflow"  
virtualenv_create(envname = virtualenv_dir)
virtualenv_install(envname = virtualenv_dir, packages = c("scikit-learn", "pandas", "numpy","matplotlib","tensorflow==2.12.0"))
use_virtualenv(virtualenv_dir, required = TRUE)


# PYTHON SETUP ----

#use_condaenv("py3.8", required = TRUE)

# Source Python Sklearn Functions
source_python("py/00_imports.py")
source_python("py/01_prepare_data.py")
source_python("py/02_tensorflow_model.py")
source_python("py/03_model_prediction.py")

# APP SETUP ----

lstm_activation_options <- c("tanh", "relu", "sigmoid", "softmax", "softplus", "softsign", "selu")
optimizer_options       <- c("rmsprop", "sgd","adam", "adadelta", "adagrad", "adamax", "nadam", "ftrl") 
loss_options            <- c("mae", "mse", "mape", "msle")

look_back <- 12
seed      <- 5

init_epochs     <- 10
init_batch_size <- 80
init_units_1    <- 64 
init_units_2    <- 12

max_epochs     <- 1000
max_batch_size <- 120
max_lstm_units <- 120

# Used for value box colors
mae_upper_limit  <- 90
rmse_upper_limit <- 90
mape_upper_limit <- 4

# INFO CARD ----
info_card <- function(title, value, sub_value = NULL,
                      main_icon = "chart-line", sub_icon = "arrow-up",
                      bg_color = "default", text_color = "default", 
                      sub_text_color = "success") {
    
    div(
        class = "panel panel-default",
        style = "padding: 0px;",
        div(
            class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
            p(class = "pull-right", icon(class = "fa-4x", main_icon)),
            h4(title),
            h5(value),
            p(
                class = str_glue("text-{sub_text_color}"),
                icon(sub_icon),
                tags$small(sub_value)
            )
        )
    )
    
}

# ---- 1.0 UI ----
ui <- navbarPage(
    title = "TensorFlow LSTM - Gold Price Forecasting",
    collapsible = TRUE,
    inverse     = TRUE, 
    theme       = shinytheme("superhero"),
    
    shiny::tabPanel(
        title = "",
        sidebarLayout(
            # 1.1 Sidebar ----
            sidebarPanel(
                width = 3,
                h3("Time Series Forecast Explorer"),
                # h3("TensorFlow LSTM Parameters"),
                HTML("<p>Adjust the hyperparameters and see their affect on <strong><em>model performance.</em></strong></p>"),
                hr(),
                h4("Model Training"),
                shiny::sliderInput(inputId = "epochs", label = "Epochs", 
                                   value = init_epochs, min = 1, max = max_epochs, step = 5),
                shiny::sliderInput(inputId = "batch_size", label = "Batch Size", 
                                   value = init_batch_size, min = 1, max = max_batch_size),
                hr(),
                h4("LSTM Layer 1"),
                shiny::sliderInput(inputId = "units_1", label = "Units", 
                                   value = init_units_1, min = 1, max = max_lstm_units),
                shiny::selectInput(inputId = "activation_1", label = "Activation", 
                                   choices = lstm_activation_options, selected = "tanh"),
                hr(),
                h4("LSTM Layer 2"),
                shiny::sliderInput(inputId = "units_2", label = "Units", 
                                   value = init_units_2, min = 1, max = max_lstm_units),
                shiny::selectInput(inputId = "activation_2", label = "Activation", 
                                   choices = lstm_activation_options, selected = "relu"),
                hr(),
                h4("Optimizer & Loss"),
                shiny::selectInput(inputId = "optimizer", label = "Optimzer", 
                                   choices = optimizer_options, selected = "rmsprop"),
                shiny::selectInput(inputId = "loss", label = "Loss Function", 
                                   choices = loss_options, selected = "mae"),
                hr(),
                shiny::actionButton(inputId = "submit", "Submit", class = "btn-primary")
            ),
            
            # 1.2 Main Panel ----
            mainPanel(
                width = 9,
                div(
                    class = "col-sm-9",
                    HTML("<h1>Forecast Gold Spot Prices<br><small>TensorFlow LSTM Time Series Forecast</small></h1>"),
                    tabsetPanel(
                        # 1.2.1.1 Forecast ----
                        tabPanel(
                            "Forecast Exploration",
                            div(
                                class = "row",
                                div(
                                    class = "col-sm-12 panel",
                                    div(class = "panel-heading", h5("Forecast")),
                                    div(class = "panel-body",
                                        plotlyOutput("plotly_forecast", height = "600px") %>%
                                            shinycssloaders::withSpinner()
                                        # plotlyOutput("plotly_residuals", width = "600px", height = "600px")
                                    )
                                )
                            )
                        ), 
                        
                        tabPanel(
                            "Model Residual Diagnostics",
                            div(
                                class = "row",
                                div(
                                    class = "col-sm-12 panel",
                                    div(class = "panel-heading", h5("Model Residuals")),
                                    div(
                                        class = "panel-body", 
                                        plotlyOutput("plotly_residuals", width = "600px", height = "600px") %>%
                                            shinycssloaders::withSpinner()
                                    )
                                )
                            )
                        )
                        # ,
                        # tabPanel(
                        #     "Model Information",
                        #     div(
                        #         class = "row",
                        #         div(
                        #             class = "col-sm-12 panel",
                        #             div(class = "panel-heading", h5("Model Summary")),
                        #             div(
                        #                 class = "panel-body", 
                        #                 # Used for debugging
                        #                 verbatimTextOutput("code")
                        #             )
                        #         )
                        #     )
                        # )
                    )
                ),
                # 1.2.2 Metrics ----
                div(
                    class = "col-sm-3",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h5("Model Metrics")),
                        div(class = "panel-body",
                            uiOutput("value_boxes")
                        )
                    )
                ) 
            )
        )
    )
)

# ---- 2.0 SERVER ----
server <- function(session, input, output) {
    
    # 2.1 Setup Reactive Values ----
    rv <- reactiveValues()
    
    observeEvent(input$submit, {
        
        # Collect data
        rv$data <- read_rds("data/gold_monthly_tbl.rds")
        
        rv$train_tbl <- rv$data %>% filter_by_time(date, "start", "2010")
        rv$test_tbl  <- rv$data %>% filter_by_time(date, "2011", "2011")

        # Preprocess Data
        recipe_spec <- recipe(~ ., data = rv$train_tbl) %>%
            step_mutate(pct_change =  (gold_price - lag(gold_price)) / lag(gold_price)) %>%
            step_normalize(contains("pct_change")) %>%
            step_impute_mean(pct_change)
        
        rv$recipe_spec_prep    <- recipe_spec %>% prep() 
        rv$train_processed_tbl <- rv$recipe_spec_prep %>% juice()
        # rv$test_processed_tbl  <- recipe_spec_prep %>% bake(rv$test_tbl)
        
        # New Data (Future Data)
        rv$new_data <- rv$train_processed_tbl %>%
            future_frame(.length_out = look_back) %>%
            bind_rows(rv$train_processed_tbl, .) %>%
            replace_na(replace = list(pct_change = 0))
        
        # LSTM Model
        training_data <- rv$new_data %>% drop_na()
        data_prepared_list <- prepare_data(
            training_data, 
            column = "pct_change", 
            look_back = as.integer(look_back)
        )
        
        X = data_prepared_list[[1]]
        y = data_prepared_list[[2]]
        
        tensorflow_lstm(
            X, y, 
            look_back    = as.integer(look_back),
            seed         = 5,
            
            # Model Fit
            epochs       = as.integer(input$epochs), 
            batch_size   = as.integer(input$batch_size),
            
            # LSTM 1
            units_1      = as.integer(input$units_1),
            activation_1 = as.character(input$activation_1), 
            
            # LSTM 2
            units_2      = as.integer(input$units_2),
            activation_2 = as.character(input$activation_2),
            
            # Optimizer
            optimizer    = as.character(input$optimizer),
            loss         = as.character(input$loss)
        )
        
        new_data_prepared_list <- prepare_data(
            rv$new_data, 
            "pct_change", 
            look_back = as.integer(look_back))
        
        rv$predictions <- predict_lstm(new_data_prepared_list[[1]])
        
        # Forecast Data Preparation
        rv$forecast_tbl <- rv$recipe_spec_prep %>% 
            bake(new_data = rv$data) %>%
            filter_by_time(date, "start", "2011") %>% 
            mutate(split = ifelse(date %in% rv$train_tbl$date, "Training", "Test")) %>%
            add_column(predictions = c(rep(NA, 12), rv$predictions))
        
        
        standardization_tbl <- rv$recipe_spec_prep %>% tidy(2)
        mean <- standardization_tbl %>% filter(statistic == "mean") %>% pull(value) 
        sd   <- standardization_tbl %>% filter(statistic == "sd") %>% pull(value)
        
        gold_price_start <- rv$forecast_tbl %>%
            filter(split == "Training") %>%
            slice(n()) %>%
            pull(gold_price)
        
        rv$forecast_prepared_tbl <- rv$forecast_tbl %>%
            filter(split == "Test") %>%
            mutate(predictions     = standardize_inv_vec(predictions, mean = mean, sd = sd)) %>%
            mutate(gold_prediction = gold_price_start * cumprod(1 + predictions)) %>%
            bind_rows(rv$forecast_tbl %>% filter(split == "Training")) %>%
            select(date, gold_price, gold_prediction, split) %>%
            pivot_longer(cols = c(gold_price, gold_prediction)) %>%
            arrange(date, split, name) %>%
            drop_na() 
        
        rv$metrics <- rv$forecast_prepared_tbl %>%
            filter(split == "Test") %>%
            pivot_wider(names_from = name, values_from = value) %>%
            group_by(split) %>%
            summarize(
                mae   = mae_vec(gold_price, gold_prediction),
                rmse  = rmse_vec(gold_price, gold_prediction),
                mape  = mape_vec(gold_price, gold_prediction),
                smape = smape_vec(gold_price, gold_prediction)
            ) %>%
            ungroup()
        
    }, ignoreNULL = FALSE)
    
    # 2.1.1 Code Output ----
    output$code <- renderPrint({
        # Used for debugging
        req(rv$predictions)
        
        print_model_summary()
        
        # list(
        #     # "rv$data" = rv$data,
        #     # "rv$recipe_spec_prep" = rv$recipe_spec_prep,
        #     "rv$forecast_tbl" = rv$forecast_tbl
        # )
    })

    # 2.2 Plot Residuals-----
    output$plotly_residuals <- renderPlotly({

        req(rv$forecast_tbl)

        g <- rv$forecast_tbl %>%
            mutate(split = factor(split, levels = c("Training", "Test"))) %>%
            ggplot(aes(predictions, pct_change, color = split)) +
            geom_point(alpha = 0.8) +
            geom_abline(slope = 1) +
            scale_color_tq() +
            theme_tq()

        ggplotly(g) 

    })

    # 2.3 Plotly Forecast ----
    output$plotly_forecast <- renderPlotly({

        req(rv$forecast_prepared_tbl)

        rv$forecast_prepared_tbl %>%
            plot_time_series(date, value, .color_var = name, .smooth = F, .plotly_slider = T)

    })
    
    # 2.4 Value Boxes -----
    output$value_boxes <- renderUI({
        
        req(rv$metrics)
        
        mae  <- round(rv$metrics$mae, 2)
        rmse <- round(rv$metrics$rmse, 2)
        mape <- round(rv$metrics$mape, 1)
        
        tagList(
            shiny::fluidRow(
                shiny::column(
                    width = 12,
                    info_card(
                        title = HTML("<span style='color:white;'>MAE</span>"), 
                        value = HTML(str_glue("<span class='label label-info'>{mae}</span>")), 
                        sub_value = ifelse(mae < mae_upper_limit, "Good", "Check Model"), 
                        sub_icon  = ifelse(mae < mae_upper_limit, "arrow-up", "arrow-down"),
                        bg_color  = ifelse(mae < mae_upper_limit, "info", "warning"), 
                        sub_text_color = ifelse(mae < mae_upper_limit, "default", "danger"), 
                        main_icon = "ruler"
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    width = 12,
                    info_card(
                        title = HTML("<span style='color:white;'>RMSE</span>"), 
                        value = HTML(str_glue("<span class='label label-info'>{rmse}</span>")), 
                        sub_value = ifelse(rmse < rmse_upper_limit, "Good", "Check Model"), 
                        sub_icon  = ifelse(rmse < rmse_upper_limit, "arrow-up", "arrow-down"),
                        bg_color  = ifelse(rmse < rmse_upper_limit, "info", "warning"), 
                        sub_text_color = ifelse(rmse < rmse_upper_limit, "default", "danger"), 
                        main_icon = "ruler"
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    width = 12,
                    info_card(
                        title = HTML("<span style='color:white;'>MAPE</span>"), 
                        value = HTML(str_glue("<span class='label label-info'>{mape}%</span>")), 
                        sub_value = ifelse(mape < mape_upper_limit, "Good", "Check Model"), 
                        sub_icon  = ifelse(mape < mape_upper_limit, "arrow-up", "arrow-down"),
                        bg_color  = ifelse(mape < mape_upper_limit, "info", "warning"), 
                        sub_text_color = ifelse(mape < mape_upper_limit, "default", "danger"), 
                        main_icon = "ruler"
                    )
                )
            )
        )
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
