library(shiny)
library(shinyWidgets)
library(bslib)
library(DT)
library(plotly)
library(tidyverse)
library(fs)
library(timetk)

# source("05_app_scripts/info_card.R")

info_card <- function(ceo_name, ceo_company, ceo_img, 
                      
                      pay_actual = 100, pay_pred = 100, pay_diff = 0,
                      rank_value = 1, 
                      
                      main_icon = "chart-line",
                      bg_color = "default", text_color = "default") {
  
  value_alert <- case_when(
    pay_diff >= 0 ~ "text-success",
    pay_diff > -10 ~ "text-warning",
    TRUE ~ "text-danger"
  )
  
  div(
    class = "card",
    # style = "padding:0px;",
    h3(class = "card-header text-center", style='margin-top:0%;padding-top:20px;', ceo_name),
    div(
      class = str_glue("card-body bg-{bg_color} text-{text_color} text-center"),
      
      #image
      tags$img(
        class = "pull-left img-thumbnail mx-auto d-block",
        # class = "img-responsive left-block",
        style = "max-width:100%;",
        src   = ceo_img
        # src = "/img/A_D_David_MacKay.jpg"
      ),
      h4(class = "card-title", tags$small("Company: "), ceo_company),
      
      hr(),
      
      fluidRow(
        column(12, h4(str_glue("CEO Value Ranking: {rank_value}"))),
        column(
          6, h6("Actual Pay"), p(scales::dollar(pay_actual, suffix = "M"))
        ),
        column(
          6, h6("Predicted Pay"), p(scales::dollar(pay_pred, suffix = "M"))
        ),
        hr(),
        column(
          12, h4("Difference", br(), tags$small( "(Model vs Actual)")), 
          h4(scales::dollar(pay_diff, suffix = "M"), class = value_alert)
        )
      ),
      
      br()
    )
  )
  
}

formatMillions <- JS(
  "function(data) {",
  "return (data).toFixed(1) + 'M'",
  "}")


color_primary = "#78c2ad"

predictions_tbl <- read_rds("01_results/predictions.rds")

stock_prices_tbl <- read_rds("00_data/stock_prices.rds") %>% distinct()

ceo_rankings_tbl <- predictions_tbl %>%
  mutate(pay_diff = .pred - total_2008_compensation) %>%
  
  # Rank Pay
  arrange(total_2008_compensation) %>%
  mutate(rank_pay = 1:nrow(.)) %>%
  
  # Rank Model
  arrange(-.pred) %>%
  mutate(rank_model = 1:nrow(.)) %>%
  
  # Rank Value
  arrange(-pay_diff) %>%
  mutate(rank_value = 1:nrow(.)) %>% 
  
  select(
    ceo, total_2008_compensation, .pred, pay_diff, 
    rank_pay, rank_model, rank_value, 
    company, ticker, industry
  ) 
  

ui <- navbarPage(
  
  title = 'CEO Performance Explorer',
  theme = bs_theme(version = 4, bootswatch = 'lux'),
  inverse = TRUE, 
  
  tabPanel(
    title = "",
    
    # SIDEBAR ----
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h1("CEO Compensation Analysis"),
        hr(),
        fluidRow(
          column(width = 2, tags$img(src = "", style="width:60px;-webkit-filter: drop-shadow(5px 5px 5px #222);") )
          ,
          column(
            width = 10, 
            h5("Forbes 2008 CEO Compensation Model"),
            shiny::markdown(
              str_glue("
> The CEO Performance Explorer uses a cutting-edge algorithm to predict CEO pay and compare the results to actual pay. The dataset used is from 2008. 

> <span class='text-info'>The model ranks CEO's based on their value</span> - the difference between modeled compensation and actual compensation.")
            )
          )
          
        )
        
      ),
      
      # MAIN ----
      mainPanel(
        width = 9,
        
        # verbatimTextOutput("debug"),
        
        fluidRow(
          
          column(
            width = 4, 
            uiOutput("ceo_card")
          ),
          column(
            width = 8, 
            plotlyOutput("stock_chart", height = "600px")
          )
          
        ),
        
        br(),
        
        DT::DTOutput("ceo_table")
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  rv$selected_ceo <- 1
  
  observeEvent(input$ceo_table_rows_selected, {
    rv$selected_row    <- input$ceo_table_rows_selected
    
    rv$ceo_rankings_filtered_tbl <- ceo_rankings_tbl %>%
      slice(rv$selected_row)
    
    print(rv$ceo_rankings_filtered_tbl)
    
    rv$selected_ceo        <- rv$ceo_rankings_filtered_tbl %>% pull(ceo)
    
    rv$selected_company    <- rv$ceo_rankings_filtered_tbl %>% pull(company)
    
    rv$selected_symbol     <- rv$ceo_rankings_filtered_tbl %>% pull(ticker)
    
    rv$pay_actual          <- rv$ceo_rankings_filtered_tbl %>% pull(total_2008_compensation)
    
    rv$pay_pred            <- rv$ceo_rankings_filtered_tbl %>% pull(.pred)
    
    rv$pay_diff            <- rv$ceo_rankings_filtered_tbl %>% pull(pay_diff)
    
    rv$rank_value         <- rv$ceo_rankings_filtered_tbl %>% pull(rank_value)
    
  })
  
  output$ceo_table <- renderDataTable(
    {
      ceo_rankings_tbl %>%
        # mutate(pay_diff2 = pay_diff) %>%
        # mutate(
        #   total_2008_compensation = scales::dollar(total_2008_compensation, suffix = "M"),
        #   .pred = scales::dollar(.pred, suffix = "M"),
        #   pay_diff = scales::dollar(pay_diff, suffix = "M")
        # ) %>%
        rename(
          "2008 Compensation (Actual)" = total_2008_compensation,
          "2008 Compensation (Model)" = .pred,
          "Difference (Model vs Actual)" = pay_diff
        ) %>%
        rename_all(.funs = ~ str_replace_all(., "_", " ")) %>%
        
        # Data Table 
        datatable(
          selection = list(mode = "single", selected = 1),
          options   = list(
            scrollX = TRUE,
            columnDefs = list(list(
              targets = 2:4,
              render  = formatMillions
            ))
          )
        ) %>%
      
        formatStyle(
          columns = "Difference (Model vs Actual)",
          color = styleInterval(cuts = 0, values = c("red", "green")),
          fontWeight = "bold"
        ) 
        
        # %>%
        # formatCurrency(
        #   columns = c("2008 Compensation (Actual)", "2008 Compensation (Model)", "Difference (Model vs Actual)"),
        #   digits = 0
        # ) 
      
    } 
    
  )
  
  output$ceo_card <- renderUI({
    
    req(rv$selected_ceo, rv$selected_company)
    
    info_card(
      ceo_name    = rv$selected_ceo, 
      ceo_company = rv$selected_company, 
      ceo_img     = str_glue('img/{rv$selected_ceo}.jpg'),
      
      pay_actual = rv$pay_actual, 
      pay_pred   = rv$pay_pred,
      pay_diff   = rv$pay_diff,
      rank_value = rv$rank_value, 
      
      text_color  = "primary"
    )
  })
  
  output$debug <- renderPrint({
    
    list(
      selected_ceo    = rv$selected_ceo,
      selected_symbol = rv$selected_symbol
    )
    
    
  })
  
  output$stock_chart <- renderPlotly({
    
    req(rv$selected_symbol)
    
    stock_prices_filter_tbl <- stock_prices_tbl %>%
      filter(symbol %in% rv$selected_symbol) 
    
    req(nrow(stock_prices_filter_tbl) > 0)
    
    stock_prices_filter_tbl %>%
      plot_time_series(
        date, adjusted, .smooth = F, 
        .title = rv$selected_symbol, .plotly_slider = T
      )
  })
  
  
}

shinyApp(ui, server)