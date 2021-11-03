



library(modeltime)
library(modeltime.ensemble)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
library(janitor)
library(plotly)

# VISUALIZATION 1 - METRICS


google_analytics_tbl <- read_rds("data/google_analytics_by_page_daily.rds") %>%
  clean_names()

google_analytics_tbl

ga_summarized_tbl <- google_analytics_tbl %>%
  group_by(page_path) %>%
  summarise(
    count = n(),
    rank  = unique(rank),
    page_views = sum(page_views),
    viralness = sum(page_views) / count
  ) %>%
  mutate(desc = str_glue("Page: {page_path}
                          ----
                          Rank: {rank}
                          Count: {count}
                          Page Views: {scales::comma(page_views)}
                          Viralness: {round(viralness)}
                         "))

g <- ga_summarized_tbl %>%
  ggplot(aes(page_views, viralness, color = page_views, size = page_views)) +
  geom_point(aes(text=desc), alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Page Views (Log10 scale)", y = "Viralness (Log10 Scale)") +
  # scale_color_distiller(type = "seq") +
  scale_color_gradientn(colors = c("white", "#18bc9c")) +
  theme_minimal() +
  scale_size(range = c(3,10)) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    text = element_text(colour = "white")
  ) 


ggplotly(g)  

# VISUALIZATION 2 - FORECAST ---- 

nested_modeltime_refit_tbl <- read_rds("models/nested_modeltime_refit_tbl.rds")

g <- nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  filter(page_path %in% c("/")) %>%
  plot_modeltime_forecast(.interactive = F) +
  scale_color_manual(values = c("#18bc9c", "white")) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    text = element_text(colour = "white"),
    legend.background = element_rect(fill="black")
  ) 

ggplotly(g)  
