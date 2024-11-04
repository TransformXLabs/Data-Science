library(rvest)
library(tidyverse)
library(job)

source("02_scripts/scrape_google_img.R")

# TEST ----

scrape_google_img("tigers", dir_path = "zz_google_scrape_test/")

scrape_google_img("forbes logo", dir_path = "www/")

# DOWNLOAD ALL CEO IMAGES ----
# **** LONG RUNNING SCRIPT **** ----

ceo_raw_tbl <- read_csv("data/ceo_compensation_2008.csv")

ceo_vec <- ceo_raw_tbl$CEO

quiet_scrape <- quietly(scrape_google_img)

ceo_vec %>%
  map(.f = quiet_scrape, dir_path = "www/img/")
