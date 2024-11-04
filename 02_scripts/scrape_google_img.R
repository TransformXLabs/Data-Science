
library(rvest)
library(tidyverse)

scrape_google_img <- function(search_term, dir_path = "") {
  
  search_term_google <- search_term %>% str_replace_all(" ", "+") %>% str_to_lower()
  
  query_string <- str_glue(("https://www.google.com/search?q={search_term_google}&rlz=1C5CHFA_enUS812US812&sxsrf=ALeKk01SYuLHoCsT-NsBQTOOBXkKfwwT2Q:1619607178650&source=lnms&tbm=isch&sa=X&ved=2ahUKEwjHlLz04qDwAhUBHM0KHY5SCaUQ_AUoAnoECAEQBA&biw=1920&bih=919"))
  
  page <- read_html(query_string)
  node <- html_nodes(page, xpath = '//img')
  link <- html_attr(node[2], "src")
  
  download.file(url = link, destfile = str_glue('{dir_path}/{search_term}.jpg'), )
}