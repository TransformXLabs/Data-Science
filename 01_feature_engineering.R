# LIBRARIES ----

library(DBI)
library(RSQLite)

library(plotly)
library(skimr)
library(GGally)

library(tidymodels)
library(embed)
library(textrecipes)
library(vip)

library(lubridate)
library(tidyverse)


# A. DATABASE ----

# * DATABASE CONNECTION ----

con <- DBI::dbConnect(SQLite(), "data/Chinook_Sqlite.sqlite")
con


# * DATABASE TABLES ----

dbListTables(con)

tbl(con, "Invoice")

tbl(con, "Invoice") %>% collect()

# Quick Tip - View all tables in Database

dbListTables(con) %>% map(~ tbl(con, .))


# * INVOICES ----

invoices_tbl <- tbl(con, "Invoice") %>% collect()

invoices_tbl <- invoices_tbl %>%
    mutate(InvoiceDate = as_date(InvoiceDate))

invoices_tbl %>% glimpse()

invoices_tbl %>% write_rds("data/invoices_tbl.rds")

# * CUSTOMERS ----

customers_tbl <- tbl(con, "Customer") %>% collect()

customers_tbl %>% glimpse()
    
# * INVOICE LINES  ----

invoice_lines_tbl <- tbl(con, "InvoiceLine") %>%
    left_join(
        tbl(con, "Track") %>% 
            select(-UnitPrice) %>%
            rename(TrackName = Name), 
        by = "TrackId"
    ) %>%
    left_join(
        tbl(con, "Genre") %>% rename(GenreName = Name), by = "GenreId"
    ) %>%
    left_join(
        tbl(con, "Album") %>% rename(AlbumTitle = Title), by = "AlbumId"
    ) %>%
    left_join(
        tbl(con, "Artist") %>% rename(ArtistName = Name), by = "ArtistId"
    ) %>%
    left_join(
        tbl(con, "Invoice") %>% select(InvoiceId, CustomerId), 
        by = "InvoiceId"
    ) %>%
    select(-ends_with("Id"), starts_with("Invoice"), starts_with("Customer")) %>%
    relocate(contains("Id"), .before = 1) %>%
    collect() 

invoice_lines_tbl %>% glimpse()

# invoice_lines_tbl %>% write_rds("data/invoice_lines_tbl.rds")
invoice_lines_tbl <- read_rds("data/invoice_lines_tbl.rds")

# * CHECK DATASET ----

invoice_lines_tbl %>% skim()

# * CLOSE CONNECTION ----

DBI::dbDisconnect(con)


# B. FEATURE ENGINEERING ----

# 1.0 PRODUCT RELATIONSHIP: CUSTOMER-ARTIST ----
# - Focus: Invoice Lines

invoice_lines_tbl %>% distinct(ArtistName)

# * PIVOT LONGER (DUMMY) ----
customer_artists_tbl <- invoice_lines_tbl %>%
    select(CustomerId, ArtistName) %>%
    count(CustomerId, ArtistName) %>%
    pivot_wider(
        names_from  = ArtistName, 
        values_from = n, 
        values_fill = 0,
        names_prefix = "artist_",
        names_sep = "_"
    )

customer_artists_tbl

# * DIMENSIONALITY REDUCTION WITH UMAP ----

recipe_spec_umap <- recipe(~ ., customer_artists_tbl) %>%
    step_umap(
        -CustomerId, 
        num_comp = 20, 
        retain   = FALSE,
        seed     = c(123, 123), 
    )

customer_artists_umap_tbl <- recipe_spec_umap %>% prep() %>% juice()

customer_artists_umap_tbl

# customer_artists_umap_tbl %>% write_rds("data/customer_artists_umap_tbl.rds")
customer_artists_umap_tbl <- read_rds("data/customer_artists_umap_tbl.rds")

# * WHICH CUSTOMERS ARE BUYING FROM SIMILAR ARTISTS? ----

# - 2D Plot
g <- customer_artists_umap_tbl %>%
    ggplot(aes(umap_01, umap_02)) +
    geom_point(aes(text = CustomerId), alpha = 0.5) 

ggplotly(g)

# - 3D Plot
customer_artists_umap_tbl %>%
    plot_ly(x = ~ umap_01, y = ~ umap_02, z = ~ umap_03, color = ~ umap_04, 
            text = ~ CustomerId) %>%
    add_markers()

invoice_lines_tbl %>% 
    filter(CustomerId %in% c(35, 55, 16)) %>%
    count(CustomerId, ArtistName) %>%
    group_by(CustomerId) %>%
    arrange(-n, .by_group = TRUE) %>%
    slice(1:5)

invoice_lines_tbl %>% 
    filter(CustomerId %in% c(32, 52)) %>%
    count(CustomerId, GenreName, ArtistName) %>%
    group_by(CustomerId) %>%
    arrange(-n, .by_group = TRUE) %>%
    slice(1:5)


invoice_lines_tbl %>% glimpse()

# * AGGREGATION FEATURES: Length of Song ----

customer_song_len_tbl <- invoice_lines_tbl %>%
    select(CustomerId, Milliseconds) %>%
    group_by(CustomerId) %>%
    summarise(
        enframe(quantile(Milliseconds, probs = c(0, 0.25, 0.5, 0.75, 1)))
    ) %>%
    ungroup() %>%
    mutate(name = str_remove_all(name, "%")) %>%
    pivot_wider(
        names_from   = name, 
        values_from  = value,
        names_prefix = "song_len_q"
    )

customer_song_len_tbl %>%
    arrange(-song_len_q100)

# 2.0 PURCHASE RELATIONSHIPS: DATE FEATURES & PRICE FEATURES ----
# - Focus: Invoices Table

# * DATE & PRICE FEATURES ----

max_date <- max(invoices_tbl$InvoiceDate)

customer_invoice_tbl <- invoices_tbl %>% 
    select(CustomerId, InvoiceDate, Total) %>%
    group_by(CustomerId) %>%
    summarise(
        
        # Date Features
        inv_most_recent_purchase = (max(InvoiceDate) - max_date) / ddays(1),
        inv_tenure               = (min(InvoiceDate) - max_date) / ddays(1),
        
        # Prices/Quantity Features
        inv_count = n(),
        inv_sum   = sum(Total, na.rm = TRUE),
        inv_avg   = mean(Total, na.rm = TRUE)
    ) 

customer_invoice_tbl

customer_invoice_tbl %>%
    ggpairs(
        columns = 2:ncol(.),
        title   = "Customer Aggregated Invoice Features"
    )

# 3.0 CUSTOMER FEATURES ----
# Focus: Customers Table

customers_tbl %>% skim()

# * JOINING ----

customers_joined_tbl <- customers_tbl %>%
    select(contains("Id"), PostalCode, Country, City) %>%
    left_join(
        customer_invoice_tbl, by = "CustomerId"
    ) %>%
    left_join(
        customer_song_len_tbl, by = "CustomerId"
    ) %>%
    left_join(
        customer_artists_umap_tbl, by = "CustomerId"
    ) %>%
    rename_at(.vars = vars(starts_with("umap")), .funs = ~ str_glue("artist_{.}"))

customers_joined_tbl %>% glimpse()

customers_joined_tbl %>% skim()

# customers_joined_tbl %>% write_rds("data/customers_joined_tbl.rds")
customers_joined_tbl <- read_rds("data/customers_joined_tbl.rds")


# C. MODELING SETUP ----

# * Make Target Feature ----
full_data_tbl <- customers_joined_tbl %>%
    mutate(Target = ifelse(inv_most_recent_purchase >= -90, 1, 0)) %>%
    mutate(Target = as.factor(Target)) %>%
    select(-inv_most_recent_purchase) %>%
    relocate(Target, .after = CustomerId) 

# * Splits ----

set.seed(123)
splits <- initial_split(full_data_tbl, prop = 0.80)

# write_rds(splits, "data/splits.rds")
splits <- read_rds("data/splits.rds")

# * Recipe Embedding: Dummy ----

recipe_spec_dummy <- recipe(Target ~ ., training(splits)) %>%
    add_role(CustomerId, new_role = "Id") %>%
    # step_rm(Email) %>%
    step_knnimpute(
        PostalCode, 
        impute_with = vars(Country, City)
    ) %>%
    step_dummy(Country, City, PostalCode, one_hot = TRUE)

recipe_spec_dummy %>% prep() %>% juice() 


# * Recipe Embedding: Hash ----

recipe_spec_hash <- recipe(Target ~ ., training(splits)) %>%
    add_role(CustomerId, new_role = "Id") %>%
    # step_rm(Email) %>%
    # step_knnimpute(
    #     PostalCode, 
    #     impute_with = vars(Country, City)
    # ) %>%
    step_feature_hash(Country, City, PostalCode, num_hash = 15)

recipe_spec_hash %>% prep() %>% juice() %>% glimpse()

# * Modeling ----

# ** Dummy ----
wflw_fit_xgb_dum <- workflow() %>%
    add_model(
        spec = boost_tree(mode = "classification") %>% set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spec_dummy) %>%
    fit(training(splits))


bind_cols(
    wflw_fit_xgb_dum %>% predict(testing(splits), type = "prob"),
    testing(splits)
) %>%
    yardstick::roc_auc(Target, .pred_1)

# ** Hash ----
wflw_fit_xgb_hash <- workflow() %>%
    add_model(
        spec = boost_tree(mode = "classification") %>% set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spec_hash) %>%
    fit(training(splits))
   
bind_cols(
    wflw_fit_xgb_hash %>% predict(testing(splits), type = "prob"),
    testing(splits)
) %>%
    yardstick::roc_auc(Target, .pred_1)

# FEATURE IMPORTANCE ----

wflw_fit_xgb_hash$fit$fit$fit %>% vip()

full_data_tbl %>%
    ggplot(aes(inv_tenure, fill = Target))+
    geom_density(alpha = 0.5)

full_data_tbl$inv_tenure %>% range()

full_data_tbl %>%
    ggplot(aes(song_len_q50, fill = Target))+
    geom_density(alpha = 0.5)

full_data_tbl %>%
    ggplot(aes(artist_umap_16, fill = Target))+
    geom_density(alpha = 0.5)

# MAKE FULL PREDICTIONS ----

bind_cols(
    wflw_fit_xgb_hash %>% predict(full_data_tbl, type = "prob"),
    customers_joined_tbl
) %>%
    write_rds("data/customer_predictions_tbl.rds")





