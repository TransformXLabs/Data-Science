

library(tidymodels)
library(embed)
library(modeldata)
library(tictoc)
library(skimr)

library(tidyverse)



data("okc")
okc

set.seed(123)
splits <- initial_split(okc, prop = 0.80)

splits

# * One Hot Encoding 
recipe_spec_dummy <- recipe(Class ~ age + location, data = training(splits)) %>%
    step_dummy(location, one_hot = TRUE, preserve = FALSE) 

recipe_spec_dummy %>% prep() %>% juice()

tic()
set.seed(123)
wflw_fit_xgb_dummy <- workflow() %>%
    add_model(
        boost_tree(mode = 'classification') %>%
            set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spec_dummy) %>%
    fit(training(splits))
toc()

wflw_fit_xgb_dummy %>%
    predict(testing(splits)) %>%
    bind_cols(testing(splits)) %>%
    yardstick::accuracy(Class, .pred_class)


# * Feature Hashing
recipe_spec_feature_hash <- recipe(Class ~ age + location, data = training(splits)) %>%
    step_feature_hash(location, num_hash = 64, preserve = FALSE) 

recipe_spec_feature_hash %>% prep() %>% juice()

tic()
set.seed(123)
wflw_fit_xgb_hash <- workflow() %>%
    add_model(
        boost_tree(mode = 'classification') %>%
            set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spec_feature_hash) %>%
    fit(training(splits))
toc()

wflw_fit_xgb_hash %>%
    predict(testing(splits)) %>%
    bind_cols(testing(splits)) %>%
    yardstick::accuracy(Class, .pred_class)


data("credit_data")
credit_data_tbl <- credit_data %>% as_tibble()

credit_data_tbl %>% skimr::skim()






