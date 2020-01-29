# CT Home Sale Pricing Analysis ----
# Author:  Michael Thomas
# Date: 2020-01-22


# Load Packages -----------------------------------------------------------

# Core
library(tidyverse)

# Data
library(RSocrata)
library(quantmod)
library(rvest)
library(xml2)
library(realEstAnalytics)
# library(tmaptools)

library(caret)

# Tidymodels
library(parsnip)
library(rsample)
library(recipes)
library(yardstick)

# Data Prep
library(janitor)
library(stringr)
library(data.table)

# EDA
library(skimr)

# Visualization Aids
library(ggridges)


# Read Data ---------------------------------------------------------------

home_prices <- RSocrata::read.socrata("https://data.ct.gov/resource/5mzw-sjtu.csv") 

counties_lookup_table <- xml2::read_html("https://www1.ctdol.state.ct.us/lmi/misc/counties.asp") %>% 
  rvest::html_nodes("table:nth-child(10)") %>% 
  rvest::html_text() %>% 
  read.table(text = ., 
             header = F, 
             sep = "\t", 
             stringsAsFactors = F) %>% 
  dplyr::slice(-n()) %>% 
  dplyr::rename(town = V1) %>% 
  dplyr::filter(!grepl("^[[:space:]]*$", town)) %>% 
  dplyr::mutate(county = ifelse(stringr::str_detect(town, "County"), 
                                town, 
                                NA)) %>% 
  dplyr::mutate(county = stringr::str_replace(county, " County", "")) %>% 
  tidyr::fill(county, .direction = "down") %>% 
  dplyr::filter(!stringr::str_detect(town, " County")) %>% 
  dplyr::mutate(town = trimws(town), 
                county = trimws(county))

macro.env <- new.env()
quantmod::getSymbols.FRED("CPIAUCSL", env = macro.env)
cpi_data <- macro.env$CPIAUCSL %>% 
  data.table::as.data.table() %>% 
  dplyr::rename(date = index) %>% 
  dplyr::mutate(cpipctchange = (CPIAUCSL - dplyr::lag(CPIAUCSL, 12)) / dplyr::lag(CPIAUCSL, 12)) %>% 
  dplyr::filter(lubridate::year(date) >= 1987) %>% 
  dplyr::select(-CPIAUCSL)

modl_data <- home_prices %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(lubridate::year(daterecorded) <= 2018) %>% 
  dplyr::filter(propertytype == "Residential") %>% 
  dplyr::filter(is.na(nonusecode) | nonusecode == "0") %>% 
  dplyr::filter(saleamount > 100000 & saleamount < 1000000) %>% 
  dplyr::filter(residentialtype %in% c("Single Family", 
                                       "Two Family", 
                                       "Three Family", 
                                       "Four Family", 
                                       "Multi Family")) %>% 
  dplyr::mutate(residentialtype = dplyr::case_when(
    residentialtype == "Single Family" ~ "Single Family", 
    TRUE ~ "Multi Family"
  )) %>% 
  dplyr::mutate(season = dplyr::case_when(
    lubridate::month(daterecorded) %in% 3:5 ~ "Spring", 
    lubridate::month(daterecorded) %in% 6:8 ~ "Summer", 
    lubridate::month(daterecorded) %in% 9:11 ~ "Fall", 
    TRUE ~ "Winter"
  )) %>% 
  dplyr::mutate(lat = as.numeric(NA), 
                lon = as.numeric(NA)) %>% 
  dplyr::mutate(monthyear = as.Date(lubridate::floor_date(daterecorded, 
                                                          unit = "month"), 
                                    "GMT")) %>% 
  dplyr::left_join(cpi_data, 
                   by = c("monthyear" = "date")) %>% 
  dplyr::left_join(counties_lookup_table, 
                   by = "town") %>% 
  dplyr::select(listyear, 
                daterecorded, 
                town, 
                address, 
                # assessedvalue, 
                residentialtype, 
                season, 
                lat, 
                lon, 
                cpipctchange, 
                county, 
                saleamount) %>% 
  dplyr::filter(county == "Tolland") %>% 
  dplyr::mutate(town_lumpd = forcats::fct_lump_min(town, min = 1000)) %>% 
  dplyr::mutate(logsaleamount = log(saleamount))


zip_data <- (xml2::read_html("https://www.zipcodestogo.com/Connecticut/") %>% 
               rvest::html_nodes("table.inner_table") %>%
               # rvest::html_nodes("tbody") %>% 
               rvest::html_table())[1] %>% 
  as.data.frame() %>% 
  dplyr::slice(-1)

zip_col_names <- unlist(zip_data[1,], 
                        use.names = F)

zip_data <- zip_data %>%
  set_names(zip_col_names) %>% 
  dplyr::slice(-1) %>% 
  dplyr::select(-`Zip Code Map`) %>% 
  dplyr::arrange(City) %>% 
  dplyr::filter(County == "Tolland") %>% 
  dplyr::select(-County) %>% 
  dplyr::mutate(City = dplyr::case_when(
    City == "Mansfield Center" ~ "Mansfield", 
    City == "Vernon Rockville" ~ "Vernon", 
    TRUE ~ City
  ))

modl_data <- modl_data %>% 
  dplyr::left_join(zip_data, by = c("town" = "City")) %>%
  dplyr::rename(zipcode = `Zip Code`)

home_specs <- modl_data[8640,] %>% 
  dplyr::mutate(state = "CT") %>% 
  dplyr::select(address, 
                zipcode, 
                state, 
                town) %>% 
  dplyr::rename(city = town) %>% 
  realEstAnalytics::GetDeepSearchResults_dataframe(
    col.address = 1, 
    col.zipcode = 2, 
    col.city = 4, 
    col.state = 3, 
    rentzestimate = F, 
    api_key = "X1-ZWz17f6oh93guj_88dza"
  )


modl_data_fnl <- home_specs %>% 
  tidyr::drop_na() %>% 
  dplyr::select(
    city, 
    lat, 
    long, 
    bathrooms, 
    bedrooms, 
    totalRooms, 
    finishedSqFt, 
    yearBuilt,
    lastSoldDate, 
    lastSoldPrice
  ) %>% 
  dplyr::mutate(lastSoldDate = as.Date(lastSoldDate, "%m/%d/%Y")) %>% 
  dplyr::mutate(city = dplyr::case_when(
    city == "hebron" ~ "Hebron", 
    city %in% c("Mansfield Center", 
                "Mansfield Depot", 
                "Storrs", 
                "Storrs Mansfield") ~ "Mansfield", 
    city %in% c("Stafford", 
                "Stafford springs") ~ "Stafford Springs",
    TRUE ~ city
  )) %>% 
  # dplyr::mutate(city = factor(city)) %>% 
  dplyr::mutate(city = forcats::fct_lump_min(
    city, 
    min = 100, 
    other_level = "Too Small"
  )) %>% 
  dplyr::mutate(city = as.character(city)) %>% 
  dplyr::filter(city != "Too Small") %>% 
  dplyr::filter(dplyr::between(
    lastSoldPrice, 
    100000, 
    1000000
  )) %>% 
  dplyr::mutate(logLastSoldPrice = log(lastSoldPrice), 
                monthyear = lubridate::floor_date(lastSoldDate, 
                                                  unit = "month")) %>% 
  dplyr::left_join(cpi_data, by = c("monthyear" = "date")) %>% 
  dplyr::filter(dplyr::between(bathrooms, 1, 7)) %>% 
  dplyr::mutate(bathrooms = (round(bathrooms * 2) / 2)) %>% 
  dplyr::filter(dplyr::between(bedrooms, 1, 6)) %>% 
  dplyr::filter(dplyr::between(finishedSqFt, 400, 10000)) %>% 
  dplyr::mutate(season = dplyr::case_when(
    lubridate::month(lastSoldDate) %in% 3:5 ~ "Spring", 
    lubridate::month(lastSoldDate) %in% 6:8 ~ "Summer", 
    lubridate::month(lastSoldDate) %in% 9:11 ~ "Fall", 
    TRUE ~ "Winter"
  )) %>% 
  tidyr::drop_na()



# for (i in 6:nrow(modl_data)) {
#   
#   fulladd <- paste0(modl_data$address[i],
#                     ", ",
#                     modl_data$town[i],
#                     ", CT")
#   
#   temp <- tmaptools::geocode_OSM(fulladd)
#   
#   if (is.null(temp)) {
#     
#     modl_data$lat[i] <- NA
#     modl_data$lon[i] <- NA
#     
#   } else {
#     
#     modl_data$lat[i] <- temp$coords[[2]]
#     modl_data$lon[i] <- temp$coords[[1]]
#     
#   }
#   
#   if (i %% 6 == 0 ) {
#     
#     Sys.sleep(rnorm(1, mean = 10, sd = 3))
#     
#   }
#   
# }


# EDA ---------------------------------------------------------------------

# Median Single Family Sale Price by Year of Sale
modl_data_fnl %>% 
  dplyr::mutate(year_recorded = lubridate::year(lastSoldDate)) %>% 
  dplyr::group_by(year_recorded) %>% 
  # dplyr::count() %>% 
  dplyr::summarise(med_sale_price = median(logLastSoldPrice)) %>% 
  dplyr::ungroup() %>% 
  ggplot2::ggplot(aes(x = year_recorded, 
                      y = med_sale_price)) + 
  ggplot2::geom_line(size = 2)

# Median Single Family Sale Price by Month and Year of Sale
modl_data_fnl %>% 
  dplyr::mutate(monthyear_recorded = lubridate::floor_date(
    lastSoldDate, 
    unit = "month"
  )) %>% 
  dplyr::group_by(monthyear_recorded) %>% 
  # dplyr::count() %>% 
  dplyr::summarise(med_sale_price = median(logLastSoldPrice)) %>% 
  dplyr::ungroup() %>% 
  ggplot2::ggplot(aes(
    x = monthyear_recorded, 
    y = med_sale_price
  )) + 
  ggplot2::geom_line(size = 1)

modl_data_fnl %>% 
  dplyr::mutate(monthyear_recorded = as.Date(lubridate::floor_date(lastSoldDate, 
                                                                   unit = "month"), 
                                             "GMT"))%>% 
  dplyr::group_by(monthyear_recorded) %>% 
  dplyr::summarise(med_sale_price = median(logLastSoldPrice), 
                   cpipctchange = mean(cpipctchange)) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_longer(
    -monthyear_recorded, 
    names_to = "metric"
  ) %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = monthyear_recorded, 
      y = value
    )
  ) + 
  ggplot2::geom_line() + 
  ggplot2::facet_grid(metric ~ ., 
                      scales = "free")


modl_data_fnl %>% 
  dplyr::pull(logLastSoldPrice) %>% 
  hist(breaks = 30, 
       main = "Histogram of log(Sale Amount)")

modl_data_fnl %>% 
  ggplot2::ggplot(aes(
    x = logLastSoldPrice, 
    y = as.character(bathrooms), 
    fill = stat(x)
  )) + 
  ggridges::geom_density_ridges_gradient(
    scale = 1.5, 
    rel_min_height = 0.01) +
  ggplot2::scale_fill_viridis_c(
    name = "Sale Amount", 
    option = "C"
  ) +
  ggplot2::labs(
    title = "log(Sale Amount) by Number of Bathrooms", 
    x = "log(Sale Amount)", 
    y = "Number of Bathrooms"
  )


modl_data_fnl %>% 
  ggplot2::ggplot(aes(
    x = logLastSoldPrice, 
    y = as.character(bedrooms), 
    fill = stat(x)
  )) + 
  ggridges::geom_density_ridges_gradient(
    scale = 1.5, 
    rel_min_height = 0.01) +
  ggplot2::scale_fill_viridis_c(
    name = "Sale Amount", 
    option = "C"
  ) +
  ggplot2::labs(
    title = "log(Sale Amount) by Number of Bedrooms", 
    x = "log(Sale Amount)", 
    y = "Number of Bedrooms"
  )



# Is list year something we should control for?
modl_data_fnl %>% 
  ggplot2::ggplot(aes(
    x = logLastSoldPrice, 
    y = as.character(lubridate::year(lastSoldDate)), 
    fill = stat(x)
  )) + 
  ggridges::geom_density_ridges_gradient(
    scale = 1.5, 
    rel_min_height = 0.01
  ) +
  ggplot2::scale_fill_viridis_c(
    name = "Sale Amount", 
    option = "C"
  ) +
  ggplot2::labs(
    title = "log(Sale Amount) by Year", 
    x = "log(Sale Amount)", 
    y = "Year of Sale"
  )

modl_data_fnl %>% 
  ggplot2::ggplot(aes(
    x = logLastSoldPrice, 
    y = season, 
    fill = stat(x)
  )) + 
  ggridges::geom_density_ridges_gradient(
    scale = 1.5, 
    rel_min_height = 0.01
  ) +
  ggplot2::scale_fill_viridis_c(
    name = "Sale Amount", 
    option = "C"
  ) +
  ggplot2::labs(
    title = "log(Sale Amount) by Season", 
    x = "log(Sale Amount)", 
    y = "Season of Sale"
  )


modl_data_fnl %>% 
  ggplot2::ggplot(aes(x = logLastSoldPrice, 
                      y = cpipctchange)) + 
  ggplot2::geom_point()


# Modeling ----------------------------------------------------------------

# Quickly fit an lm() to see basic linear variable significance
summary(lm(
  logLastSoldPrice ~ 
    city + 
    bathrooms + 
    bedrooms + 
    finishedSqFt + 
    yearBuilt + 
    cpipctchange + 
    season, 
  data = modl_data_fnl
))




set.seed(123)
modl_data_initial_split <- rsample::initial_split(modl_data_fnl, 
                                                  prop = 0.8)


# Simple Linear Regression ------------------------------------------------

lm_preproc_recipe <- recipes::recipe(
  logLastSoldPrice ~ 
    city + 
    bathrooms + 
    # bedrooms + 
    finishedSqFt + 
    yearBuilt + 
    cpipctchange + 
    season, 
  data = rsample::training(modl_data_initial_split)
) %>% 
  recipes::step_string2factor(recipes::all_nominal()) %>% 
  # recipes::step_rm(model) %>% 
  recipes::prep()


lm_modl_data_preproc_train <- recipes::bake(lm_preproc_recipe, 
                                            rsample::training(modl_data_initial_split))

lm_modl_data_preproc_test <- recipes::bake(lm_preproc_recipe, 
                                           rsample::testing(modl_data_initial_split))


lm_modl <- parsnip::linear_reg(
  mode = "regression"
) %>% 
  parsnip::set_engine("lm") %>%
  parsnip::fit(logLastSoldPrice ~ ., 
               data = lm_modl_data_preproc_train)


lm_preds <- parsnip::predict.model_fit(object = lm_modl, 
                                       new_data = lm_modl_data_preproc_test)

lm_results <- lm_modl_data_preproc_test %>% 
  dplyr::bind_cols(lm_preds)


# Nearest Neighbors Approaches --------------------------------------------

knn_preproc_recipe <- recipes::recipe(
  logLastSoldPrice ~ 
    city + 
    lat + 
    long + 
    bathrooms + 
    bedrooms + 
    finishedSqFt + 
    yearBuilt + 
    cpipctchange + 
    season, 
  data = rsample::training(modl_data_initial_split)
) %>% 
  recipes::step_string2factor(recipes::all_nominal()) %>% 
  # recipes::step_rm(model) %>% 
  recipes::prep()


knn_modl_data_preproc_train <- recipes::bake(knn_preproc_recipe, 
                                             rsample::training(modl_data_initial_split))

knn_modl_data_preproc_test <- recipes::bake(knn_preproc_recipe, 
                                            rsample::testing(modl_data_initial_split))


knn_modl <- parsnip::nearest_neighbor(
  mode = "regression", 
  neighbors = 8, 
  weight_func = "rectangular"
) %>% 
  parsnip::set_engine("kknn") %>%
  parsnip::fit(logLastSoldPrice ~ ., 
               data = knn_modl_data_preproc_train)


knn_preds <- parsnip::predict.model_fit(object = knn_modl, 
                                        new_data = knn_modl_data_preproc_test)


knn_results <- knn_modl_data_preproc_test %>% 
  dplyr::bind_cols(knn_preds)

yardstick::mae(knn_results, 
               truth = exp(logLastSoldPrice), 
               estimate = exp(.pred))


# Other Regression Approaches ---------------------------------------------

# set.seed(123)
# modl_data_initial_split <- rsample::initial_split(modl_data_fnl, 
#                                                   prop = 0.8)

xgb_preproc_recipe <- recipes::recipe(
  logLastSoldPrice ~ 
    city + 
    bathrooms + 
    bedrooms + 
    finishedSqFt + 
    yearBuilt + 
    cpipctchange + 
    season, 
  data = rsample::training(modl_data_initial_split)
) %>% 
  recipes::step_string2factor(recipes::all_nominal()) %>% 
  # recipes::step_rm(model) %>% 
  recipes::prep()

xgb_modl_data_preproc_train <- recipes::bake(xgb_preproc_recipe, 
                                             rsample::training(modl_data_initial_split))

xgb_modl_data_preproc_test <- recipes::bake(xgb_preproc_recipe, 
                                            rsample::testing(modl_data_initial_split))

xgb_modl <- parsnip::boost_tree(
  mode = "regression", 
  trees = 1000
) %>% 
  parsnip::set_engine("xgboost", 
                      objective = "reg:squarederror") %>% 
  parsnip::fit(logLastSoldPrice ~ ., 
               data = xgb_modl_data_preproc_train)

# glmnet_modl <- parsnip::linear_reg(
#   mode = "regression" 
# ) %>% 
#   parsnip::set_engine("glmnet") %>% 
#   parsnip::fit(logsaleamount ~ ., 
#                data = modl_data_preproc_train)

xgb_preds <- parsnip::predict.model_fit(
  object = xgb_modl, 
  new_data = xgb_modl_data_preproc_test
)

# glmnet_preds <- parsnip::predict.model_fit(object = glmnet_modl, 
#                                            new_data = modl_data_preproc_test)

xgb_results <- xgb_modl_data_preproc_test %>% 
  dplyr::bind_cols(xgb_preds)

yardstick::mae(
  results2, 
  truth = exp(logLastSoldPrice), 
  estimate = exp(.pred)
)



# Model Aggregation -------------------------------------------------------

actuals <- recipes::bake(lm_preproc_recipe, 
                         rsample::testing(modl_data_initial_split)) %>% 
  dplyr::select(logLastSoldPrice)


agg_results <- lm_preds %>% 
  dplyr::rename(lm_pred = .pred) %>% 
  dplyr::bind_cols(knn_preds) %>% 
  dplyr::rename(knn_pred = .pred) %>% 
  dplyr::bind_cols(xgb_preds) %>% 
  dplyr::rename(xgb_pred = .pred) %>% 
  dplyr::mutate(mean_pred = (lm_pred + knn_pred + xgb_pred) / 3) %>% 
  dplyr::mutate(lm_xgb_pred = (lm_pred + xgb_pred) / 2) %>% 
  dplyr::select(mean_pred, lm_xgb_pred) %>% 
  dplyr::bind_cols(actuals)
  
  
# Model Comparison --------------------------------------------------------

# lm
yardstick::mae(
  lm_results, 
  truth = exp(logLastSoldPrice), 
  estimate = exp(.pred)
)

# knn
yardstick::mae(
  knn_results, 
  truth = exp(logLastSoldPrice), 
  estimate = exp(.pred)
)

# xgb
yardstick::mae(
  xgb_results, 
  truth = exp(logLastSoldPrice), 
  estimate = exp(.pred)
)

# agg
yardstick::mae(
  agg_results, 
  truth = exp(logLastSoldPrice), 
  estimate = exp(mean_pred)
)

# lm + xgb
yardstick::mae(
  agg_results, 
  truth = exp(logLastSoldPrice), 
  estimate = exp(lm_xgb_pred)
)




