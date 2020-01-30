# Functions to aid Shiny app

generate_model_pred <- function(model, data) {
  
  round(exp(stats::predict(model, data)) %>% 
          dplyr::pull(.pred), 0)
  
}

generate_ensemble_pred <- function(model1, model2, data) {
  
  pred1 <- exp(stats::predict(model1, data)) %>%
    dplyr::pull(.pred)
  
  pred2 <- exp(stats::predict(model2, data)) %>%
    dplyr::pull(.pred)
  
  mean_pred <- round((pred1 + pred2) / 2, 0)
  
  return(mean_pred)
  
}

generate_lime_chart <- function(model, train_data, new_data) {
  
  new_data <- new_data %>% 
    dplyr::select(
      city, 
      bathrooms, 
      finishedSqFt, 
      yearBuilt, 
      cpipctchange, 
      season
    )
  
  
  explainer <- lime::lime(
    train_data %>% 
      dplyr::select(-logLastSoldPrice), 
    model
  )
  
  explanation <- lime::explain(
    new_data, 
    explainer, 
    n_features = 3
  ) %>% 
    dplyr::mutate(prediction = round(exp(prediction), 0))
  
  p <- lime::plot_features(explanation)
  
  p <- p + ggplot2::theme(
    axis.text = ggplot2::element_text(size = 14), 
    axis.title = ggplot2::element_text(
      size = 14, 
      face = "bold"
    )
  )
  
  return(p)
  
}


# generate_price_message <- function() {
#   
#   intro <- "Today's Estimated Sale Price: $"
#   
#   price <- glue::glue(
#     "{format(generate_ensemble_pred(model1 = lm_modl, model2 = xgb_modl, data = df_eval()), big.mark = \",\")}"
#   )
#   
#   line_break <- 1
#   
#   
# }