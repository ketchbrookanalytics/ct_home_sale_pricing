# Functions to aid Shiny app

# Function to generate predicted value(s) (rounded to the nearest $1)
# from an observation(s) run through a trained model.
# Note: target variable in all models is log(y), so use `exp()` to reverse
# logarithmic scaling.
generate_model_pred <- function(model, data) {
  
  # Using the input model and input data, get the predicted value(s) rounded
  # to the nearest integer
  round(exp(stats::predict(model, data)) %>% 
          dplyr::pull(.pred), 0)
  
}

# Function to return the mean of two predicted values given a singular 
# observation run through two different trained models
generate_ensemble_pred <- function(model1, model2, data) {
  
  if (nrow(data) > 1) {
    
    stop("Length of dataframe is > 1")
    
  }
  
  # Get the predicted value using model passed in the 'model1'
  # argument and the input data from the 'data' argument
  pred1 <- generate_model_pred(
    model = model1, 
    data = data
  )
  
  # Get the predicted value using model passed in the 'model2'
  # argument and the input data from the 'data' argument
  pred2 <- generate_model_pred(
    model = model2, 
    data = data
  )
  
  # Return the mean of 'pred1' and 'pred2', rounded to nearest integer
  mean(c(pred1, pred2)) %>% round(0)
  
}

# Function to generate a ggplot object containing the LIME plot which
# explains feature importance for the particular observation passed to
# the input model
generate_lime_chart <- function(model, train_data, new_data) {
  
  # Capture the new data being scored by the input 'model' object
  new_data <- new_data %>% 
    dplyr::select(
      city, 
      bathrooms, 
      finishedSqFt, 
      yearBuilt, 
      cpipctchange, 
      season
    )

  # Generate the lime explainer object using the input 'train_data'
  # and the input 'model'
  explainer <- lime::lime(
    train_data %>% 
      dplyr::select(-logLastSoldPrice), 
    model
  )
  
  # Generate the lime explanation object using the input 'new_data' and
  # the 'explainer' object, with the top 3 most important features.
  # The `lime::explain()` function generates a new column variable named
  # "explanation", which gets rounded to the nearest integer
  explanation <- lime::explain(
    new_data, 
    explainer, 
    n_features = 3
  ) %>% 
    dplyr::mutate(prediction = round(exp(prediction), 0))
  
  # Create a plot of the top 3 features & explanations
  p <- lime::plot_features(explanation)
  
  # Adjust the plot aesthetics and return the plot 
  p + ggplot2::theme(
    axis.text = ggplot2::element_text(size = 14), 
    axis.title = ggplot2::element_text(
      size = 14, 
      face = "bold"
    )
  )
  
}

# Function to calculate the percent change in the Consumer Price Index using
# the most recent date's value versus the value 12 months prior
get_cpipctchange <- function() {
  
  macro.env <- new.env()
  
  quantmod::getSymbols.FRED(
    "CPIAUCSL", 
    env = macro.env
  )
  
  macro.env$CPIAUCSL %>% 
    data.table::as.data.table() %>% 
    dplyr::rename(date = index) %>% 
    dplyr::arrange(date) %>% 
    dplyr::top_n(13, date) %>% 
    dplyr::mutate(cpipctchange = (CPIAUCSL - dplyr::lag(CPIAUCSL, 12)) / dplyr::lag(CPIAUCSL, 12)) %>% 
    dplyr::slice(dplyr::n()) %>% 
    dplyr::pull(cpipctchange)
  
}
