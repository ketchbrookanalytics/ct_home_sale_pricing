# Function to calculate the percent change in the Consumer Price Index using
# the most recent date's value versus the value 12 months prior
get_cpipctchange <- function() {
  
  macro.env <- new.env()
  
  quantmod::getSymbols.FRED(
    "CPIAUCSL", 
    env = macro.env
  )
  
  macro.env$CPIAUCSL |> 
    data.table::as.data.table() |> 
    dplyr::rename(date = index) |> 
    dplyr::arrange(date) |> 
    dplyr::top_n(13, date) |> 
    dplyr::mutate(cpipctchange = (CPIAUCSL - dplyr::lag(CPIAUCSL, 12)) / dplyr::lag(CPIAUCSL, 12)) |> 
    dplyr::slice(dplyr::n()) |> 
    dplyr::pull(cpipctchange)
  
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
  mean(c(pred1, pred2)) |> round(0)
  
}

# Function to generate predicted value(s) (rounded to the nearest $1)
# from an observation(s) run through a trained model.
# Note: target variable in all models is log(y), so use `exp()` to reverse
# logarithmic scaling.
generate_model_pred <- function(model, data) {
  # Using the input model and input data, get the predicted value(s) rounded
  # to the nearest integer
  exp(stats::predict(model, data)) |>
    dplyr::pull(.pred) |> 
    round(0)

}

# Function to generate a ggplot object containing the LIME plot which
# explains feature importance for the particular observation passed to
# the input model
generate_lime_chart <- function(model, train_data, new_data) {
  set.seed(123)
  # Capture the new data being scored by the input 'model' object
  new_data <- new_data |> 
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
    train_data |> 
    dplyr::select(
      city, 
      bathrooms, 
      finishedSqFt, 
      yearBuilt, 
      cpipctchange, 
      season
    ), 
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
  ) |> 
    dplyr::mutate(prediction = round(exp(prediction), 0))

  # Create a plot of the top 3 features & explanations
  chart_data <- explanation |>
    dplyr::arrange(abs(feature_weight)) |>
    dplyr::mutate(
      feature_weight = round(feature_weight, 3),
      sign = dplyr::case_when(
        feature_weight < 0 ~ "Negative",
        TRUE ~ "Positive"
      ),
      color = dplyr::case_when(
        sign == "Positive" ~ "green",
        TRUE ~ "red"
      )
    )

    chart_data |>
      dplyr::rename("Feature Weight" = feature_weight) |>
      dplyr::group_by(sign) |>
      echarts4r::e_chart(x = feature_desc) |>
      echarts4r::e_bar_(serie = "Feature Weight", stack = "my_stack") |>
      echarts4r::e_flip_coords() |>
      echarts4r::e_tooltip() |> 
      echarts4r::e_legend(
        left = "middle",
        top = "bottom"
      ) |>
      echarts4r::e_title(
        glue::glue(
          "Prediction {scales::label_currency()(explanation$prediction[[1]])}"
        )
      ) |>
      echarts4r::e_y_axis(
        name = "Feature",
        nameTextStyle = list(
          fontWeight = "bold",
          fontSize = 15
        ),
        nameLocation = "middle",
        nameGap = 250,
        axisLabel = list(
          fontSize = 15
        )
      ) |> 
      echarts4r::e_color(
        # When we have both positive and negative values, unique(chart_data$color) returns c("green", "red").
        # Given that "Negative" comes before "Positive", "Negative" is assigned "green" and "Positive" "red".
        # For that reason, I add sort(decreasing = TRUE) to properly assign colors when both are present.
        # If I were to default to c("red", "green"), instances of only "Positive" would evaluate to "red".
        unique(chart_data$color) |> 
          sort(decreasing = TRUE)
      ) |> 
      echarts4r::e_grid(containLabel = TRUE)

}
