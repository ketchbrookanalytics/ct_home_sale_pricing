Two models were developed for use in this application. The final predicted value seen in the user interface is the result of taking the average of each model's predicted value for the sale price. This ensemble approach was found to yield the lowest error (RMSE and MAE).

# [Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)

The first model uses simple linear regression, which allows for a straightforward interpretation of the model coefficients, and each variable's impact on the output predicted value.

# [XGBoost](https://xgboost.readthedocs.io/en/latest/)

The second model uses a specific gradient boosting algorithm, called "XGBoost". There are both classification and regression interpretations of this algorithm, and the regression implementation was used for our home sale price prediction model. This algorithm produces a non-linear "black box" model, so the coefficients are not easily interpretable. This is where the power of the LIME technique shines, allowing us to gain some insight into how the model is making its predictions.