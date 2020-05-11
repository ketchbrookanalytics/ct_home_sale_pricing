# Home Sale Price Estimator for Tolland County, Connecticut
 Repo for materials associated with the pricing model developed for homes in Tolland County, Connecticut 
 
 The accompanying Shiny application can be found here: [https://ketchbrookanalytics.shinyapps.io/tolland-county-home-sale-pricing-app/](https://ketchbrookanalytics.shinyapps.io/tolland-county-home-sale-pricing-app/)  
 
This application uses data on historical home sale prices in Tolland County, Connecticut, and two models fit using linear regression & XGBoost regression to predict the sale price of a home sold today given parameter values entered by the user.  The resultant predicted value is the *mean* of the two model output values.  

Additionally, [LIME](https://lime.data-imaginist.com/), or **Local Interpretable Model-Agnostic Explanations** is employed to help the audience understand which parameters are having the biggest impact on the model predictions given the values entered for each parameter.  Note, there are also macroeconomic variables pulled in from API sources whenever the app is running (the user has no interaction with these via the User Interface input selections, but they may see these in the LIME charts).  
