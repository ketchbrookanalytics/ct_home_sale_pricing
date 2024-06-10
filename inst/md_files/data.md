There are multiple sources of data that were used in developing the models. The final models were trained using 5,260 observations across 7 column variables, including the dependent variable, sale price.

Data was gathered from the following sources:

- [Connecticut Open Data](https://data.ct.gov/Housing-and-Development/Real-Estate-Sales-2001-2021-GL/5mzw-sjtu/about_data): includes data about historical home sales in Connecticut, accessed via the [RSocrata API](https://github.com/Chicago/RSocrata)
- [Zillow](https://www.zillow.com/): retrieved data about home details (number of bedrooms, number of bathrooms, etc.), accessed via the [realEstAnalytics interface to the Zillow API](https://github.com/stharms/realEstAnalytics.r)

Here are the final variables chosen & engineered for these models, along with their descriptions (Note: you may be surprised that the number of bedrooms was not used. This omission was due to high collinearity between the number of bathrooms, and the number of bathrooms were found to be more predictive of the sale price than the number of bedrooms.):

- **City**: a categorical independent variable representing which one of the 12 unique towns/cities in Tolland County, Connecticut, that the sale occurred. In the app, the user has the ability to choose the value (city) that they want the model to use in its prediction calculation.
- **Bathrooms**: a continuous independent variable representing the number of bathrooms in the home at the time of the sale. This variable had values between 1 and 7. In the app, the user has the ability to choose the value (number of bathrooms) that they want the model to use in its prediction calculation.
- **Square Footage**: a continuous independent variable representing the finished square footage of the home at the time of the sale. This variable had values between 462 and 7,168. In the app, the user has the ability to choose the value (square footage) that they want the model to use in its prediction calculation.
- **Year Built**: a continuous independent variable representing the year the home was built. This variable had values between 1694 and 2017. In the app, the user has the ability to choose the value (year built) that they want the model to use in its prediction calculation.
- **Consumer Price Index (% Change from 12 Months Prior)**: a continuous independent variable representing the percent change in the Consumer Price Index between the time of the sale and the Consumer Price Index 12 months prior. The purpose of this variable was to serve as a proxy variable for the year of the sale, and to help account for some of the time series elements seen in the raw data (e.g., increasing trend in home sale prices over time). In the app, this value is generated behind the scenes. When a user opens the app, the most recent data for the Consumer Price Index is pulled in via the FRED API that they want the model to use in its prediction calculation.
- **Season**: a categorical independent variable representing the season during which the sale occurred. This variable was engineered based upon the date of the sale. Sales in December, January, or February were coded to a value of "Winter". Sales that took place in March, April, or May were coded to a value of "Spring". Sales that took place in June, July, or August were coded to a value of "Summer". Sales that took place in Septebmer, October, or November were coded to a value of "Fall". In the app, this value is generated behind the scenes. When a user opens the app, the system date is capture and translated into the season based upon the month of the system date for the model to use in its prediction calculation.
- **Sale Price**: the numeric idependent variable representing the sale price. For data preparation purposes (specifically for joining to the Zillow data, which represents only current home data), only the most recent sale price for a given address was used. Additionally, a log transformation was applied to this variable for helping to satisfy statistical normality & stationarity assumptions. Consequently, the model output gets transformed via the exponential function prior to consumption by the end user. In the training data, this dependent variable had values between 100,000 and 960,000. In the app, this is the output "prediction" of the sale price returned to the user.

There were some additional data preparation steps that were taken in the training data:

- Only residential properties were included.
- Sales with a non-use code were excluded. For more information, visit the Connecticut Open Data link above.
- Only sales between $100,000 and $1,000,000 were included, to ensure stabality against outliers.
- Only single- or multi-family homes were included.
- Only residential properties were included.
- Towns with less than 100 observations (sales) in the dataset were excluded.
- Only properties with between 1 and 7 bathrooms were included, to ensure stabality against outliers.
- Only properties with finished square footage betwen 400 and 10,000 were included, to ensure stabality against outliers.
- Only properties with finished square footage betwen 400 and 10,000 were included, to ensure stabality against outliers.
