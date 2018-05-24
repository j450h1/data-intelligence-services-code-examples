# config ------------------------------------------------------------------
## load packages
library(googleAuthR)
library(bigQueryR)
library(dplyr)
library(prophet)
library(ggplot2)
library(plotly)
library(scales)

## set variables for ease of changing, re-running analysis
dataset_name <- "cpc_revenue_by_date"
train_start <- "2016-08-01"
train_end <- "2017-07-23"
valid_start <- "2017-07-24"
valid_end <- "2017-07-30"
forecast_period <- 7

## set options for authentication
## Setup instructions here: http://code.markedmondson.me/googleAnalyticsR/big-query.html
options(googleAuthR.client_id = "XXXXXXXXXXXXXXXX") # SET CLIENT ID
options(googleAuthR.client_secret = "XXXXXXXXXXXXXXXX") # SET CLIENT SECRET 
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/cloud-platform"))

## authenticate
gar_auth()

# extract -----------------------------------------------------------------
## set parameters for easier interpretation/reuse later
project <- "XXXXX" # SET GCP PROJECT ID 
dataset <- "google_analytics_sample"
query <- "#legacySQL 
          SELECT 
          DATE(date) AS date, 
          (trafficSource.medium) AS medium,
          ROUND(SUM(IFNULL(totals.transactionRevenue/100000,0)),2) AS transactionRevenue
          FROM (TABLE_DATE_RANGE([bigquery-public-data:google_analytics_sample.ga_sessions_], 
                                 TIMESTAMP('2016-07-01'), 
                                 TIMESTAMP('2017-08-31')))
          GROUP BY
          date,
          medium
          HAVING
          medium = 'cpc'
          ORDER BY
          date ASC,
          transactionRevenue DESC"

## SMALL results (under ~ 100000 rows)
bq_raw_data <- bqr_query(projectId = project, 
                         datasetId = dataset, 
                         query = query,
                         useLegacySql = TRUE)

## save original dataset as csv to resume work later if our session crashes
write.csv(bq_raw_data, paste0(dataset_name, ".csv"), row.names = FALSE)

# model -------------------------------------------------------------------
## load data
bq_raw_data <- read.csv(paste0(dataset_name, ".csv"), 
                        stringsAsFactors = FALSE)
## explore data 
### create data frame for exploration and modelling to preserve orginal 
data <- bq_raw_data %>% 
  select(ds = date, y = transactionRevenue) %>% 
  mutate(ds = as.Date(ds)) 

## visualize to idenitfy outliers if needed later on 
p <- ggplot(data, aes(ds, y)) + geom_line() 
ggp <- ggplotly(p)
ggp

### create training dataset
train_data <- data %>% 
  filter(ds >= train_start & ds <= train_end)

### create validation dataset 
validation_data <- data %>% 
  filter(ds >= valid_start & ds <= valid_end)

## forecast - basic ------------------------------------------------
###  fit the model on the dataframe 
m <- prophet(train_data)
future <- make_future_dataframe(m, periods = forecast_period)

### deploy model using receive predictions for the length of days required
forecast <- predict(m, future)

### visualize forecast result 
plot(m, forecast)

### view raw data with predicted value by day and uncertainty intervals 
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")], n = forecast_period * 2)

## remove outliers ---------------------------------------------------------
## create vector of outliers and convert outliers to NA 
## to improve forecast model accuracy 
outlier_dates <- c("2016-09-28", 
                   "2016-11-30", 
                   "2016-12-07", 
                   "2017-04-03", 
                   "2017-04-06")
outliers <- train_data$ds %in% as.Date(outlier_dates)
train_data$y[outliers] = NA 

## check to ensure outliers are NA 
train_data %>%
  filter(is.na(y))

## forecast - final ----------------------------------------------------------
###  fit the model on the dataframe 
m <- prophet(train_data)
future <- make_future_dataframe(m, periods = forecast_period)

### deploy model using receive predictions for the length of days required
forecast <- predict(m, future)

### visualize forecast result 
plot(m, forecast)

### view raw data with predicted value by day and uncertainty intervals 
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")], n = forecast_period * 2)

## results -------------------------------------------------------------------
### calculate and print total forecasted 
total_forecast <- forecast %>% 
  filter(ds >= valid_start & ds <= valid_end) %>% 
  summarise(transactionRevenue = sum(yhat))

dollar(total_forecast$transactionRevenue)

### calculate and print total actual revenue 
total_actual <- validation_data %>% 
  filter(ds >= valid_start & ds <= valid_end) %>% 
  summarise(transactionRevenue = sum(y))

dollar(total_actual$transactionRevenue)

### compare forecast vs actual 
difference <- ((total_actual$transactionRevenue - total_forecast$transactionRevenue) 
               / total_forecast$transactionRevenue )

### calculate percent difference/error in forecast vs actual
percent(difference)
