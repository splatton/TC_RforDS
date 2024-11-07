#Let's pick up where we left off last time with forecasting the CDC data.

library(tidyverse)

library(prophet)

cdc_prophet <- read_csv("cdc_data.csv", name_repair = "universal", skip = 2) |>
  rename(ds = Date,
         y = Daily.Percent.of.ED.Visits.with.diagnosed.COVID.19) |>
  filter(Age.Range == "All Ages") |>
  select(c(ds, y))

cdc_prophet <- cdc_prophet |>
  mutate(cap = 5)

prophet_model <- prophet(cdc_prophet, seasonality.mode = "multiplicative", growth = "logistic", yearly.seasonality = TRUE)

future <- make_future_dataframe(prophet_model, periods = 365) |>
  mutate(floor = 0) |>
  mutate(cap = 5)

forecast <- predict(prophet_model, future)

plot(prophet_model, forecast)

#Now to reprex...

#Can copy to clipboard then call function reprex()

#After calling reprex, github markdown text in clipboard
