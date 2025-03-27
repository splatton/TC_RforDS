#### Dates and Times

#### Libraries -----------------------------

library(tidyverse)

#Handy functions

Sys.Date()

Sys.time()

now()

today()

#ISO8601 is the international standard for writing date-times - written largest to smallest

as_date("2022-1-5", "%y/%m/%d")

#ymd, mdy, dmy etc

#ymd_hms

#Can add tz argument

#Can make dates or datetimes

time_tib <- tibble(Date = c("2025-1-1", "2025-3-31", "2025-3-2"), Time = c("4:00", "5:35", "14:13"))

time_tib |> mutate(DT = make_datetime(Date, Time))

time_tib2 <- tibble(Year = c(2001, 2022, 2023), Month = c(1,2,3), Day = c(1, 23, 31))

time_tib2 |> mutate(Date = make_date(Year, Month, Day))

#as_date and as_datetime switch between the two

#year, month, day functions; yday, wday function - these functions can take arguments label and abbr

#can round with floor_date, ceiling_date, round_date

#can modify values with the same year/month/day functions

test_date <- mdy("2-28-2032")
year(test_date) <- 2030

update(test_date, year = 2022, month = 13)

#difftime - can be painful

#as.duration always in seconds

#can add and multiply durations (cannot multiply by each other)

one_am <- ymd_hms("2026-03-08 01:00:00", tz = "America/New_York")

#periods

one_am

one_am + ddays(1)

one_am + days(1)

#Can use other similar functions

#Intervals

y2023 <- ymd("2023-01-01") %--% ymd("2024-01-01")
y2024 <- ymd("2024-01-01") %--% ymd("2025-01-01")

y2023

y2024

y2023 / days(1)

y2024 / days(1)

#Timezones - suck

Sys.timezone()

OlsonNames()

#with_tz function changes the display to more numeric format


#### Missing Values

treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)

treatment |>
  fill(everything())
#LOCF, can use .direction argument

#coalesce and na_if

#NaN

#Explicit and implicit missing values

stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

#Can pivot to make explicit

stocks |>
  complete(year, qtr)

stocks |>
  complete(year = 2019:2021, qtr)

#'filtering' joins, anti-join

#Empty factors

health <- tibble(
  name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"),
  smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
  age    = c(34, 88, 75, 47, 56),
)

health |> count(smoker, .drop = FALSE)

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete()

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)
