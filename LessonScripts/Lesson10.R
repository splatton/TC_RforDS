#Let's think about what we've learned so far. Unmute your mics and we'll dive in!

#### Libraries ---------------------------------

library(tidyverse)

#### Load the data -----------------------------

pt_volumes <- read_csv("pt_volumes.csv") |>
  mutate(DOS = mdy(DOS)) |>
  mutate(Month = month(DOS))

month_summary <- pt_volumes |>
  group_by(Month, FacilityName) |>
  summarize(Volume = sum(Volume))

mktg_data <- read_csv("mktg_data.csv") |>
  mutate(DOS = mdy(DOS)) |>
  rename(FacilityName = Facility)

#### Explore the question: how do volumes differ in between facilities overall and for the same date?

#### Variation within the data


ggplot(filter(pt_volumes, DOS >= Sys.Date() - days(30)), aes(x = DOS, y = Volume, color = FacilityName)) + geom_line()

ggplot(month_summary, aes(x = Month, y = Volume, color = FacilityName)) + geom_line()

ggplot(pt_volumes, aes(x = DOS, y = Volume, color = FacilityName)) + geom_line() + geom_smooth(method = "lm", color = "black") + facet_wrap(~FacilityName)

#### covariation

#Cost to volume at Cedar Hill

cedar_hill_data <- pt_volumes |>
  filter(FacilityName == "TotalCare ER - Cedar Hill") |>
  select(-FacilityName)

ch_mktg <- mktg_data |>
  filter(FacilityName == "TotalCare ER - Cedar Hill") |>
  select(-FacilityName)

ch_data <- ch_mktg |>
  left_join(cedar_hill_data)

ggplot(ch_data, aes(x = Cost, y = Volume)) + geom_point()
