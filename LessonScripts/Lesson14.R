#Libraries

library(tidyverse)

#The forcats library is already part of the tidyverse package

#Let's read in some data to mess with. And then let's create some categorical variables.

pt_volumes <- read_csv("pt_volumes.csv")

#look at the data

str(pt_volumes)

#Now let's make some categorical variables that we might want to check out.

pt_volumes <- pt_volumes |>
  mutate(DOS = mdy(DOS),
         Month = month(DOS),
         Year = year(DOS),
         Classification = case_when(Volume >= quantile(pt_volumes$Volume, probs = 0.75) ~ "High",
                                    Volume <= quantile(pt_volumes$Volume, probs = 0.25) ~ "Low",
                                    .default = "Medium"))

#Now we should examine our data once more...

pt_volumes <- pt_volumes |>
  mutate(FacilityName = fct(FacilityName),
         Month = fct(Month),
         Year = fct(Year),
         Classification = fct(Classification))

pt_volumes <- pt_volumes |>
  mutate(FacilityName = fct(FacilityName),
         Month = as.factor(Month),
         Year = as.factor(Year),
         Classification = fct(Classification))

#Let's see how many of the days meet the individual classification requirements

pt_volumes |>
  count(Classification)

#Now let's rank the months by their average volumes

pt_summary <- pt_volumes |>
  group_by(Month) |>
  summarize(
    Avg_Vol = mean(Volume, na.rm = TRUE),
    n = n()
  )

ggplot(pt_summary, aes(x = Avg_Vol, y = Month)) +
  geom_point()

#For plotting, we can reorder the factor levels

ggplot(pt_summary, aes(x = Avg_Vol, y = fct_reorder(Month, Avg_Vol))) +
  geom_point()

#Can also use fct_relevel to change the order of a specific level

ggplot(pt_summary, aes(x = Avg_Vol, y = fct_relevel(Month, "12"))) +
  geom_point()

#Can also reorder factors on a line graph

daily_prop <- pt_volumes |>
  group_by(DOS) |>
  mutate(All_Volume = sum(Volume)) |>
  ungroup() |>
  mutate(Daily_Prop = Volume/All_Volume)

ggplot(daily_prop, aes(x = DOS, y = Daily_Prop, color = FacilityName)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  scale_color_brewer(palette = "Set1")

ggplot(daily_prop, aes(x = DOS, y = Daily_Prop, color = fct_reorder2(FacilityName, DOS, Daily_Prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "Facility")

#Another way to reorder

pt_volumes |>
  mutate(Classification = Classification |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = Classification)) +
  geom_bar()

#Can imposed factors as ordered factors - can be used in ordinal logistic regression

ordered(c("1", "2", "3"))

#Can recode factors

pt_volumes <- pt_volumes |>
  mutate(Month = fct_collapse(Month,
                              "Winter" = c(12,1,2),
                              "Spring" = c(3,4,5),
                              "Summer" = c(6,7,8),
                              "Fall" = c(9,10,11)))

pt_summary <- pt_volumes |>
  group_by(Month) |>
  summarize(
    Avg_Vol = mean(Volume, na.rm = TRUE),
    n = n()
  )

ggplot(pt_summary, aes(x = Avg_Vol, y = fct_reorder(Month, Avg_Vol))) +
  geom_point()
