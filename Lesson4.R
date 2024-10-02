#### Libraries ---------------

library(tidyverse)

#### An Experiment ----------------

#Can we figure out how likely we are to win a dice roll using two six-sided die?

number_of_tests <- 1000

possible_rolls <- 2:12

dice_outcome <- tibble() |>
  add_column(Dice_Roll = as.numeric(NA), Outcome = as.character(NA))

for (i in 1:length(possible_rolls)) {
  
  for (j in 1:number_of_tests) {
    
    single_roll <- sum(sample(1:6, size = 2, replace = TRUE))
    
    single_outcome <- ifelse(single_roll > possible_rolls[i], 'Loss',
                             ifelse(single_roll == possible_rolls[i], 'Tie', 'Win'))
    
    dice_outcome <- dice_outcome |>
      add_row(Dice_Roll = possible_rolls[i], Outcome = single_outcome)
    
  }
  
}

#Now let's graph our results

ggplot(dice_outcome, aes(x = as.factor(Dice_Roll), fill = Outcome)) +
  geom_bar(position = "fill") +
  theme(panel.background = element_blank()) +
  labs(x = 'Dice Roll', y = '', title = 'Two-Dice Roll Challenge Outcomes')

#### More on dplyr -------------------------

#Load flights

flights <- nycflights13::flights

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay)
  )

#Try again....

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )

#Group by multiple variables

daily <- flights |>  
  group_by(year, month, day)
daily


daily_flights <- daily |> 
  summarize(n = n())

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
  )

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop"
  )

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "keep"
  )

#Now let's look at ungroup

#And the .by argument

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = month
  )

#Slice

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |>
  relocate(dest)

#Baseball data





