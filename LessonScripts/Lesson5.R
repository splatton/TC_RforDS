#First, we load our libraries

library(tidyverse)

#Three different representations of the same data

table1

table2

table3

#What makes these datasets tidy or not?

#Pivot Longer

billboard

billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank"
  )

#A common error is to forget the quotes around the variables when you're pivoting longer

#Let's look at the number of NAs in our data

billboard |>
  summarise_if(is.numeric, ~ sum(is.na(.x))) |>
  pivot_longer(is.numeric, names_to = "Week", values_to = "NA_Count") |>
  mutate(Week = parse_number(Week)) |>
  ggplot(aes(x = Week, y = NA_Count)) +
  geom_bar(stat = "identity")

#We will talk about the ~ notation later

billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )

billboard_longer |> 
  ggplot(aes(x = week, y = rank, group = track)) + 
  geom_line(alpha = 0.25) + 
  scale_y_reverse()

#You can also pivot into multiple column names

who2

who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"), 
    names_sep = "_",
    values_to = "count"
  )

#Let's look at more complex information in the column names

household

household |> 
  pivot_longer(
    cols = !family, 
    names_to = c(".value", "child"), 
    names_sep = "_", 
    values_drop_na = TRUE
  )

#Or...

household |>
  mutate_all(as.character) |>
  pivot_longer(
    cols = !family,
    names_to = "Variable",
    values_to = "Data"
  ) |>
  mutate(Child = parse_number(Variable)) |>
  rowwise() |>
  mutate(Variable = str_split(Variable, "_")[[1]][[1]]) |>
  pivot_wider(names_from = Variable, values_from = Data)