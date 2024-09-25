#First, a note on packages...

#When you load a package, you will sometimes get conflicts. If you want to call a function from a SPECIFIC package (even if there are conflicts), you can use the terminology package::function() to call this.

#You can also use this notation WITHOUT loading the package (though it must be installed)

flights <- nycflights13::flights

#OR

library(nycflights13)
data(flights)
force(flights)

#Now let's take a quick look at this object

flights
view(flights)
str(flights)
glimpse(flights)

#The dplyr package allows us to transform data - this is my FAVORITE package

#But FIRST, let's talk about the pipe

flights |>
  select(day)

#Whoops! We need to load the tidyverse. Or just dplyr!

#Did this operation change the flights tibble?

#We could do this by assigning the output to a variable

flights_adjusted <- flights |>
  filter(day == 1) |>
  select(c(carrier, dep_delay))

#First, we'll look at operations on rows - filter

#Look above - we used a logical operator

1 == 1
1 >= 2
2 > 1
3 != 4
3 == 4
!(3 == 4)
3 <= 4
(3 == 4) | (1 == 1)
(3 == 4) & (1 == 1)
1 %in% 1
1 %in% c(1,2,3,4,5)
1 %in% c(2,3,4,5)
!(1 %in% c(2,3,4,5))
'Tom' == 'Tom'
'Tom' == 'John'

#Single '=' is similar to the assignment operator

flights_adjusted <- flights |>
  filter(day = 1)

#Whoops!

jan1 <- flights |> 
  filter(month == 1 & day == 1)

#NEXT - arrange (also operates on rows)

flights |> 
  arrange(year, month, day, dep_time)

flights |> 
  arrange(desc(dep_delay))

#Next - distinct

#Can call without specifying columns

flights |> 
  distinct()

flights |> 
  distinct(origin, dest)

#Or, if you want to keep the other columns

flights |> 
  distinct(origin, dest, .keep_all = TRUE)

#Count function

flights |>
  count(origin, dest, sort = TRUE)

#We can also operate on columns

#MUTATE

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

#Select - pick the columns you want!

flights |> 
  select(year, month, day)

flights |> 
  select(year:day)

flights |> 
  select(!year:day)

flights |> 
  select(where(is.character))

flights |> 
  select(tail_num = tailnum)

#Rename

flights |> 
  rename(tail_num = tailnum)

#Relocate

flights |> 
  relocate(time_hour, air_time)

flights |> 
  relocate(year:dep_time, .after = time_hour)
flights |> 
  relocate(starts_with("arr"), .before = dep_time)
