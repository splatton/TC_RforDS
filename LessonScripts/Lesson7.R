#First, let's take a look at a CSV file without reading it into R.

library(tidyverse)

###https://covid.cdc.gov/covid-data-tracker/index.html#ed-visits_separated_by_age_group

#Take a look at this file in Excel or Google Sheets

#Let's also see how we might be able to export a csv from Google Sheets

#Try to read in the CDC data - what does it look like?

#Can also read in the data from the book

students <- read_csv("https://pos.it/r4ds-students-csv")

#Show how to find a path for a file

students <- read_csv("data/students.csv", na = c("N/A", ""))

#Take a look at the names with spaces - these have to be referred to with the back quotes

students |> 
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

?read_csv

#Can also use the janitor package

students |> janitor::clean_names()

#Let's take a look at the data now, especially the data types

students |>
  janitor::clean_names() |>
  mutate(meal_plan = factor(meal_plan))

students <- students |>
  janitor::clean_names() |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )

ifelse(TRUE, "excellent", "bad")

ifelse(FALSE, "excellent", "bad")

ifelse(5 < 2, "excellent", "bad")

x <- "bad at math"
ifelse(5 < 2, "excellent", ifelse(x == 'bad at math', "meh", "oh man"))

#Read_csv can also read text formatted like a csv

read_csv(
  "a,b,c
  1,2,3
  4,5,6"
)

#The skip argument is useful for headers

read_csv(
  "The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3",
  skip = 2
)

#Could we use this with the skip argument?

#Can also use the comment argument

read_csv(
  "# A comment I want to skip
  x,y,z
  1,2,3",
  comment = "#"
)

#Sometimes files will not have the column titles as part of the file

read_csv(
  "1,2,3
  4,5,6",
  col_names = FALSE
)

read_csv(
  "1,2,3
  4,5,6",
  col_names = c("x", "y", "z")
)

#Now let's take a look at column types

simple_csv <- "
  x
  10
  .
  20
  30"

df <- read_csv(
  simple_csv, 
  col_types = list(x = col_double())
)

#Call problems to identify problems

#There are multiple column types you can specify

#Can also change the default column type if read_csv can't identify it

another_csv <- "
x,y,z
1,2,3"

read_csv(
  another_csv, 
  col_types = cols(.default = col_character())
)

#Can also specify to only read specific columns

read_csv(
  another_csv,
  col_types = cols_only(x = col_character())
)

#Tibble and tribble function allow you to build tabular data on the fly

tibble(
  x = c(1, 2, 5), 
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)

tribble(
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
)

#Let's take another look at the CDC data

cdc_data <- cdc_data |>
  mutate(MonthYear = str_c(as.character(month(Date)),
                           as.character(year(Date)),
                           sep = "_"))

#Now we can make a new directory to keep these files

dir.create("cdc_data")

#Now let's write some code to break this into specific months and then create files for each month and place them in the new directory

unique_combos <- unique(cdc_data$MonthYear)

unique_combos[2]

for(i in 1:length(unique_combos)) {
  
  #First we create a filename to use for writing in the data
  
  temp_filename <- str_c("cdc_data/", unique_combos[i], ".csv")
  
  #Then, we'll filter our data to create a df for each month/year combo
  #Now we can write this file to the directory using write_csv
  
  cdc_data |>
    filter(MonthYear == unique_combos[i]) |>
    write_csv(temp_filename)
  
}

#Now let's check to make sure we did it right!

#Could also use rds files write_rds or read_rds to save formatting - also easier to read, but only readable in R

#But what if we had a directory like this already? How could we read it in?

cdc_files <- list.files("cdc_Data", full.names = TRUE)
cdc_files

total_cdc <- read_csv(cdc_files)

#What could we do with this data?

cdc_prophet <- cdc_data |>
  rename(ds = Date,
         y = Daily.Percent.of.ED.Visits.with.diagnosed.COVID.19) |>
  filter(Age.Range == "All Ages") |>
  select(c(ds, y))

#Homework: send me a graph of the forecasted COVID-19 volumes for next year.

#https://facebook.github.io/prophet/docs/quick_start.html#r-api