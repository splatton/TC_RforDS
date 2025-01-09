# As much as we can, we will try to work with real TotalCare data.

#### Libraries ------------------------

library(tidyverse)

#### Reading in the data ------------------
# Download the files from the Looker dashboard and label them so you can find them. Your filenames may be different from mine!

mktg_data <- read_csv("mktg_data.csv") |>
  mutate(Clicks = as.numeric(Clicks),
         Cost = as.numeric(Cost),
         Impr = as.numeric(Impr)) |>
  mutate(DOS = mdy(DOS))

str(mktg_data)

#### Logical Vectors ------------------

#Filtering is an example of using a logical vector

#For instance, both of these are equivalent

mktg_filtered <- mktg_data |>
  filter(Facility == "TotalCare ER - Frisco" & Cost >= 150)

mktg_filtered <- mktg_data |>
  mutate(Frisco = Facility == "TotalCare ER - Frisco",
         GreaterThan150 = Cost >= 150) |>
  filter(Frisco & GreaterThan150) |>
  select(-c(Frisco, GreaterThan150))

#Special Case - Rounding of printed values

x <- c(1 / 49 * 49, sqrt(2) ^ 2)
x

x == c(1, 2)

print(x, digits = 16)

near(x, c(1, 2))

#Missing values

NA == NA
NA != NA

filter(mktg_data, Cost == NA)

filter(mktg_data, is.na(Cost))

arranged_mktg <- mktg_data |>
  arrange(desc(is.na(Cost)), Cost)

#Logical operations

NA | TRUE
NA | FALSE

NA & TRUE
NA & FALSE

as.logical(0)
as.logical(-1)
as.logical(12)

NA %in% NA

c(1, 2, NA) == NA
c(1, 2, NA) %in% NA

#Any and all

mktg_summary <- mktg_data |>
  group_by(DOS) |>
  summarize(AnyHighCost = any(Cost > 150),
            AllHighCost = all(Cost > 150))

#Can also use mean and sum - TRUE defaults to being counted as 1, FALSE is 0

#Can also subset based on logical vectors

mktg_data$Cost[mktg_data$Cost > 300 & !is.na(mktg_data$Cost)]

#if_else, case_when

if_else(TRUE, "Condition true", "Condition false")
if_else(FALSE, "Condition true", "Condition false")

#Note if_else requires compatible types

mktg_data_costs <- mktg_data |>
  mutate(
    RelativeCost = case_when(
      Cost <= quantile(Cost, probs = 0.3, na.rm = TRUE) ~ "Low",
      Cost > quantile(Cost, probs = 0.7, na.rm = TRUE) ~ "High",
      Cost > quantile(Cost, probs = 0.3, na.rm = TRUE) ~ "Medium",
      .default = "Unknown"
    ))

#### Numbers -------------------------------

#Parsing numbers

parse_double("1.3")
parse_number("$1.30 USD")

#Counting

mktg_data |>
  count(DOS) |>
  filter(n != 9)

#Can do same function with summarize and n()

mktg_data |>
  group_by(DOS) |>
  summarize(n = n()) |> #The variable does not HAVE to be named n, but the FUNCTION does!
  filter(n != 9)

n_distinct(mktg_data$Facility)

#Vector recycling

x <- c(1,2,3)
y <- c(1,2,3,4,5,6,7)

x+y

#Pmin and Pmax

df <- tribble(
  ~x, ~y,
  1,  3,
  5,  2,
  7, NA,
)

df |> 
  mutate(
    min = pmin(x, y, na.rm = TRUE),
    max = pmax(x, y, na.rm = TRUE)
  )

mktg_high <- mktg_data |>
  mutate(HighVal = pmax(Cost, Budget, na.rm = TRUE))

#Modular arithmetic

1:10 %/% 3

1:10 %% 3

#Logs

mktg_log <- mktg_data |>
  mutate(Log.Budget = log10(Budget)) |>
  group_by(DOS) |>
  summarize(Tot.Budget = sum(Budget)/1000, Tot.Log.Budget = sum(Log.Budget)) |>
  pivot_longer(!DOS, names_to = "Measure", values_to = "Value")

ggplot(mktg_log, aes(x = DOS, y = Value, color = Measure)) + geom_line()

#Rounding

mktg_decimals <- mktg_data |>
  mutate(Decimals = Budget %% 1) |>
  filter(Decimals > 0)

mktg_decimals <- mktg_decimals |>
  mutate(Rd.Budget = round(Budget, 1))

#Can use floor or ceiling
#Can also make multiples of a specific number

round(mktg_data$Budget / 50) * 50

#Cut function

cut(x, 
    breaks = c(0, 5, 10, 15, 20), 
    labels = c("sm", "md", "lg", "xl")
)

#R also provides for cumulative functions

#R will also let you rank or number rows

mktg_data |>
  mutate(Row.Num = row_number())
