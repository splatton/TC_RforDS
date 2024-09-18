#How to make different data types

x <- 1
y <- 0.5

class(x)
class(y)

z <- TRUE

#Now let's take a look at vectors.

number_vector <- c(0,1,2,3,4,5)

number_vector + x
number_vector * y
number_vector + number_vector
number_vector * number_vector

mixed_vector <- c(1, "boo", TRUE)

#Now we'll look at some common functions in R

mean(number_vector)
sd(number_vector)
median(number_vector)

round(y, 1)
round(y, 0)

#Now let's look at some missing data

#To do this, let's create a named vector

heights <- c(Steve = 72, Chris = NA, Peter = 78, John = 68)

#Let's take a moment to subset this vector and look at its length

heights[1]
heights[3]
heights['Peter']

length(heights)

#What is the mean? What if there were 1000 measurements?

1 + NA

x + NA

#Coercion

as.numeric(mixed_vector)

1 + TRUE

as.logical(mixed_vector)

as.logical(number_vector)

library(lubridate)

ymd("2024-4-5")
ydm("2024/4/5")

temp_date <- dmy("3-4-2024")

as.integer(temp_date)

as.Date(as.integer(temp_date))

as.Date(0)


