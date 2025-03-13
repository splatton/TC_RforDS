#Strings and Regex

#### Libraries --------------------------------------

library(tidyverse) #Loads the stringr package

#### Strings ---------------------------------------

#All stringr commands start with str. In addition, there is a handy stringr cheat sheet: https://rstudio.github.io/cheatsheets/strings.pdf

#Generally, if you get stuck in a command (like an unfinished string), hitting escape will finish that command

#To create a string, close it in single or double quotes. Whichever one 'bookends' your string will be the one that closes it out

#You can use an escape to make a string hold a \, ', or "

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

backslash <- "\\"

x <- c(single_quote, double_quote, backslash)
x

str_view(x)

#A raw string terminology allows you to not use escapes:

raw_example <- r"(Look! It's a raw \ string!'")"

#Can also use any number of dashes between the starting quote and parenthesis

#The backslash also lets you start a new line \n or make a tab \t or do special characters

#Now let's look at how to put strings together

str_c("Data science", "rules", "!", sep = " ")

str_c("Data science", "rules", "!", NA)

coalesce("Data science", "rules", "!", NA)

my_class <- "data science"

str_glue("My {my_class} class is awesome!")

#str_glue will not create NAs if you use missing values - this means you may miss NAs.

#str_flatten will merge all inputs into a single string - equivalent to adding the collapse = TRUE argument in str_c

exclamation <- "I love"

str_c(exclamation, c(my_class, "today"))

str_flatten(exclamation, c(my_class, "today"))

different_input <- c("I", "love", "data", "science")

str_c(different_input)

str_flatten(different_input)

#Now let's look at the separate functions - I just learned about these while reading this book!

df1 <- tibble(x = c("a,b,c", "d,e", "f"))

df1 |> 
  separate_longer_delim(x, delim = ",")

df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", "edition", "year")
  )

#Can use too_few or too_many arguments to handle problems with column alignment. Can also debug this way

#Now let's look at how to take a string apart

str_length(c("a", "R for data science", NA))

str_sub("TotalCare ER - Frisco", 4, 7)

#Can also count back from the end

str_sub("TotalCare ER - Frisco", 14, -1)

#Finally, on strings - English speakers have it easy! However, you can change the encoding when reading a csv or the locale for stringr functions. It is also possible to use hex code with escapes to specify a specific character

"\u00fc"

#### Regular Expressions -----------------------------------------

str_view(fruit, "berry") #Literal characters

str_view(fruit, "a...e")

# ab? matches an "a", optionally followed by a "b".
str_view(c("a", "ab", "abb"), "ab?")


# ab+ matches an "a", followed by at least one "b".
str_view(c("a", "ab", "abb"), "ab+")


# ab* matches an "a", followed by any number of "b"s.
str_view(c("a", "ab", "abb"), "ab*")

str_view(words, "[aeiou]x[aeiou]") #matches any of

str_view(words, "[^aeiou]y[^aeiou]") #matches any EXCEPT

#Alternation

str_view(fruit, "apple|melon|nut")

str_view(fruit, "aa|ee|ii|oo|uu")

#str_detect produces a vector of T/F

str_detect(c("a", "b", "c"), "[aeiou]")

#str_subset returns the string that matches a pattern, str_which returns the location in the vector of the string that matches the pattern

str_subset(str_to_title(fruit), "a")

#Remember that capital letters are different than lowercase ones

#Str_replace, str_replace_all, str_remove, str_remove_all are all similar functions

x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")

#We can also use separate_wider_regex to separate a single column into multiple based on regex

df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84", 
)

df |> 
  separate_wider_regex(
    str,
    patterns = c(
      "<", 
      name = "[A-Za-z]+", 
      ">-", 
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )

#In using escapes, if you want to create a string with an escape in it, you have to escape the escape

dot <- "\\."

#Anchors allow you to match the start or end of a string

str_view(fruit, "^a")

str_view(fruit, "a$")

#\b allows you match word boundaries

str_replace_all("abc", c("$", "^", "\\b"), "--")

#Character classes allow you to look for specific sets of characters

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "[^a-z0-9]+")

#Can use parentheses for order of operations for regex

