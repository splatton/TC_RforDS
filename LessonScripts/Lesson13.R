#### Today we will learn about regression, specifically linear regression.

library(tidyverse)
library(tidymodels)

#First, R has an important class of objects called formulas. They are written as follows: y ~ x1 + x2...

#Useful cheatsheet to R formula syntax: https://www.econometrics.blog/post/the-r-formula-cheatsheet/

test_formula <- y ~ x + z
class(test_formula)

#You can also change strings to formulas

class(as.formula("y ~ x + z"))

#Multiple other functions for modifying formulae in R but I recommend using strings and converting them to formulas.

original_formula <- "y ~ x + z"
new_formula <- as.formula(str_c(original_formula, "q", sep = "+"))

#Now let's take a look at the lm function - base R's function for linear modeling

#First, we read in the data

mktg_data <- read_csv("mktg_data.csv")
pt_volumes <- read_csv("pt_volumes.csv")

#Now we can briefly examine the data to make sure it is in the correct format and then join the two tibbles together

str(mktg_data)

mktg_data <- mktg_data |>
  mutate(DOS = mdy(DOS),
         Clicks = as.numeric(Clicks),
         Cost = as.numeric(Cost),
         Impr = as.numeric(Impr))

str(pt_volumes)

pt_volumes <- pt_volumes |>
  mutate(DOS = mdy(DOS)) |>
  rename(Facility = FacilityName)

joined_data <- mktg_data |>
  left_join(pt_volumes)

#I did encounter some errors with this join intermittently - this was due to coercion of the DOS column from a string to a date

#Now let's take a look at volumes and see if Google Ads spend is correlated with patient volumes

ggplot(joined_data, aes(x = Cost, y = Volume)) + geom_smooth() + geom_jitter(alpha = 0.2)

#For a better view, we can remove the jitter

ggplot(joined_data, aes(x = Cost, y = Volume)) + geom_smooth()

#Is this an example of linear regression? How could we find out?

ggplot(joined_data, aes(x = Cost, y = Volume)) + geom_smooth(method = "lm")
ggplot(joined_data, aes(x = Cost, y = Volume, color = Facility)) + geom_smooth(method = "lm", se = FALSE)
ggplot(joined_data, aes(x = Cost, y = Volume, color = Facility)) + geom_smooth(method = "lm", se = FALSE) + geom_jitter(alpha = 0.2) + coord_cartesian(ylim = c(4, 13))

#Generally, it appears that, as Cost increases, patient volumes also increase - at some facilities more than others. How could we describe this relationship?

cost_model <- lm(Volume ~ Cost, data = joined_data)

cost_model

summary(cost_model)

confint(cost_model)
confint(cost_model, level = 0.9)

coef(cost_model)

cost_model$residuals

residuals(cost_model)
predict(cost_model)

#Now let's use more variables - and maybe even add some more in!

joined_data <- joined_data |>
  mutate(Weekday = wday(DOS))

full_model <- lm(Volume ~ ., data = joined_data)

#We can use step-wise feature selection to use the AIC to select our best features

step(full_model, direction = "both")

#But weekday doesn't really make sense like this...nor does it make sense to include DOS

joined_data <- joined_data |>
  mutate(Weekday = as.factor(Weekday))

full_model <- lm(Volume ~ ., data = select(joined_data, -c(Campaign, DOS)))

step(full_model)

#What does this tell us about our model?

#Now let's look at how we can use cross-validation to both estimate our error and pick a good model

formula_list <- list(Volume ~ Cost, Volume ~ ., stats::step(full_model)$call$formula)

calculate_RMSE <- function(input_model, test_frame) {
  
  temp_frame <- augment(input_model, newdata = test_frame)
  temp_RMSE <- sqrt(mean(temp_frame$.resid^2, na.rm = TRUE))
  
  return(temp_RMSE)
  
}

k <- 10

RMSE_tibble <- tibble()

for(i in 1:length(formula_list)) {
  
  for(j in 1:k) {
    
    temp_split <- initial_split(select(joined_data, -Campaign))
    temp_model <- lm(formula_list[[i]], data = training(temp_split))
    
    temp_tib <- tibble(Terms = as.character(formula_list[[i]])[3], RMSE = calculate_RMSE(temp_model, testing(temp_split)))
    
    RMSE_tibble <- bind_rows(RMSE_tibble, temp_tib)
    
  }
  
}

#Let's try weighting our model now, by using the DOS

weighted_model <- lm(Volume ~ Cost, data = joined_data, weights = DOS)

#Could we make a better predictive model than this for guessing the volume at a facility?
