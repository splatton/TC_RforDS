#### Libraries -----------------------------------------

library(tidyverse)
library(tidymodels)

#### Read the data ------------------------

pt_volumes <- read_csv("pt_volumes.csv") |>
  mutate(DOS = mdy(DOS)) |>
  mutate(Monday = ifelse(wday(DOS) == 2, TRUE, FALSE),
         Season = case_when(month(DOS) %in% c(12,1,2) ~ "Winter",
                            month(DOS) %in% c(3,4,5) ~ "Spring",
                            month(DOS) %in% c(6,7,8) ~ "Summer",
                            .default = "Fall"))

#We created some categorical variables to work with, here. Now, we'll try to see if a given day of the week is a Monday.

#First, let's take a look at our data.

pt_volumes |>
  mutate(Weekday = as.factor(wday(DOS))) |>
  ggplot(aes(x = Weekday, y = Volume, fill = Weekday)) +
  geom_boxplot()

monday_model <- glm(Monday ~ Volume + Season, pt_volumes, family = "binomial")

summary(monday_model)

#Now let's take a look at how to gather the coefficients

exp(coef(monday_model))

#What does this mean?

#Can also use same techniques for logistic regression - splines, steps, interactions, etc

monday_model <- glm(Monday ~ Volume*FacilityName, pt_volumes, family = "binomial")

pt_volumes_pred <- pt_volumes |>
  mutate(Probs = predict(monday_model, pt_volumes, type = "response"),
         Log.Odds = predict(monday_model, pt_volumes)) 

#Now let's calculate a lift curve for our predictions

# Create deciles based on predicted probabilities
pt_volumes_lift <- pt_volumes_pred %>%
  mutate(decile = ntile(Probs, 10)) %>%  # Split into 10 groups
  group_by(decile) %>%
  summarise(
    total = n(),                               # Total cases in each decile
    actual_positives = sum(Monday),                 # Number of actual positives in each decile
    avg_pred_prob = mean(Probs)       # Average predicted probability
  )

# Calculate overall proportion of positives in the dataset
overall_positive_rate <- sum(pt_volumes_lift$actual_positives) / sum(pt_volumes_lift$total)

# Calculate lift for each decile
pt_volumes_lift <- pt_volumes_lift %>%
  mutate(lift = (actual_positives / total) / overall_positive_rate)

# Reverse order so highest probability decile is first
pt_volumes_lift <- pt_volumes_lift %>% arrange(desc(decile))

ggplot(pt_volumes_lift, aes(x = decile, y = lift)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Lift Curve for Logistic Regression",
       x = "Decile (1 = Highest Probability)",
       y = "Lift (Proportion of True Positives vs. Baseline)") +
  theme_minimal()

#Can weight a model

mean(pt_volumes_pred$Probs)

wts <- ifelse(pt_volumes$Monday, round(1/(mean(pt_volumes$Monday))), 1)

monday_model_wtd <- glm(Monday ~ Volume, pt_volumes, family = "binomial", weights = wts)

mean(predict(monday_model, type = "response"))

#Next we will use the yardstick package (part of tidymodels) to generate a confusion matrix

pt_volumes_pred <- pt_volumes_pred |>
  mutate(Wtd_Prob = predict(monday_model_wtd, type = "response")) |>
  mutate(Predicted_Class = ifelse(Wtd_Prob >= 0.5, TRUE, FALSE)) |>
  mutate(Predicted_Class = as.factor(Predicted_Class)) |>
  mutate(Monday = as.factor(Monday)) |>
  mutate(Weights = wts)

pt_volumes_pred |>
  conf_mat(truth = Monday, estimate = Predicted_Class) |>
  print()

#ROC curve

# Compute ROC Curve data
roc_curve_data <- pt_volumes_pred %>%
  roc_curve(Wtd_Prob, truth = Monday)

# Plot ROC Curve
autoplot(roc_curve_data)

pt_volumes_pred %>%
  roc_auc(truth = Monday, Probs)

#Let's use a slightly easier dataset to illustrate this

mtcars$am <- factor(mtcars$am, labels = c("automatic", "manual"))

mtcars_model <- glm(am ~ mpg + cyl, mtcars, family = "binomial")

roc_curve_data <- mtcars %>%
  mutate(Probs = predict(mtcars_model, type = "response")) |>
  mutate(Probs = 1-Probs) |>
  roc_curve(Probs, truth = am)

# Plot ROC Curve
autoplot(roc_curve_data)

#Another way we could make things simpler is to only try to distinguish Monday from Thursday (lowest volume) day.

pt_volumes_simple <- pt_volumes |>
  filter(wday(DOS) %in% c(2, 5))

monday_model_simple <- glm(Monday ~ Volume*Season*FacilityName, pt_volumes_simple, family = "binomial")

pt_volumes_pred <- pt_volumes_simple |>
  mutate(Probs = predict(monday_model_simple, type = "response")) |>
  mutate(Probs = 1- Probs) |>
  mutate(Predicted_Class = ifelse(Probs <= 0.5, TRUE, FALSE)) |>
  mutate(Predicted_Class = as.factor(Predicted_Class)) |>
  mutate(Monday = as.factor(Monday))

roc_curve_data <- pt_volumes_pred %>%
  roc_curve(Probs, truth = Monday)

# Plot ROC Curve
autoplot(roc_curve_data)

pt_volumes_pred %>%
  roc_auc(truth = Monday, Probs)
