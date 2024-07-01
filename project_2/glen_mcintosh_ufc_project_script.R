#############################################################################
# Project 2: UFC Winner Predictions (10 points of 25 total points)
# DUE: May 5, 11:59pm
#
### CLEAR YOUR RSTUDIO ENVIRONMENT. RESTART YOUR R SESSION. ###
#
# Follow all project instructions EXACTLY, no extensions will be given
#
# Before answering project questions on canvas, clear the Environment, Clear the
#  console and restart your R session, then highlight ALL of your code and run it. 
#  Look in the console for output and/or errors
#
# Before downloading your code, SAVE your script in Liberator
#############################################################################
# Author: Glen McIntosh

##### Getting Ready #####

# Load Libraries:
library(rpart)
library(rpart.plot)
library(ggplot2)

# Read in Data:
ufc <- read.csv("ufc_project.csv")



##### 2. Split Up the Data #####

N <- nrow(ufc)

print(N)

set.seed(052024)

sample_size <- round(0.10 * N)

test_rows <- sample(1:N, size = sample_size, replace = FALSE)

test_data <- ufc[test_rows, ]
train_data <- ufc[-test_rows, ]

print(nrow(test_data))

original_row_number <- test_rows[458]

print(original_row_number)

##### 3. Data Visualization #####

# Plot 1:
ggplot(train_data, aes(x = "", fill = winner)) +
  geom_bar(width = 0.5, color = "black") +
  scale_fill_manual(values = c("Red" = "red", "Blue" = "blue")) +
  ggtitle("Red vs Blue winners in training set") +
  facet_wrap(~weight_class)


# Plot 2:
ggplot(train_data) +
  geom_point(aes(x = b_SS_acc, y = b_SS_def_pct), color = "blue", alpha = 0.25) +
  geom_point(aes(x = r_SS_acc, y = r_SS_def_pct), color = "red", alpha = 0.25) +
  facet_wrap(~ winner) +
  ggtitle("Blue vs Red Fighter SS Accuracy vs. SS Defense") +
  xlab("Significant Strikes Accuracy") +
  ylab("Significant Strikes Defense Percentage") +
  theme_minimal()




##### 4. Model Building #####

# Model 1:

ufc_mod_1 <- rpart(winner ~ r_stance + r_reach + r_weight + r_SS_acc + r_SS_def_pct + b_stance + b_reach + b_weight + b_SS_acc + b_SS_def_pct, train_data)
predictions <- predict(ufc_mod_1, test_data, type = "class")
conttab1 <- table(actual = test_data$winner, predicted = predictions)
print(conttab1)

correct_predictions <- sum(diag(conttab1))
total_predictions <- sum(conttab1)
accuracy <- correct_predictions / total_predictions
print(accuracy)

# Model 2:

ufc_mod_2 <- rpart(winner ~ r_wins + r_losses + b_wins + b_losses + r_SS_land_pm + r_SS_att_pm + b_SS_land_pm + b_SS_att_pm + weight_class + is_title_bout, train_data)
predictions2 <- predict(ufc_mod_2, test_data, type = "class")
conttab2 <- table(actual = test_data$winner, predicted = predictions2)
print(conttab2)

correct_predictions <- sum(diag(conttab2))
total_predictions <- sum(conttab2)
accuracy <- correct_predictions / total_predictions
print(accuracy)

# Model 3:

ufc_mod_3 <- rpart(winner ~ weight_class+is_title_bout+gender+total_rounds+r_weight+b_weight+r_height+b_height+r_reach+b_reach+r_stance+b_stance+r_wins+r_losses+r_SS_land_pm+r_SS_att_pm+r_SS_acc+r_TD_acc+r_SS_def_pct+r_TD_def_pct+b_wins+b_losses+b_SS_land_pm+b_SS_att_pm+b_SS_acc+b_TD_acc+b_SS_def_pct+b_TD_def_pct+r_age+b_age, train_data)
predictions3 <- predict(ufc_mod_3, test_data, type = "class")
conttab3 <- table(actual = test_data$winner, predicted = predictions3)
print(conttab3)

correct_predictions <- sum(diag(conttab3))
total_predictions <- sum(conttab3)
accuracy <- correct_predictions / total_predictions
print(accuracy)