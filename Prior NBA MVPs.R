
Title: "Prior NBA MVPs"
Author: "Gavin Baskette"
Date: Sys.Date()


# ---- R Libraries ----

library(dplyr)
library(car)
library(lmtest)


# ---- DATA LOADING AND INITIAL PROCESSING ----

Prior_NBA_MVPs <- read.csv("C:/Users/gavin/OneDrive/Desktop/Sport Models/Prior NBA MVPs.csv")
head(Prior_NBA_MVPs)
tail(Prior_NBA_MVPs)


# ---- DATA CLEANING: REMOVE NON-NUMERIC COLUMNS & HANDLE MISSING DATA ----

non_numeric_column <- c("Player")

numeric_data <- Prior_NBA_MVPs[, !(names(Prior_NBA_MVPs) %in% 
                                          non_numeric_column)]

numeric_data <- numeric_data %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

clean_numeric_data <- numeric_data[complete.cases(numeric_data), ]

clean_numeric_data <- clean_numeric_data[, -which(names(clean_numeric_data) == "Season")]


# ---- INITIAL LINEAR MODEL TO CHECK MULTICOLLINEARITY ----

clean_numeric_data$dummy_y <- rnorm(nrow(clean_numeric_data))
lm_model <- lm(dummy_y ~ ., data = clean_numeric_data)

print(names(clean_numeric_data))

predictors <- clean_numeric_data[, !names(clean_numeric_data) %in% "dummy_y"]
target <- clean_numeric_data$dummy_y


# ---- IDENTIFYING AND REMOVING HIGHLY CORRELATED VARIABLES ----

cor_matrix <- cor(predictors)
high_corr <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr)

variables_to_drop <- c("True.Shooting..", "BPM.Rank", "VORP.Rank", 
                       "eFG.", "PER.Rank")  # Use high_corr results
predictors_reduced <- predictors[, !(names(predictors) %in% variables_to_drop)]


# ---- UPDATED MODEL AFTER DROPPING HIGHLY CORRELATED VARIABLES ----

final_data <- cbind(predictors_reduced, dummy_y = target)

lm_model_final <- lm(dummy_y ~ ., data = final_data)


# ---- CHECKING FOR ALIASED VARIABLES ----

alias_results_final <- alias(lm_model_final)
print(alias_results_final)

alias(lm_model_final)$Complete

lm_model_final

final_data_filtered <- final_data

na_vars <- names(coef(lm_model_final))[is.na(coef(lm_model_final))]

final_data_filtered <- final_data_filtered[, !names(final_data_filtered) %in% na_vars]

lm_model_final <- lm(dummy_y ~ ., data = final_data_filtered)

summary(lm_model_final)

dim(final_data_filtered)


lm_model_final <- lm(dummy_y ~ ., data = final_data_filtered)
summary(lm_model_final)


# ---- FINAL MODEL SELECTION & VIF ANALYSIS ----

vif_values_final <- vif(lm_model_final)
print(vif_values_final)


# ---- ADJUSTING WEIGHTS BASED ON VIF ----

# Original weights (z-scores from dataset)
weights <- c(5.244182623, 1.150556186, 0.37366451, 0.453131562, 
             0.391333563, 1.161900982, 0.237005853, 1.948084723, 
             2.887249273, 1.299014278, 20.09084626, 3.388816107, 
             1.038234147, 1.486023777, 1.472873517, 3.083052883, 
             4.295486003, 1.112500642)

# Actual VIF values from earlier result
vif_values <- c(61.142843, 105.960811, 9.594955, 3.984952, 
                184.280529, 62.245989, 13.361316, 40.770809, 
                36.129117, 53.827944, 43.955936, 35.272856, 
                89.097657, 148.247559, 56.058227, 24.568070,
                12.430933, 3.687647)

# Calculated adjusted weights based on VIF
adjusted_weights <- weights / sqrt(log(vif_values))

# Normalized adjusted weights to maintain the same total as original weights
normalized_weights <- adjusted_weights / sum(adjusted_weights) * sum(weights)

# Summary table to compare
results <- data.frame(
  Variable = c("PPG.Rank", "Rebounds...Assists.Per.Game", "Free.Throw..", "Games.Played",
               "Rebound..", "Assist..", "Steal..", "Usage..", "Offensive.Win.Shares",
               "Defensive.Win.Shares", "WS.Rank", "Offensive.Box.Plus.Minus", 
               "Defensive.Box.Plus.Minus", "Offensive.Rating", "Defensive.Rating", 
               "Wins", "Seeding", "Win...Difference.When.Playing.vs..Sitting"),
  Original_Weight = weights,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weights
)

print(results)


# ---- FINAL SELECTION OF REMAINING VARIABLES & RUNNING VIF ANALYSIS ----

head(Prior_NBA_MVPs)
head(predictors)

remaining_vars <- c("eFG.", "PER.Rank", "True.Shooting..", 
                    "BPM.Rank", "VORP.Rank", 
                    "Difference.Between.ORTG...DRTG")

predictors_with_target <- cbind(predictors, dummy_y = target)

final_data_remaining <- predictors_with_target[, c(remaining_vars, "dummy_y")]

lm_remaining <- lm(dummy_y ~ ., data = final_data_remaining)

vif_values_remaining <- vif(lm_remaining)

print(vif_values_remaining)


# ---- ADJUSTING WEIGHTS BASED ON VIF FOR REMAINING VARIABLES ----

# Original weights (z-scores from dataset)
weights <- c(0.475332161, 16.58587231, 1.09217586, 19.83761792, 
             15.56070589, 3.499650993)

# Actual VIF values from earlier result
vif_values <- c(10.344550, 9.438641, 7.595098, 9.352036, 
                9.637570, 3.816568)


# Calculated adjusted weights based on VIF
adjusted_weights <- weights / sqrt(vif_values)

# Normalized adjusted weights to maintain the same total as original weights
normalized_weights <- adjusted_weights / sum(adjusted_weights) * sum(weights)

# Summary table to compare
results <- data.frame(
  Variable = c("eFG.", "PER.Rank", "True.Shooting..", 
               "BPM.Rank", "VORP.Rank", 
               "Difference.Between.ORTG...DRTG"),
  Original_Weight = weights,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weights
)

print(results)
