# Load required libraries
library(readxl)
library(dplyr)
library(glmnet)
library(mgcv)
library(ggplot2)

# Load the dataset
Mental_disorder_symptoms <- read_excel("C:/Users/User/Desktop/Debashis 2024/SAGE OPEN JOURNAL SUBMISSION PAPERS/PAPER 1 (MENTAL DISORDER DATASET FROM kRAGGLE)/DATASET/Mental disorder symptoms.xlsx")

# Convert 'Disorder' into a binary variable for logistic regression (e.g., ADHD = 1, others = 0)
Mental_disorder_symptoms$disorder_binary <- ifelse(Mental_disorder_symptoms$Disorder == "ADHD", 1, 0)

# Create a folder to save outputs
output_dir <- "Objective3"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Ensure all symptoms are numeric and remove uninformative columns
binary_symptoms <- Mental_disorder_symptoms %>%
  select(-Disorder) %>%
  mutate(across(everything(), as.numeric))

# Check for zero variance columns and remove them
zero_var_cols <- sapply(binary_symptoms, function(x) length(unique(x)) == 1)
binary_symptoms <- binary_symptoms[, !zero_var_cols]

# Prepare data for regularized logistic regression (Lasso/Ridge)
X <- as.matrix(binary_symptoms %>% select(-disorder_binary))
y <- binary_symptoms$disorder_binary

# Fit Lasso (L1 regularization) using glmnet
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1)  # alpha=1 for Lasso
lasso_model <- glmnet(X, y, family = "binomial", alpha = 1, lambda = cv_lasso$lambda.min)

# Get predicted probabilities from Lasso
Mental_disorder_symptoms$logistic_pred <- predict(lasso_model, newx = X, type = "response")

# Fit Ridge (L2 regularization) instead of Lasso
cv_ridge <- cv.glmnet(X, y, family = "binomial", alpha = 0)  # alpha=0 for Ridge
ridge_model <- glmnet(X, y, family = "binomial", alpha = 0, lambda = cv_ridge$lambda.min)

# Get predicted probabilities from Ridge model
Mental_disorder_symptoms$logistic_pred_ridge <- predict(ridge_model, newx = X, type = "response")

# Generalized Additive Model (GAM) with smooth function for age
gam_formula <- as.formula(paste("disorder_binary ~ s(age) +", 
                                paste(colnames(binary_symptoms)[-ncol(binary_symptoms)], collapse = " + ")))

# Fit the GAM model
gam_model <- gam(gam_formula, data = Mental_disorder_symptoms, family = binomial)

# Get predicted probabilities from the GAM model
Mental_disorder_symptoms$gam_pred <- predict(gam_model, type = "response")

# Calculate observed probabilities by age group
observed_probs <- Mental_disorder_symptoms %>%
  group_by(age) %>%
  summarize(observed_prob = mean(disorder_binary))

# Merge observed probabilities into the main dataset
Mental_disorder_symptoms <- left_join(Mental_disorder_symptoms, observed_probs, by = "age")

# Plot predicted probabilities against age for Lasso, Ridge, GAM, and observed probabilities
predicted_plot <- ggplot(Mental_disorder_symptoms, aes(x = age)) +
  geom_line(aes(y = logistic_pred, color = "Lasso Logistic Model"), size = 1) +
  geom_line(aes(y = logistic_pred_ridge, color = "Ridge Logistic Model"), size = 1, linetype = "dashed") +
  geom_line(aes(y = gam_pred, color = "GAM"), size = 1) +
  geom_point(aes(y = observed_prob, color = "Observed Probability"), size = 2) +
  labs(title = "Predicted and Observed Probabilities by Age",
       x = "Age", y = "Probability") +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  theme_minimal()

# Save the predicted probabilities plot
ggsave(filename = paste0(output_dir, "/predicted_probabilities_with_observed.png"), plot = predicted_plot)

# Print the predicted probabilities plot to the console
print(predicted_plot)

# Save the summaries of the models to text files
sink(paste0(output_dir, "/lasso_model_summary.txt"))
print(summary(lasso_model))
sink()

sink(paste0(output_dir, "/ridge_model_summary.txt"))
print(summary(ridge_model))
sink()

sink(paste0(output_dir, "/gam_model_summary.txt"))
print(summary(gam_model))
sink()

# Save the dataset with predictions and observed probabilities
write.csv(Mental_disorder_symptoms, file = paste0(output_dir, "/Mental_disorder_predictions.csv"))

# Success message
print("All plots and tables have been saved successfully in the Objective3 folder and printed in the console.")


#########################


# Plot predicted probabilities against age for Lasso, Ridge, GAM, and observed probabilities
predicted_plot <- ggplot(Mental_disorder_symptoms, aes(x = age)) +
  geom_line(aes(y = logistic_pred, color = "Lasso Logistic Model"), size = 1.2, linetype = "solid", alpha = 0.8) +  # Thicker and less transparent
  geom_line(aes(y = logistic_pred_ridge, color = "Ridge Logistic Model"), size = 1.2, linetype = "dashed", alpha = 0.8) +  # Dashed for Ridge
  geom_line(aes(y = gam_pred, color = "GAM"), size = 1.2, linetype = "dotdash", alpha = 0.8) +  # Dot-dash for GAM
  geom_point(aes(y = observed_prob, color = "Observed Probability"), size = 2) +  # Points for observed probability
  labs(title = "Predicted and Observed Probabilities by Age",
       x = "Age", y = "Probability") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +  # Red for Lasso, Blue for Ridge, Green for GAM, Purple for Observed
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to the bottom for better visibility

# Save the updated predicted probabilities plot
ggsave(filename = paste0(output_dir, "/predicted_probabilities_with_observed_updated.png"), plot = predicted_plot)

# Print the updated predicted probabilities plot to the console
print(predicted_plot)

# Success message
print("All updated plots and tables have been saved successfully in the Objective3 folder and printed in the console.")

