# Load required packages
library(ggplot2)
library(gridExtra)  # For arranging multiple plots
library(dplyr)
library(glmnet)
library(mgcv)  # For GAM
library(readxl)  # For reading Excel files

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

# Get predicted probabilities from Lasso (important!)
Mental_disorder_symptoms$logistic_pred <- predict(lasso_model, newx = X, type = "response")

# Fit Ridge (L2 regularization) using glmnet
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

# Plot Lasso Logistic Model
plot_lasso <- ggplot(Mental_disorder_symptoms, aes(x = age)) +
  geom_line(aes(y = logistic_pred, color = "Lasso Logistic Model"), size = 1) +
  geom_point(aes(y = observed_prob, color = "Observed Probability"), size = 2) +
  labs(title = "Lasso Logistic Model vs Observed Probabilities",
       x = "Age", y = "Probability") +
  scale_color_manual(values = c("red", "purple")) +
  theme_minimal()

# Plot Ridge Logistic Model
plot_ridge <- ggplot(Mental_disorder_symptoms, aes(x = age)) +
  geom_line(aes(y = logistic_pred_ridge, color = "Ridge Logistic Model"), size = 1, linetype = "dashed") +
  geom_point(aes(y = observed_prob, color = "Observed Probability"), size = 2) +
  labs(title = "Ridge Logistic Model vs Observed Probabilities",
       x = "Age", y = "Probability") +
  scale_color_manual(values = c("blue", "purple")) +
  theme_minimal()

# Plot GAM Model
plot_gam <- ggplot(Mental_disorder_symptoms, aes(x = age)) +
  geom_line(aes(y = gam_pred, color = "GAM"), size = 1) +
  geom_point(aes(y = observed_prob, color = "Observed Probability"), size = 2) +
  labs(title = "GAM vs Observed Probabilities",
       x = "Age", y = "Probability") +
  scale_color_manual(values = c("green", "purple")) +
  theme_minimal()

# Combine the plots into one grid
combined_plots <- grid.arrange(plot_lasso, plot_ridge, plot_gam, nrow = 3)

# Save the combined plot
ggsave(filename = paste0(output_dir, "/combined_predicted_probabilities.png"), plot = combined_plots)

# Print the combined plot to the console
print(combined_plots)

# Success message for plots
print("All plots have been saved successfully in the Objective3 folder and printed in the console.")

#################################################



# Calculate AIC and deviance for Lasso and Ridge manually
calc_aic_deviance <- function(model, X, y) {
  # Get the predicted probabilities
  predicted_prob <- predict(model, newx = X, type = "response")
  
  # Calculate log-likelihood
  log_lik <- sum(y * log(predicted_prob) + (1 - y) * log(1 - predicted_prob))
  
  # Calculate number of non-zero coefficients
  k <- sum(coef(model) != 0)
  
  # Calculate AIC
  aic <- -2 * log_lik + 2 * k
  
  # Calculate deviance
  deviance <- -2 * log_lik
  
  return(list(AIC = aic, Deviance = deviance))
}

# AIC and deviance for Lasso
lasso_metrics <- calc_aic_deviance(lasso_model, X, y)

# AIC and deviance for Ridge
ridge_metrics <- calc_aic_deviance(ridge_model, X, y)

# AIC and deviance for GAM (GAM already provides AIC and deviance)
gam_aic <- AIC(gam_model)
gam_deviance <- deviance(gam_model)

# Model Comparison (Table)
comparison_table <- data.frame(
  Model = c("Lasso Logistic", "Ridge Logistic", "GAM"),
  AIC = c(lasso_metrics$AIC, ridge_metrics$AIC, gam_aic),
  Deviance = c(lasso_metrics$Deviance, ridge_metrics$Deviance, gam_deviance)
)

# Print the model comparison table
print(comparison_table)

# Save the comparison table to a CSV file
write.csv(comparison_table, file = paste0(output_dir, "/model_comparison_table.csv"))

# Plot AIC and Deviance Comparison
comparison_plot <- ggplot(comparison_table, aes(x = Model)) +
  geom_bar(aes(y = AIC, fill = "AIC"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Deviance, fill = "Deviance"), stat = "identity", position = "dodge", alpha = 0.6) +
  labs(title = "Model Comparison: AIC and Deviance",
       x = "Model", y = "Value") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal()

# Save the model comparison plot
ggsave(filename = paste0(output_dir, "/model_comparison_plot.png"), plot = comparison_plot)

# Print the model comparison plot to the console
print(comparison_plot)

# Success message for comparison plot
print("Model comparison plots and tables have been saved successfully in the Objective3 folder.")
