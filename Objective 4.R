# Load required packages
library(dplyr)
library(poLCA)  # For LCA
library(ggplot2)
library(tidyr)  # For reshaping data if needed
library(readxl)

# Step 1: Load the dataset
Mental_disorder_symptoms <- read_excel("C:/Users/User/Desktop/Debashis 2024/SAGE OPEN JOURNAL SUBMISSION PAPERS/PAPER 1 (MENTAL DISORDER DATASET FROM kRAGGLE)/DATASET/Mental disorder symptoms.xlsx")

# Step 2: Check column names to ensure correct referencing
print(colnames(Mental_disorder_symptoms))

# Step 3: Remove 'Disorder' and 'age' columns, ensuring that all other columns are numeric
# Use the correct select syntax for dplyr and ensure that all columns except 'Disorder' and 'age' are numeric

binary_symptoms <- Mental_disorder_symptoms %>%
  dplyr::select(-Disorder, -age) %>%  # Remove 'Disorder' and 'age'
  mutate(across(everything(), as.numeric))  # Ensure remaining columns are numeric

# Step 4: Check the resulting dataset to ensure no issues
print(head(binary_symptoms))  # Preview to ensure the data is in correct format

# Step 5: Run Latent Class Analysis (LCA)
# Prepare the LCA model (poLCA requires data in a specific format, with all variables as factors)
binary_symptoms_factor <- binary_symptoms %>%
  mutate(across(everything(), as.factor))

# Set up models with different numbers of latent classes (K)
set.seed(123)  # For reproducibility

# Fit models with varying number of classes (K = 2, 3, 4)
lca_model_2 <- poLCA(cbind(feeling.nervous, panic, breathing.rapidly, sweating, 
                           trouble.in.concentration, having.trouble.in.sleeping, 
                           having.trouble.with.work, hopelessness, anger, 
                           over.react, change.in.eating, suicidal.thought, 
                           feeling.tired, close.friend, social.media.addiction, 
                           weight.gain, introvert, popping.up.stressful.memory, 
                           having.nightmares, avoids.people.or.activities, 
                           feeling.negative, trouble.concentrating, blamming.yourself, 
                           hallucinations, repetitive.behaviour, seasonally, 
                           increased.energy) ~ 1, 
                     data = binary_symptoms_factor, nclass = 2)

lca_model_3 <- poLCA(cbind(feeling.nervous, panic, breathing.rapidly, sweating, 
                           trouble.in.concentration, having.trouble.in.sleeping, 
                           having.trouble.with.work, hopelessness, anger, 
                           over.react, change.in.eating, suicidal.thought, 
                           feeling.tired, close.friend, social.media.addiction, 
                           weight.gain, introvert, popping.up.stressful.memory, 
                           having.nightmares, avoids.people.or.activities, 
                           feeling.negative, trouble.concentrating, blamming.yourself, 
                           hallucinations, repetitive.behaviour, seasonally, 
                           increased.energy) ~ 1, 
                     data = binary_symptoms_factor, nclass = 3)

lca_model_4 <- poLCA(cbind(feeling.nervous, panic, breathing.rapidly, sweating, 
                           trouble.in.concentration, having.trouble.in.sleeping, 
                           having.trouble.with.work, hopelessness, anger, 
                           over.react, change.in.eating, suicidal.thought, 
                           feeling.tired, close.friend, social.media.addiction, 
                           weight.gain, introvert, popping.up.stressful.memory, 
                           having.nightmares, avoids.people.or.activities, 
                           feeling.negative, trouble.concentrating, blamming.yourself, 
                           hallucinations, repetitive.behaviour, seasonally, 
                           increased.energy) ~ 1, 
                     data = binary_symptoms_factor, nclass = 4)

# Step 6: Compare models based on BIC and AIC
model_comparison <- data.frame(
  Model = c("2 Classes", "3 Classes", "4 Classes"),
  AIC = c(lca_model_2$aic, lca_model_3$aic, lca_model_4$aic),
  BIC = c(lca_model_2$bic, lca_model_3$bic, lca_model_4$bic)
)

print(model_comparison)

# Save model comparison table to CSV
output_dir <- "Objective4"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
write.csv(model_comparison, file = paste0(output_dir, "/lca_model_comparison.csv"))

# Step 7: Plot model comparison for AIC and BIC
comparison_plot <- ggplot(model_comparison, aes(x = Model)) +
  geom_bar(aes(y = AIC, fill = "AIC"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = BIC, fill = "BIC"), stat = "identity", position = "dodge", alpha = 0.6) +
  labs(title = "Model Comparison: AIC and BIC for Latent Classes",
       x = "Number of Classes", y = "Information Criterion Value") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

# Save the plot and print
ggsave(filename = paste0(output_dir, "/lca_model_comparison_plot.png"), plot = comparison_plot)
print(comparison_plot)

# Step 8: Visualize posterior probabilities of class membership
# Plot the posterior probabilities for each individual (based on the best model, e.g., 3-class model)

posterior_probs <- as.data.frame(lca_model_3$posterior)
colnames(posterior_probs) <- paste("Class", 1:3, sep = "")

# Add subject index for visualization
posterior_probs$Subject <- 1:nrow(posterior_probs)

# Reshape for ggplot2
posterior_probs_long <- posterior_probs %>%
  pivot_longer(cols = starts_with("Class"), names_to = "Class", values_to = "Probability")

# Plot the posterior probabilities for each individual
posterior_plot <- ggplot(posterior_probs_long, aes(x = Subject, y = Probability, color = Class)) +
  geom_line() +
  labs(title = "Posterior Probabilities for Latent Classes (3 Classes)",
       x = "Subject", y = "Probability") +
  theme_minimal()

# Save the posterior probability plot
ggsave(filename = paste0(output_dir, "/posterior_probabilities_3class.png"), plot = posterior_plot)
print(posterior_plot)

# Step 9: Generate class membership table
class_membership <- data.frame(
  Subject = 1:nrow(posterior_probs),
  Class = apply(posterior_probs[, 1:3], 1, which.max)  # Assign individuals to the most probable class
)

print(head(class_membership))

# Save class membership table to CSV
write.csv(class_membership, file = paste0(output_dir, "/class_membership_3class.csv"))

# Step 10: Success message
print("All plots and tables have been saved successfully in the Objective4 folder.")
