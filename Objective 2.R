# Load necessary libraries
library(readxl)
library(psych)
library(corrplot)
library(reshape2)
library(ggplot2)

# Ensure that the output directory exists
output_dir <- "Objective2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Load the data
Mental_disorder_symptoms <- read_excel("C:/Users/User/Desktop/Debashis 2024/SAGE OPEN JOURNAL SUBMISSION PAPERS/PAPER 1 (MENTAL DISORDER DATASET FROM kRAGGLE)/DATASET/Mental disorder symptoms.xlsx")

# Remove non-binary variables (for EFA and Phi coefficient)
binary_symptoms <- Mental_disorder_symptoms[, sapply(Mental_disorder_symptoms, function(x) length(unique(x)) == 2)]

# -----------------------
# Exploratory Factor Analysis (EFA)
# -----------------------

# Perform EFA with an initial number of factors (adjust as necessary)
efa_model <- fa(binary_symptoms, nfactors = 5, fm = "ml", rotate = "varimax")

# Print the EFA results
print(efa_model)

# Convert the loadings to a data frame for easier handling
efa_loadings <- as.data.frame.matrix(efa_model$loadings[])
efa_loadings$Symptoms <- rownames(efa_loadings)  # Keep symptoms as rownames

# Save the EFA loadings as a CSV file
write.csv(efa_loadings, file = paste0(output_dir, "/efa_loadings.csv"))

# Plot the Scree plot for EFA
scree_plot <- fa.parallel(binary_symptoms, fm = "ml", fa = "fa")
ggsave(filename = paste0(output_dir, "/efa_scree_plot.png"))

# Reshape the loadings for plotting
efa_loadings_melted <- melt(efa_loadings, id.vars = "Symptoms", variable.name = "Factor", value.name = "Loading")

# Plot factor loadings for visualization
loadings_plot <- ggplot(efa_loadings_melted, aes(x = Factor, y = Symptoms, fill = Loading)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(title = "Factor Loadings", x = "Factor", y = "Symptoms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot
ggsave(filename = paste0(output_dir, "/efa_loadings_plot.png"), plot = loadings_plot)

# -----------------------
# Correlation Analysis Using Phi Coefficient
# -----------------------

# Compute the Phi coefficient matrix (for binary data)
phi_corr_matrix <- phi(binary_symptoms)

# Save the correlation matrix as a CSV file
write.csv(as.data.frame(phi_corr_matrix), file = paste0(output_dir, "/phi_correlation_matrix.csv"))

# Plot the correlation matrix using corrplot
png(filename = paste0(output_dir, "/phi_correlation_plot.png"))
corrplot(phi_corr_matrix, method = "color", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)
dev.off()

# Print success message for completion
print("Objective 2 analysis completed successfully. All plots and tables have been saved.")

#################################

# Load required libraries
library(readxl)
library(psych)
library(corrplot)
library(ggplot2)
library(reshape2)

# Ensure the output directory exists
output_dir <- "Objective2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Load the data
Mental_disorder_symptoms <- read_excel("C:/Users/User/Desktop/Debashis 2024/SAGE OPEN JOURNAL SUBMISSION PAPERS/PAPER 1 (MENTAL DISORDER DATASET FROM kRAGGLE)/DATASET/Mental disorder symptoms.xlsx")

# Remove non-binary variables for EFA and Phi coefficient analysis
binary_symptoms <- Mental_disorder_symptoms[, sapply(Mental_disorder_symptoms, function(x) length(unique(x)) == 2)]

# -----------------------
# Exploratory Factor Analysis (EFA)
# -----------------------

# Perform EFA with an initial number of factors (adjust number of factors if needed)
efa_model <- fa(binary_symptoms, nfactors = 5, fm = "ml", rotate = "varimax")
print(efa_model)  # Print the EFA results to the console

# Convert the loadings to a data frame for easier handling
efa_loadings <- as.data.frame(efa_model$loadings[,])
efa_loadings$Symptoms <- rownames(efa_loadings)

# Save the EFA loadings as a CSV file and print to console
write.csv(efa_loadings, file = paste0(output_dir, "/efa_loadings.csv"))
print(efa_loadings)  # Print the loadings table to the console

# -----------------------
# Scree Plot for EFA
# -----------------------

# Perform parallel analysis for the scree plot
scree_plot <- fa.parallel(binary_symptoms, fm = "ml", fa = "fa")

# Save the scree plot and display it in the console
png(filename = paste0(output_dir, "/efa_scree_plot.png"))
fa.parallel(binary_symptoms, fm = "ml", fa = "fa")  # Scree plot output
dev.off()

# -----------------------
# Factor Loadings Heatmap
# -----------------------

# Reshape the loadings for plotting
efa_loadings_melted <- melt(efa_loadings, id.vars = "Symptoms", variable.name = "Factors", value.name = "Loadings")

# Plot factor loadings as a heatmap
loadings_plot <- ggplot(efa_loadings_melted, aes(x = Factors, y = Symptoms, fill = Loadings)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(title = "Factor Loadings", x = "Factors", y = "Symptoms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the heatmap plot and print it in the console
ggsave(filename = paste0(output_dir, "/efa_loadings_plot.png"), plot = loadings_plot)
print(loadings_plot)  # Display the factor loadings plot in the console

# -----------------------
# Phi Coefficient Correlation Analysis
# -----------------------

# Initialize a matrix to store Phi coefficients
phi_corr_matrix <- matrix(NA, ncol = ncol(binary_symptoms), nrow = ncol(binary_symptoms))
colnames(phi_corr_matrix) <- colnames(binary_symptoms)
rownames(phi_corr_matrix) <- colnames(binary_symptoms)

# Calculate phi coefficients for each pair of binary variables
for (i in 1:ncol(binary_symptoms)) {
  for (j in 1:ncol(binary_symptoms)) {
    if (i != j) {
      # Create a 2x2 contingency table for the two variables
      contingency_table <- table(as.vector(binary_symptoms[, i]), as.vector(binary_symptoms[, j]))
      
      # Compute cell counts needed for Phi coefficient formula
      if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
        n11 <- contingency_table[2, 2]
        n00 <- contingency_table[1, 1]
        n10 <- contingency_table[2, 1]
        n01 <- contingency_table[1, 2]
        
        # Calculate the Phi coefficient
        phi_corr_matrix[i, j] <- (n11 * n00 - n10 * n01) / sqrt((n11 + n10) * (n01 + n00) * (n11 + n01) * (n00 + n10))
      } else {
        phi_corr_matrix[i, j] <- 0  # If the contingency table is not 2x2, set Phi to 0
      }
    } else {
      # Set diagonal values to 1 (perfect correlation with itself)
      phi_corr_matrix[i, j] <- 1
    }
  }
}

# Save the Phi correlation matrix as a CSV file and print to console
write.csv(as.data.frame(phi_corr_matrix), file = paste0(output_dir, "/phi_correlation_matrix.csv"))
print(phi_corr_matrix)  # Print the Phi coefficient matrix to the console

# -----------------------
# Phi Coefficient Correlation Plot
# -----------------------

# Plot the Phi correlation matrix as a heatmap
png(filename = paste0(output_dir, "/phi_correlation_plot.png"))

# Round the values to 2 decimal places for better readability
rounded_phi_corr_matrix <- round(phi_corr_matrix, 2)

# Use `corrplot` with appropriate number formatting and size adjustments
corrplot(as.matrix(rounded_phi_corr_matrix), method = "color", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.8, # Adjust text size for visibility
         is.corr = FALSE, # Disable correlation assumption for the plot
         cl.pos = "r") # Legend on the right

dev.off()

# Print success message
print("All plots and tables saved successfully for Objective 2.")

###############################################

# Ensure that necessary libraries are installed
if (!require(psych)) install.packages("psych")
if (!require(corrplot)) install.packages("corrplot")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")

# Load the psych package for tetrachoric correlation
library(psych)
library(corrplot)
library(ggplot2)
library(reshape2)

# Load the data
Mental_disorder_symptoms <- read_excel("C:/Users/User/Desktop/Debashis 2024/SAGE OPEN JOURNAL SUBMISSION PAPERS/PAPER 1 (MENTAL DISORDER DATASET FROM kRAGGLE)/DATASET/Mental disorder symptoms.xlsx")

# Remove non-binary variables for correlation analysis
binary_symptoms <- Mental_disorder_symptoms[, sapply(Mental_disorder_symptoms, function(x) length(unique(x)) == 2)]

# Perform Tetrachoric Correlation
tetra_corr <- tetrachoric(binary_symptoms)

# Extract the correlation matrix
tetra_corr_matrix <- tetra_corr$rho

# Save the Tetrachoric Correlation matrix as a CSV file and print to console
output_dir <- "Objective2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
write.csv(as.data.frame(tetra_corr_matrix), file = paste0(output_dir, "/tetra_correlation_matrix.csv"))
print(tetra_corr_matrix)  # Print the matrix to console

# Plot the Tetrachoric Correlation matrix as a heatmap
png(filename = paste0(output_dir, "/tetra_correlation_plot.png"))
corrplot(as.matrix(tetra_corr_matrix), method = "color", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.8, # Adjust text size for visibility
         is.corr = TRUE, # Enable correlation assumption for the plot
         cl.pos = "r") # Legend on the right
dev.off()

# Print success message
print("Tetrachoric correlation analysis completed successfully. All plots and tables saved.")


###########################
# Adjust plot dimensions for a larger heatmap
# Adjust plot dimensions for a larger heatmap with smaller text
png(filename = paste0(output_dir, "/tetra_correlation_plot.png"), width = 1920, height = 1080, res = 300)

# Plot the Tetrachoric Correlation matrix as a heatmap
corrplot(as.matrix(tetra_corr_matrix), method = "color", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.5, # Reduce text size inside the heatmap
         tl.cex = 0.5, # Reduce axis label size
         is.corr = TRUE, # Enable correlation assumption for the plot
         cl.pos = "r", cl.cex = 0.7)  # Adjust legend size

dev.off()

print("Heatmap of the Tetrachoric Correlation matrix saved with smaller text.")

