library(readxl)
 Mental_disorder_symptoms <- read_excel("C:/Users/User/Desktop/Debashis 2024/SAGE OPEN JOURNAL SUBMISSION PAPERS/PAPER 1 (MENTAL DISORDER DATASET FROM kRAGGLE)/DATASET/Mental disorder symptoms.xlsx")
 Mental_disorder_Rawdata=Mental_disorder_symptoms
######################
 
 # Load necessary libraries
 library(readxl)
 library(nnet)        # For multinomial logistic regression
 library(glmnet)      # For regularized regression (LASSO)
 library(randomForest) # For Random Forest
 library(ggplot2)     # For plotting
 library(caret)       # For model training and evaluation
 
 # Load the dataset
 Mental_disorder_symptoms <- read_excel("C:/Users/User/Desktop/Debashis 2024/SAGE OPEN JOURNAL SUBMISSION PAPERS/PAPER 1 (MENTAL DISORDER DATASET FROM kRAGGLE)/DATASET/Mental disorder symptoms.xlsx")
 
 # Ensure that the output directory exists
 output_dir <- "objective1"
 if (!dir.exists(output_dir)) {
   dir.create(output_dir)
 }
 
 # Convert the outcome variable to a factor
 Mental_disorder_symptoms$Disorder <- as.factor(Mental_disorder_symptoms$Disorder)
 
 # Split the data into training and testing sets
 set.seed(123)
 trainIndex <- createDataPartition(Mental_disorder_symptoms$Disorder, p = .7, 
                                   list = FALSE, 
                                   times = 1)
 Mental_train <- Mental_disorder_symptoms[trainIndex,]
 Mental_test  <- Mental_disorder_symptoms[-trainIndex,]
 
 # Prepare the data for LASSO
 x_train <- model.matrix(Disorder ~ . -1, data = Mental_train)
 y_train <- Mental_train$Disorder
 
 x_test <- model.matrix(Disorder ~ . -1, data = Mental_test)
 y_test <- Mental_test$Disorder
 
 # Apply LASSO regularization to reduce the number of predictors
 set.seed(123)
 cvfit <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1)
 
 # Get the coefficients at the optimal lambda
 lasso_coef <- coef(cvfit, s = "lambda.min")
 
 # Extract the names of the non-zero coefficients (selected features)
 selected_features <- unique(unlist(lapply(lasso_coef, function(x) {
   non_zero <- which(x != 0)
   if(length(non_zero) > 0) {
     rownames(x)[non_zero]
   } else {
     NULL
   }
 })))
 
 # Ensure the selected features are available in x_train
 selected_features <- selected_features[selected_features %in% colnames(x_train)]
 
 # Check if any features were selected by LASSO
 if(length(selected_features) > 0) {
   x_train_selected <- x_train[, selected_features, drop=FALSE]
   x_test_selected <- x_test[, selected_features, drop=FALSE]
   
   # Fit the multinomial logistic regression model using the selected features
   mlogit_model <- multinom(y_train ~ ., data = as.data.frame(x_train_selected))
   
   # Predict and evaluate on the test set
   mlogit_pred <- predict(mlogit_model, newdata = as.data.frame(x_test_selected))
   confusion_mlogit <- confusionMatrix(mlogit_pred, y_test)
   print(confusion_mlogit)
   
   # Save confusion matrix as a table
   write.csv(as.data.frame(confusion_mlogit$table), file = paste0(output_dir, "/mlogit_confusion_matrix.csv"))
   
   # Plot the LASSO cross-validation curve and save it
   plot(cvfit)
   ggsave(filename = paste0(output_dir, "/lasso_cv_plot.png"))
 } else {
   print("No features were selected by LASSO.")
 }
 
 ###########################################
 
 # Load necessary libraries
 library(readxl)
 library(caret)
 library(nnet)
 library(glmnet)
 library(randomForest)
 library(ggplot2)
 
 # Ensure that the output directory exists
 output_dir <- "objective1"
 if (!dir.exists(output_dir)) {
   dir.create(output_dir)
 }
 
 # Load the data
 Mental_disorder_symptoms <- read_excel("C:/Users/User/Desktop/Debashis 2024/SAGE OPEN JOURNAL SUBMISSION PAPERS/PAPER 1 (MENTAL DISORDER DATASET FROM kRAGGLE)/DATASET/Mental disorder symptoms.xlsx")
 
 # Convert the outcome variable to a factor
 Mental_disorder_symptoms$Disorder <- as.factor(Mental_disorder_symptoms$Disorder)
 
 # Split the data into training and testing sets
 set.seed(123)
 trainIndex <- createDataPartition(Mental_disorder_symptoms$Disorder, p = .7, list = FALSE, times = 1)
 Mental_train <- Mental_disorder_symptoms[trainIndex, ]
 Mental_test <- Mental_disorder_symptoms[-trainIndex, ]
 
 # Multinomial Logistic Regression
 mlogit_model <- multinom(Disorder ~ ., data = Mental_train)
 
 # Predict on the test set
 mlogit_pred <- predict(mlogit_model, newdata = Mental_test)
 
 # Confusion matrix
 confusion_mlogit <- confusionMatrix(mlogit_pred, Mental_test$Disorder)
 print(confusion_mlogit)
 
 # Save confusion matrix as a table
 write.csv(as.data.frame(confusion_mlogit$table), file = paste0(output_dir, "/mlogit_confusion_matrix.csv"))
 
 # Prepare the data for LASSO
 x_train <- model.matrix(Disorder ~ . -1, data = Mental_train)
 y_train <- Mental_train$Disorder
 x_test <- model.matrix(Disorder ~ . -1, data = Mental_test)
 y_test <- Mental_test$Disorder
 
 # Apply LASSO regularization to reduce the number of predictors
 set.seed(123)
 cvfit <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1)
 
 # Save LASSO cross-validation plot
 png(filename = paste0(output_dir, "/lasso_cv_plot.png"))
 plot(cvfit)
 dev.off()
 
 # Get the coefficients at the optimal lambda
 lasso_coef <- coef(cvfit, s = "lambda.min")
 
 # Extract the names of the non-zero coefficients (selected features)
 selected_features <- unique(unlist(lapply(lasso_coef, function(x) rownames(x)[x[, 1] != 0])))
 
 # Check if any features were selected and if they exist in the training data
 selected_features <- selected_features[selected_features %in% colnames(x_train)]
 
 if (length(selected_features) > 0) {
   x_train_selected <- x_train[, selected_features, drop = FALSE]
   x_test_selected <- x_test[, selected_features, drop = FALSE]
   
   # Refit multinomial logistic regression with selected features
   selected_formula <- as.formula(paste("Disorder ~", paste(selected_features, collapse = " + ")))
   mlogit_model_lasso <- multinom(selected_formula, data = Mental_train)
   
   # Predict on the test set using the LASSO-selected features
   mlogit_pred_lasso <- predict(mlogit_model_lasso, newdata = Mental_test)
   
   # Confusion matrix for LASSO-regularized model
   confusion_mlogit_lasso <- confusionMatrix(mlogit_pred_lasso, Mental_test$Disorder)
   print(confusion_mlogit_lasso)
   
   # Save confusion matrix for LASSO model as a table
   write.csv(as.data.frame(confusion_mlogit_lasso$table), file = paste0(output_dir, "/mlogit_lasso_confusion_matrix.csv"))
 } else {
   print("No features were selected by LASSO.")
 }
 
 # Random Forest Classifier
 rf_model <- randomForest(Disorder ~ ., data = Mental_train)
 
 # Predict on the test set
 rf_pred <- predict(rf_model, newdata = Mental_test)
 
 # Confusion matrix for Random Forest
 confusion_rf <- confusionMatrix(rf_pred, Mental_test$Disorder)
 print(confusion_rf)
 
 # Save confusion matrix as a table
 write.csv(as.data.frame(confusion_rf$table), file = paste0(output_dir, "/rf_confusion_matrix.csv"))
 
 # Variable importance plot for Random Forest
 importance_df <- as.data.frame(importance(rf_model))
 importance_df$Feature <- rownames(importance_df)
 
 ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
   geom_bar(stat = "identity") +
   coord_flip() +
   labs(title = "Variable Importance - Random Forest", x = "Features", y = "Mean Decrease in Gini") +
   theme_minimal()
 
 # Save variable importance plot
 ggsave(filename = paste0(output_dir, "/rf_importance.png"))
 
 
 
 ####################
 
 
 # Load necessary libraries
 library(readxl)
 library(caret)
 library(nnet)
 library(glmnet)
 library(randomForest)
 library(ggplot2)
 
 # Ensure that the output directory exists
 output_dir <- "objective1"
 if (!dir.exists(output_dir)) {
   dir.create(output_dir)
 }
 
 # Load the data
 Mental_disorder_symptoms <- read_excel("C:/Users/User/Desktop/Debashis 2024/SAGE OPEN JOURNAL SUBMISSION PAPERS/PAPER 1 (MENTAL DISORDER DATASET FROM kRAGGLE)/DATASET/Mental disorder symptoms.xlsx")
 
 # Convert the outcome variable to a factor
 Mental_disorder_symptoms$Disorder <- as.factor(Mental_disorder_symptoms$Disorder)
 
 # Split the data into training and testing sets
 set.seed(123)
 trainIndex <- createDataPartition(Mental_disorder_symptoms$Disorder, p = .7, list = FALSE, times = 1)
 Mental_train <- Mental_disorder_symptoms[trainIndex, ]
 Mental_test <- Mental_disorder_symptoms[-trainIndex, ]
 
 # -----------------------
 # Multinomial Logistic Regression
 # -----------------------
 mlogit_model <- multinom(Disorder ~ ., data = Mental_train)
 
 # Predict on the test set
 mlogit_pred <- predict(mlogit_model, newdata = Mental_test)
 
 # Confusion matrix
 confusion_mlogit <- confusionMatrix(mlogit_pred, Mental_test$Disorder)
 print(confusion_mlogit)
 
 # Save confusion matrix as a table
 write.csv(as.data.frame(confusion_mlogit$table), file = paste0(output_dir, "/mlogit_confusion_matrix.csv"))
 
 # -----------------------
 # Regularized Multinomial Logistic Regression (LASSO)
 # -----------------------
 # Prepare the data for LASSO
 x_train <- model.matrix(Disorder ~ . -1, data = Mental_train)
 y_train <- Mental_train$Disorder
 x_test <- model.matrix(Disorder ~ . -1, data = Mental_test)
 y_test <- Mental_test$Disorder
 
 # Apply LASSO regularization to reduce the number of predictors
 set.seed(123)
 cvfit <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1)
 
 # Save LASSO cross-validation plot
 png(filename = paste0(output_dir, "/lasso_cv_plot.png"))
 plot(cvfit)
 dev.off()
 
 # Get the coefficients at the optimal lambda
 lasso_coef <- coef(cvfit, s = "lambda.min")
 
 # Extract the names of the non-zero coefficients (selected features)
 selected_features <- unique(unlist(lapply(lasso_coef, function(x) rownames(x)[x[, 1] != 0])))
 
 # Check if any features were selected and if they exist in the training data
 selected_features <- selected_features[selected_features %in% colnames(x_train)]
 
 if (length(selected_features) > 0) {
   x_train_selected <- x_train[, selected_features, drop = FALSE]
   x_test_selected <- x_test[, selected_features, drop = FALSE]
   
   # Refit multinomial logistic regression with selected features
   selected_formula <- as.formula(paste("Disorder ~", paste(selected_features, collapse = " + ")))
   mlogit_model_lasso <- multinom(selected_formula, data = Mental_train)
   
   # Predict on the test set using the LASSO-selected features
   mlogit_pred_lasso <- predict(mlogit_model_lasso, newdata = Mental_test)
   
   # Confusion matrix for LASSO-regularized model
   confusion_mlogit_lasso <- confusionMatrix(mlogit_pred_lasso, Mental_test$Disorder)
   print(confusion_mlogit_lasso)
   
   # Save confusion matrix for LASSO model as a table
   write.csv(as.data.frame(confusion_mlogit_lasso$table), file = paste0(output_dir, "/mlogit_lasso_confusion_matrix.csv"))
 } else {
   print("No features were selected by LASSO.")
 }
 
 # -----------------------
 # Random Forest Classifier
 # -----------------------
 rf_model <- randomForest(Disorder ~ ., data = Mental_train)
 
 # Predict on the test set
 rf_pred <- predict(rf_model, newdata = Mental_test)
 
 # Confusion matrix for Random Forest
 confusion_rf <- confusionMatrix(rf_pred, Mental_test$Disorder)
 print(confusion_rf)
 
 # Save confusion matrix as a table
 write.csv(as.data.frame(confusion_rf$table), file = paste0(output_dir, "/rf_confusion_matrix.csv"))
 
 # Variable importance plot for Random Forest
 importance_df <- as.data.frame(importance(rf_model))
 importance_df$Feature <- rownames(importance_df)
 
 ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
   geom_bar(stat = "identity") +
   coord_flip() +
   labs(title = "Variable Importance - Random Forest", x = "Features", y = "Mean Decrease in Gini") +
   theme_minimal()
 
 # Save variable importance plot
 ggsave(filename = paste0(output_dir, "/rf_importance.png"))
 