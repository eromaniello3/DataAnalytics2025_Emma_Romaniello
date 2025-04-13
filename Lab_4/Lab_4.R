#LAB 4 - Emma Romaniello 

library("ggplot2")
library("readr")

#read dataset
dataset <- wine

#Assigning columns 
colnames(wine) <- c(
  "Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", 
  "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", 
  "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315", "Proline"
)

head(wine)

#Separate X and target Class 
X <- wine[, -1]  
y <- wine$Class

#Standardize the data (mean=0, std=1)
X_scaled <- scale(X)

#Compute PCA
pca_result <- prcomp(X_scaled, center = TRUE, scale. = FALSE) 

#Summary 
summary(pca_result)

#Plotting first two PCs

#Extracting principal components
pc1 <- pca_result$x[, 1]
pc2 <- pca_result$x[, 2]

#Data frame for plotting
pca_df <- data.frame(PC1 = pc1, PC2 = pc2, Class = as.factor(y))

#Plot 
ggplot(pca_df, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(size = 3) +
  labs(
    title = "Wine Dataset: First Two Principal Components",
    x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 2), "% variance)"),
    y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 2), "% variance)")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "orange", "purple"), name = "Wine Class")

#Get loadings from PCA
loadings <- pca_result$rotation

#Extract loadings for PC1
pc1_loadings <- loadings[, 1]

#Sort in descending order of absolute contribution
sorted_loadings <- sort(abs(pc1_loadings), decreasing = TRUE)
sorted_loadings

#Converting loadings to data frame for plotting
loadings_df <- data.frame(
  Variable = names(pc1_loadings),
  Loading = pc1_loadings
)

# Order by absolute loading value
loadings_df <- loadings_df[order(-abs(loadings_df$Loading)), ]

ggplot(loadings_df, aes(x = reorder(Variable, -abs(Loading)), y = Loading, fill = Loading > 0)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Variable Contributions to PC1",
    x = "Original Variable",
    y = "Loading (Weight in PC1)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("green", "blue"), name = "Direction")

#Dropping Least Contributing Variables 

#Extracting absolute loadings for PC1
abs_loadings <- abs(pca_result$rotation[, 1])

#Sorting variables by contribution strength
sorted_vars <- names(sort(abs_loadings, decreasing = TRUE))

#Keep top N variables
top_n <- 5
selected_vars <- sorted_vars[1:top_n]

#Subsetting the original scaled data
X_reduced <- X_scaled[, selected_vars]

#PPCA on the reduced dataset
pca_reduced <- prcomp(X_reduced, center = TRUE, scale. = FALSE)

summary(pca_reduced)

#Extracting new PCs
pc1_new <- pca_reduced$x[, 1]
pc2_new <- pca_reduced$x[, 2]

#Data frame for plotting
pca_reduced_df <- data.frame(
  PC1 = pc1_new,
  PC2 = pc2_new,
  Class = as.factor(y)
)

#Plot
ggplot(pca_reduced_df, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(size = 3) +
  labs(
    title = "PCA After Dropping Weak Contributors to PC1",
    x = paste0("PC1 (", round(summary(pca_reduced)$importance[2, 1] * 100, 2), "% variance)"),
    y = paste0("PC2 (", round(summary(pca_reduced)$importance[2, 2] * 100, 2), "% variance)")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "orange", "purple"), name = "Wine Class")


#kNN Model 

dataset <- wine

#Assigning columns
colnames(wine) <- c(
  "Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", 
  "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", 
  "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315", "Proline"
)

#Converting 'Class' to a factor 
wine$Class <- as.factor(wine$Class)

str(wine)

#Training and Test sets 

#Setting seed for reproducibility
set.seed(123)

#Splitting data (70% training, 30% testing)
library(caret)
train_index <- createDataPartition(wine$Class, p = 0.7, list = FALSE)
train_data <- wine[train_index, ]
test_data <- wine[-train_index, ]

#Standardizing features (mean=0, sd=1)
preproc <- preProcess(train_data[, -1], method = c("center", "scale"))
train_scaled <- predict(preproc, train_data)
test_scaled <- predict(preproc, test_data)

library(class)

#Predictors (X) and target (y)
X_train <- train_scaled[, -1]
y_train <- train_scaled$Class
X_test <- test_scaled[, -1]
y_test <- test_scaled$Class

#Train kNN (k=5)
k <- 5
knn_pred <- knn(train = X_train, test = X_test, cl = y_train, k = k)

#Confusion matrix
confusionMatrix(knn_pred, y_test)

#Using caret to tune k
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold CV
knn_tune <- train(
  Class ~ ., 
  data = train_scaled, 
  method = "knn", 
  trControl = ctrl,
  tuneGrid = expand.grid(k = 1:20)  # Test k from 1 to 20
)

#Best k
knn_tune$bestTune

#Accuracy vs. k
plot(knn_tune)

#Retrain with best k
best_k <- knn_tune$bestTune$k
final_pred <- knn(train = X_train, test = X_test, cl = y_train, k = best_k)

#Final metrics
confusionMatrix(final_pred, y_test)


#kNN with projected data

dataset <- wine

#Assigning columns
colnames(wine) <- c(
  "Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", 
  "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", 
  "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315", "Proline"
)

#Standardize features
X_scaled <- scale(wine[, -1])

#Perform PCA
pca_result <- prcomp(X_scaled, center = TRUE, scale. = FALSE)

#Extracting first 3 PCs
pc_scores <- pca_result$x[, 1:3]
pca_df <- data.frame(Class = wine$Class, pc_scores)

set.seed(123)
library(caret)
train_index <- createDataPartition(pca_df$Class, p = 0.7, list = FALSE)
train_pca <- pca_df[train_index, ]
test_pca <- pca_df[-train_index, ]

library(class)

#Defining predictors (PC1-PC3) and target (Class)
X_train <- train_pca[, -1]
y_train <- train_pca$Class
X_test <- test_pca[, -1]
y_test <- test_pca$Class

#kNN (k=5)
k <- 5
knn_pca_pred <- knn(train = X_train, test = X_test, cl = y_train, k = k)

confusionMatrix(knn_pca_pred, y_test)

library(class)
library(caret)

#Splitting data 
X_train <- train_pca[, -1]
y_train <- train_pca$Class
X_test <- test_pca[, -1]
y_test <- test_pca$Class

#kNN (k=5)
k <- 5
knn_pca_pred <- knn(train = X_train, test = X_test, cl = y_train, k = k)

#Factor levels match
knn_pca_pred <- factor(knn_pca_pred, levels = levels(y_test))

confusionMatrix(knn_pca_pred, y_test)

#Train kNN with caret (handles factors automatically)
ctrl <- trainControl(method = "cv", number = 10)
knn_fit <- train(
  Class ~ ., 
  data = train_pca, 
  method = "knn", 
  trControl = ctrl,
  tuneGrid = expand.grid(k = 1:10)
)

#Predict and evaluate
pred_caret <- predict(knn_fit, test_pca)
confusionMatrix(pred_caret, test_pca$Class)


#Comparing

#Train kNN on original data 
knn_original <- knn(train = X_train_orig, test = X_test_orig, cl = y_train, k = 5)
knn_original <- factor(knn_original, levels = levels(y_test))

#Train kNN on PCA-transformed data (PC1-PC3)
knn_pca <- knn(train = X_train_pca, test = X_test_pca, cl = y_train, k = 5)
knn_pca <- factor(knn_pca, levels = levels(y_test))

conf_original <- confusionMatrix(knn_original, y_test)
print(conf_original$table)

conf_pca <- confusionMatrix(knn_pca, y_test)
print(conf_pca$table)


#Extract class-wise metrics
metrics_original <- data.frame(
  Class = c("1", "2", "3"),
  Precision = conf_original$byClass[, "Precision"],
  Recall = conf_original$byClass[, "Recall"],
  F1 = conf_original$byClass[, "F1"]
)
print(metrics_original)


metrics_pca <- data.frame(
  Class = c("1", "2", "3"),
  Precision = conf_pca$byClass[, "Precision"],
  Recall = conf_pca$byClass[, "Recall"],
  F1 = conf_pca$byClass[, "F1"]
)
print(metrics_pca)

library(ggplot2)
library(tidyr)

#Combine metrics for plotting
metrics_combined <- rbind(
  cbind(Model = "Original", metrics_original),
  cbind(Model = "PCA (3 PCs)", metrics_pca)
)

#Plot F1-scores
ggplot(metrics_combined, aes(x = Class, y = F1, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "F1-Score Comparison by Class", y = "F1-Score") +
  theme_minimal()