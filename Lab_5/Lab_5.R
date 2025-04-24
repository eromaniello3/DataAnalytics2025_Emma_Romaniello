#LAB 5 - Emma Romaniello
#Referenced lectures 

library(caret)
library(e1071)

#Wine Dataset
dataset <- wine
dataset$Type <- as.factor(dataset$Type)

#Train-test split
set.seed(123)
train.indexes <- sample(nrow(dataset), 0.7 * nrow(dataset))
train <- dataset[train.indexes, ]
test <- dataset[-train.indexes, ]

#Separate x & y 
X <- train[, 2:14]  #First column is Type (class), rest are features
Y <- train[, 1]

#Feature boxplots
boxplot(X, main = "Wine Features")

#Class label distributions
plot(Y)

#Feature-class plots 
library(caret)
featurePlot(x = X[, c("Alcohol", "Malic.acid")], y = Y, plot = "ellipse")
featurePlot(x = X[, c("Alcohol", "Malic.acid")], y = Y, plot = "box")

scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = X[, c("Alcohol", "Malic.acid")], y = Y, plot = "density", scales = scales)

#Scatter plot
library(ggplot2)
ggplot(train, aes(x = Alcohol, y = Malic.acid, colour = Type)) +
  geom_point()

#Train SVM model - linear kernel
library(e1071)
svm.mod0 <- svm(Type ~ Alcohol + Malic.acid, data = train, kernel = 'linear')
svm.mod0

plot(svm.mod0, data = train, formula = Alcohol ~ Malic.acid, svSymbol = "x", dataSymbol = "o")

#Train predictions and evaluation
train.pred <- predict(svm.mod0, train)
cm <- confusionMatrix(train.pred, train$Type)
print(cm)

#Decision boundary plot
make.grid <- function(x, n = 75) {
  grange <- apply(x, 2, range)
  x1 <- seq(from = grange[1, 1], to = grange[2, 1], length = n)
  x2 <- seq(from = grange[1, 2], to = grange[2, 2], length = n)
  expand.grid(Alcohol = x1, Malic.acid = x2)
}

x <- train[, c("Alcohol", "Malic.acid")]
y <- as.numeric(train$Type)

xgrid <- make.grid(x)
ygrid <- predict(svm.mod0, xgrid)

plot(xgrid, col = c("red", "green", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 1, pch = 19)
points(x[svm.mod0$index, ], pch = 5, cex = 2)

#Train SVM model - radial kernel
svm.mod1 <- svm(Type ~ Alcohol + Malic.acid, data = train, kernel = 'radial')
svm.mod1

plot(svm.mod1, train, Malic.acid ~ Alcohol)

#Tuned SVM
tuned.svm <- tune.svm(Type ~ Alcohol + Malic.acid, data = train, 
                      kernel = 'radial',
                      gamma = 10^(-3:1), 
                      cost = 10^(-1:2))

summary(tuned.svm)
svm.mod2 <- tuned.svm$best.model

#Test set evaluation
test.pred <- predict(svm.mod2, test)
cm_test <- confusionMatrix(test.pred, test$Type)
print(cm_test)


#NY Housing Dataset

dataset <- NY_House_Dataset

#dataset <- wine
#dataset$Type <- as.factor(dataset$Type)

names(dataset)

#Split train/test
train.indexes <- sample(nrow(dataset),0.75*nrow(dataset))

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

x <- dataset[,2:4] 
y <- as.factor(as.matrix(dataset[,1]))

#Feature boxplots
boxplot(X, main="iris features")

#Class label distributions
plot(Y)

#Feature-class plots
featurePlot(x=x, y=y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

ggplot(dataset, aes(x = Alcohol, y = Ash, colour = Type)) +
  geom_point()

svr.mod0 <- svm(PRICE ~ PROPERTYSQFT, train)

summary(svr.mod0)

svr.pred <- predict(svr.mod0, test)

svr.pred <- cbind(test$PRICE,svr.pred)

ggplot(svr.pred, aes(x = V1, y = svr.pred)) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod0 <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset, kernel="radial")

summary(svr.mod0)

svr.pred <- predict(svr.mod0, dataset)

svr.outs <- data.frame(real=log10(dataset$PRICE), pred=svr.pred)

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod0 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset)

summary(svr.mod0)

svr.pred <- predict(svr.mod0, dataset)

svr.outs <- data.frame(real=log10(dataset$PRICE), pred=svr.pred)

ggplot(svr.outs, aes(x = real, y = pred)) +
  geom_point() +
  stat_smooth(method = "lm")