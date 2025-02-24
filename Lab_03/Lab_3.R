#Lab_3
###################
##### Abalone #####
###################

#read dataset
abalone <- read.csv("C:/Users/Emma Romaniello/Downloads/abalone_dataset.csv")
dataset <- abalone
View(abalone)

#add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


#setting age.group
dataset$age.group[dataset$rings<=8] <- "young"
dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"


#Exercise 1
#knn model 1
sqrt(4176) #for k

Knn.predicted <- knn(train =dataset[,2:4], test = dataset[,2:4], cl = dataset$age.group, k=65)

contingency.table <- table(knn.predicted, dataset$age.group, dnn=list('predicted','actual'))

contingency.table

#classification accuracy
sum(diag(contingency.table))/length(dataset$age.group)

k.list <- c(59,61,63,65,67,69,71)
#empty list for accuracy
accuracy.list <- c()
#train & predict model for k, compute accuracy to append it to list
for (k in k.list) {
  knn.predicted <- knn(train =dataset[,2:4], test = dataset[,2:4], cl = dataset$age.group, k=k)
  contigency.table <-table(knn.predicted, dataset$age.group, dnn=list('predicted','actual'))
  accuracy <- sum(diag(contigency.table))/length(dataset$age.group)
  accuracy.list <- c(accuracy.list,accuracy)
}
plot(k.list, accuracy.list, type = 'b')

#knn model 2
knn.predicted <- knn(train =dataset[,3:5], test = dataset[,3:5], cl = dataset$age.group, k=65)

contingency.table <- table(knn.predicted, dataset$age.group, dnn=list('predicted','actual'))

contingency.table

#classification accuracy
sum(diag(contigency.table))/length(dataset$age.group)

k.list <- c(59,61,63,65,67,69,71)
#empty list for accuracy
accuracy.list <- c()
#train & predict model for k, compute accuracy to append it to list
for (k in k.list) {
  knn.predicted <- knn(train =dataset[,3:5], test = dataset[,3:5], cl = dataset$age.group, k=k)
  contigency.table <-table(knn.predicted, dataset$age.group, dnn=list('predicted','actual'))
  accuracy <- sum(diag(contigency.table))/length(dataset$age.group)
  accuracy.list <- c(accuracy.list,accuracy)
}
plot(k.list, accuracy.list, type = 'b')



#exercise 2
k.list <- c(2,3,4,5)

wcss.list <- c()

for (k in k.list) {
  abalone.km <- kmeans(dataset[,2:4], centers = k)
  wcss <- abalone.km$tot.withinss
  wcss.list <- c(wcss.list,wcss)
  #get and plot clustering output
  assigned.clusters <- as.factor(abalone.km$cluster)
}
plot(k.list,wcss.list,typbe = 'b')