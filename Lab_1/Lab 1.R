#Lab_1
EPI_data <- read.csv("C:/Users/Emma Romaniello/Downloads/epi2024results06022024.csv")
View(EPI_data)
attach(EPI_data) # sets the ‘default’ object
EPI.new # prints out values EPI_data$EPI.new
NAs <- is.na(EPI.new) # records True values if the value is NA 
EPI.new.noNAs <- EPI.new[!NAs] # filters out NA values, new array 
summary(EPI.new) # stats
fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new)

hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.))  
rug(EPI.new)

boxplot(EPI.new, APO.new) 

x<-seq(20,80,1) 

q<- dnorm(x,mean=42, sd=5,log=FALSE) 

lines(x,q)
lines(x,.4*q) 

q<-dnorm(x,mean=65, sd=5,log=FALSE) 

lines(x,.12*q) 

#Exercise 2
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 

qqnorm(EPI.new); qqline(EPI.new) 

qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)

qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#Exercise 2a_1
BDH.new 
NAs <- is.na(BDH.new) # records True values if the value is NA 
BDH.new.noNAs <- BDH.new[!NAs] # filters out NA values, new array 
summary(BDH.new) # stats
fivenum(BDH.new,na.rm=TRUE)
stem(BDH.new)

plot(ecdf(BDH.new), do.points=FALSE, verticals=TRUE) 

qqnorm(BDH.new); qqline(BDH.new) 

qqplot(rnorm(250), BDH.new, xlab = "Q-Q plot for norm dsn") 
qqline(BDH.new)

qqplot(rt(250, df = 5), BDH.new, xlab = "Q-Q plot for t dsn") 
qqline(BDH.new)

#Exercise 2a_2
MHP.new 
NAs <- is.na(MHP.new) # records True values if the value is NA 
MHP.new.noNAs <- MHP.new[!NAs] # filters out NA values, new array 
summary(MHP.new) # stats
fivenum(MHP.new,na.rm=TRUE)
stem(MHP.new)

plot(ecdf(MHP.new), do.points=FALSE, verticals=TRUE) 

qqnorm(MHP.new); qqline(MHP.new) 

qqplot(rnorm(250), MHP.new, xlab = "Q-Q plot for norm dsn") 
qqline(MHP.new)

qqplot(rt(250, df = 5), MHP.new, xlab = "Q-Q plot for t dsn") 
qqline(MHP.new)
