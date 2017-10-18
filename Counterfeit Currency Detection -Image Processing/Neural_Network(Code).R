##Reading Data
df <- read.csv('bank_note_data.csv')
##EDA
library(ggplot2)
ggplot(df, aes(x = factor(Class))) + geom_bar(aes(fill= factor(Class)))
ggplot(df, aes(x= Image.Skew, y = Image.Curt)) + geom_point(aes(color= factor(Class))) +theme_dark()
ggplot(df, aes(x= Image.Var, y = Image.Curt)) + geom_point(aes(color= factor(Class))) +theme_dark()
#######
library(caTools)
library(neuralnet)
#####
#Splitting the data
sample <- sample.split(df, SplitRatio = 0.7)
train <- subset(df,sample==T)
test <- subset(df, sample==F)

###Neural n/w

nn <- neuralnet(formula = Class ~ Image.Var+ Image.Skew+ Image.Curt+ Entropy, data = train, hidden = 10,linear.output = F)

#PREDICTION
predict.class <-compute(nn,test[1:4])

#Rounding off to get class 1 and 0
predict <- apply(predict.class$net.result,1,round)

##Mse 
MSE.nn <- sum((test$Class - predict)^2)/nrow(test)

#confusion matrix of your predictions versus the real values
table(predict, test$Class)

###Comparing with randomforest
library(randomForest)
df$Class <- factor(df$Class)
##Splitting Data
sample <- sample.split(df, SplitRatio = 0.7)
train <- subset(df,sample==T)
test <- subset(df, sample==F)

##Training model 
rf<- randomForest(Class ~. , data = train)
#prediction
predict.val <- predict(rf, test[1:4]) 

###Confusion Matrix
table(predict.val, test$Class)



