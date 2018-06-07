#Reading Data
df <- read.csv('loan_data.csv')

#Converting to factors
df$inq.last.6mths <- factor(df$inq.last.6mths)
df$delinq.2yrs <- factor(df$delinq.2yrs)
df$pub.rec <- factor(df$pub.rec)
df$not.fully.paid <- factor(df$not.fully.paid)
df$credit.policy <- factor(df$credit.policy)

#FICO score is a type of credit score created by the 'Fair Isaac Corporation'.Lenders use borrowers
#FICO scores along with other details on borrowers credit reports to assess credit risk and 
#determine whether to extend credit.

###Exploratory Data Analysis
library(ggplot2)

#Histogram of fico
ggplot(df, aes(x = fico)) + geom_histogram(aes(fill= not.fully.paid), color='black',bins = 30) 

#Barplot of purpose counts, colored by not.fully.paid
ggplot(df, aes(x = factor(purpose))) + geom_bar(aes(fill= not.fully.paid), position = "dodge") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Scatterplot of fico score versus int.rate
ggplot(df, aes(x = int.rate, y = fico)) + geom_point(aes(color = not.fully.paid)) + theme_bw()

##Scatterplot of fico score versus int.rate
ggplot(df, aes(x = int.rate, y = fico)) + geom_point(aes(color = not.fully.paid)) + theme_bw()

#Splitting the Data
library(caTools)
sample <- sample.split(df, SplitRatio = 0.7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)

########Model(Support Vector Machines)
library(e1071)

#Training Model
tuned.svm <- tune(svm, train.x = not.fully.paid~., data = train , kernel= 'radial',ranges = list(cost= c(1,10,100)) ,gamma =c(0.1,.2,.3,1,10) )

#Prediction 
tuned.model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(tuned.model,test[1:13])

#Model Evaluation
table(predicted.values,test$not.fully.paid)
