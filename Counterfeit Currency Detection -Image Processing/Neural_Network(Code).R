##Reading Data
df <- read.csv('bank_note_data.csv')

##Exploratory Data Analysis
library(ggplot2)
ggplot(df, aes(x = factor(Class))) + geom_bar(aes(fill= factor(Class)))
ggplot(df, aes(x= Image.Skew, y = Image.Curt)) + geom_point(aes(color= factor(Class))) +theme_dark()
ggplot(df, aes(x= Image.Var, y = Image.Curt)) + geom_point(aes(color= factor(Class))) +theme_dark()

#######Calling Necessary Libraries
library(caTools)
library(neuralnet)
#####

#Splitting the data(Using catools lib)
sample <- sample.split(df, SplitRatio = 0.7)
train <- subset(df,sample==T)
test <- subset(df, sample==F)

#Training Model On Train Dataset
###Neural n/w Model
nn <- neuralnet(formula = Class ~ Image.Var+ Image.Skew+ Image.Curt+ Entropy, data = train, hidden = 10,linear.output = F)

#PREDICTION (Test Dataset)
predict.class <-compute(nn,test[1:4])

#Rounding off to get class 1 and 0
#Final predicted output that shows whether the Currency is genuine or not....
predict <- apply(predict.class$net.result,1,round)

###############
#EVALUATING TRAINED MODEL 

##Mean Square Error(MSE) 
MSE.nn <- sum((test$Class - predict)^2)/nrow(test)

#Confusion matrix of your predictions versus the real values
table(predict, test$Class)




