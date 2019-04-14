
                            #OR/SYST-568 Applied Predictive Analytics (Spring 2019)

                         #Assignment 3 – Logistic Regression and Decision Tree Models

                                          

                                            #1. Customer Churn Study

rm(list = ls())

print(getwd())
setwd('C:/Users/murug/Desktop')
data = read.csv('Churn.csv')


#a. What is the overall churning rate in the original dataset?
  
sum(data[1])
nrow(data)
str(data)

a = 483/3333
print(a)


#b. Partition the data into training (60%) and validation (40%) (set the random seed to 12345).
#Using the training dataset, generate the scatterplot of Churn (Y axis) vs. RoamMins (X axis).

set.seed(12345)
row<-nrow(data)

trainindex <- sample(row, 0.6*row, replace=FALSE)
training <- data[trainindex,]
validation <- data[-trainindex,]

library(ggplot2)
ggplot(training, aes(x= training$RoamMins, y= training$ï..Churn, colour = training$ContractRenewal))+ 
  geom_point()+ ggtitle("Scatterplot between Churn and Roaming data") +
  xlab("Roaming data (mins)") + ylab("Customer Churns")


#c. Build a logistic regression model by including only the numerical variables using the training
#data, then apply the constructed model to the validation set and set the cutoff level at 0.5.

datanew <- data[,c(-3,-4)]
str(datanew)

trainindex1 <- sample(row, 0.6*row, replace=FALSE)
training1 <- datanew[trainindex1,]
validation1 <- datanew[-trainindex1,]

mylogit<-glm(ï..Churn ~ . ,data=training1, family=binomial)
mylogit.probs<-predict(mylogit,validation1,type="response")
summary(mylogit)

library(SDMTools)
matrix = confusion.matrix(validation1$ï..Churn,mylogit.probs,threshold=0.5)    
matrix

library(InformationValue)

misClasificError <- misClassError(validation1$ï..Churn, mylogit.probs, threshold = 0.5)
print(misClasificError)

print(paste('Accuracy',1-misClasificError))

accumeasure <- accuracy(validation1$ï..Churn, mylogit.probs, threshold = 0.5)
print(accumeasure)

#d. Using all the predictors, build a full logistic regression model using the training data. Which
#categorical predictor has the most impact on the response variable? How does the odds of
#churning change with a change (e.g. changing from 0 to 1) in this categorical variable? 
  

set.seed(12345)

row1<-nrow(data)

trainindex2 <- sample(row, 0.6*row, replace=FALSE)
training2 <- data[trainindex2,]
validation2 <- data[-trainindex2,]

mylogit1<-glm(ï..Churn ~ . ,data=training2, family=binomial)
mylogit.probs1<-predict(mylogit,validation2,type="response")

matrix1 = confusion.matrix(validation2$ï..Churn,mylogit.probs1,threshold=0.5)    
matrix1

summary(mylogit1)

exp(coef(mylogit1))
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))

#intrepretation : 
#We could see from the table that the  “ ContractRenewal“ has the most impact on the response variable. 
#Although CustServCalls and ContractRenewal are significant in terms of P value,
#the Z-statistic estimate for ContractRenewal has higher (-ve) magnitude and
#hence cannot be zero or we could say the corresponding variable matters.
  

misClasificError2 <- misClassError(validation2$ï..Churn, mylogit.probs1, threshold = 0.5)
print(misClasificError2)
summary()
sensitivity(validation2$ï..Churn, mylogit.probs1)
print(paste('Accuracy',1-misClasificError2))

accumeasure1 <- accuracy(validation2$ï..Churn, mylogit.probs1, threshold = 0.5)
print(accumeasure1)


# e. Utilizing the average net loss table (provided at the beginning of question context) along with
#the classification/confusion matrix, compare the performance of models developed in parts (c)
#and (d) with respect to the average net loss in the validation dataset using cutoff level at 0.5.

 
  
#explained in the PDF file
  



#f. Provide the validation ROC curves, AUC values and Lift charts for all the models developed
#in parts (c) and (d). Which model has better performance with respect to the AUC value. 
  

#########    For part c

library(pROC)

roc <- roc(validation1$ï..Churn,mylogit.probs)
plot(roc)

auc(roc)

mydf <-cbind(validation,mylogit.probs)
mydf$response <- as.factor(ifelse(mydf$mylogit.probs>0.5, 1, 0))

library(ROCR)
logit_scores <- prediction(predictions=mydf$mylogit.probs, labels=mydf$ï..Churn)

logit_lift <- performance(logit_scores, measure="lift", x.measure="rpp")

plot(logit_lift,
     main="Lift Chart",
     #xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")



#######   For part d


roc2 <- roc(validation2$ï..Churn,mylogit.probs1)
plot(roc2)

auc(roc2)

mydf2 <-cbind(validation2,mylogit.probs1)
mydf2$response1 <- as.factor(ifelse(mydf2$mylogit.probs1>0.5, 1, 0))

library(ROCR)
logit_scores3 <- prediction(predictions=mydf2$mylogit.probs1, labels=mydf2$ï..Churn)

logit_lift2 <- performance(logit_scores3, measure="lift", x.measure="rpp")

plot(logit_lift2,
     main="Lift Chart",
     #xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")





                ###########################  2. Web Robot Detection  #########################


robot = read.csv('Web Robot.csv')
head(robot)
str(robot)

# g.) What is the overall proportion of the Web Robot in the original dataset?
  

head(robot$Robot)
sums <- table(robot$Robot)
addmargins(sums)


# h.) Partition the data into training (60%) and validation (40%) (set the random seed to 12345).
#Using the same training and validation sets, build a classification tree with the training data,
#report the tree graph. Calculate and report the Gini index for the split of the top node. 

library(tree)
library(rpart)
library(rattle)				      	# Fancy tree plot
library(rpart.plot)			    	# Enhanced tree plots
library(RColorBrewer)			  	# Color selection for fancy tree plot
library(party)					      # Alternative decision tree algorithm
library(partykit)				      # Convert rpart object to BinaryTree
library(caret)


robot[,'Robot']<-factor(robot[,'Robot'])

robot = data.frame(robot)

row4<-nrow(robot)



set.seed(12345)
trainindex3 <- sample(row4, 0.6*row4, replace=FALSE)
training3 <- robot[trainindex3,]
robot.test = robot[-trainindex3,]


tree1 <- tree(as.factor(Robot)~., training3)
plot(tree1)
text(tree1,pretty=0)   # pretty=0 includes the category names for any qualitative predictors


library(REAT)

tree.pred = predict(tree1 , robot.test,type="class")

#MisclassError rate

misClasificError4<-   (119+23)/(169+119+23+63) 
print(misClasificError4)

print(paste('Accuracy',1-misClasificError4))

matrix4 = confusion.matrix(robot.test$Robot,tree.pred,threshold=0.5)    
matrix4



# i.) Suppose we now have new observation with some missing values. The only information
#available to us is as follows: Depth=1, Breadth=1, ImagePages=0.75. Following the structure of
#classification tree you obtained in part (c), is it possible to make a predictor whether this web
#access is from Robot or not? 
  


  #Answer Explained in the PDF
  
  
    
  
# j.) With the same training and validation sets, use the random forest method to build a classification
#tree with the training data, generate the variable importance plot and report the top three most
#important variables. 

library(randomForest)
model1 <- randomForest(Robot~., training3, importance = TRUE)
model1
  
importance(model1)        
varImpPlot(model1)  

model_dt_vs = predict(model1, newdata = robot.test)
table(model_dt_vs, robot.test$Robot)
matrix3 = confusion.matrix(robot.test$Robot ,model_dt_vs,threshold=0.5)    
matrix3


misClasificError5 <- misClassError(robot.test$Robot, model_dt_vs, threshold = 0.5)
print(misClasificError5)
print(paste('Accuracy',1-misClasificError5))

  

# k.) Compare the models you obtained in parts (b) and (d), which model is the best with respect to
#the misclassification error rate performance on the validation dataset.


print(misClasificError4)
print(misClasificError5)


  
  
  
  