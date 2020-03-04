getwd()
setwd("E://IDS 572//Assg2")
library(readxl)
GermanCred <- read_excel("German Credit.xls")
View(GermanCred)
str(GermanCred)


library(dplyr)
#or
German_Credit <-mutate(GermanCred,
                       CHK_ACCT = as.factor(CHK_ACCT),
                       HISTORY = as.factor(HISTORY),
                       NEW_CAR = as.factor(NEW_CAR),
                       USED_CAR = as.factor(USED_CAR),
                       FURNITURE = as.factor(FURNITURE),
                       EDUCATION = as.factor(EDUCATION),
                       RETRAINING = as.factor(RETRAINING),
                       SAV_ACCT= as.factor(SAV_ACCT),
                       EMPLOYMENT = as.factor(EMPLOYMENT),
                       MALE_DIV=as.factor(MALE_DIV),
                       MALE_SINGLE = as.factor(MALE_SINGLE),
                       MALE_MAR_or_WID = as.factor(MALE_MAR_or_WID),
                       GUARANTOR = as.factor(GUARANTOR),
                       PRESENT_RESIDENT = as.factor(PRESENT_RESIDENT),
                       REAL_ESTATE = as.factor(REAL_ESTATE),
                       PROP_UNKN_NONE =as.factor(PROP_UNKN_NONE),
                       OTHER_INSTALL = as.factor(OTHER_INSTALL),
                       RENT = as.factor(RENT),
                       OWN_RES = as.factor(OWN_RES),
                       JOB = as.factor(JOB),
                       TELEPHONE = as.factor(TELEPHONE),
                       FOREIGN = as.factor(FOREIGN),
                       RESPONSE = as.factor(RESPONSE))

str(German_Credit)
View(German_Credit)
#a.) 
#Proportion of good to bad cases
good_bad <- table(GermanCred$RESPONSE)
prop.table(good_bad)


#Making the RANDOM FOREST  model
set.seed(2)
sample=sample(1:nrow(German_Credit),floor(nrow(German_Credit)*0.6))
train <-German_Credit[sample, ]
test <-German_Credit[-sample,]
nrow(train)
nrow(test)

install.packages("randomForest")
library(randomForest)

#attributes found significant from logistic regression model, performed on this data earlier.
#CHK_ACCT+AMOUNT+DURATION+INSTALL_RATE+OTHER_INSTALL+SAV_ACCT+HISTORY
model1 <- randomForest(RESPONSE~CHK_ACCT+AMOUNT+DURATION+INSTALL_RATE+OTHER_INSTALL+SAV_ACCT+HISTORY+OWN_RES,data=train,ntree=500,mtry=2,importance=TRUE,proximity=TRUE)
model1


#Testing on Training dataset
#Predictions on training dataset
Predict_Train<-predict(model1,train,type = "class")

#Confusion matrix for evaluating the model on training dataset
confusionMatrix(Predict_Train,train$RESPONSE)


#Testing on Testing dataset
#Predictions on training dataset
Predict_Test<-predict(model1,test,type = "class")

#Confusion matrix for evaluating the model on Testing dataset
confusionMatrix(Predict_Test,test$RESPONSE)

#View(train[,c("CHK_ACCT","AMOUNT","DURATION","INSTALL_RATE","OTHER_INSTALL","SAV_ACCT","HISTORY","OWN_RES")])
#View(train[,"RESPONSE"])
?tuneRF

#used to find optimal value of Mtry
t <-tuneRF(x=train[,c("CHK_ACCT","AMOUNT","DURATION","INSTALL_RATE","OTHER_INSTALL","SAV_ACCT","HISTORY","OWN_RES")],y=train$RESPONSE,
             stepFactor = 0.5,
             plot=TRUE,
           trace=TRUE,
             ntreetry=300,
             doBest = TRUE,
           improve=0.05)


#Checking Importance of Attributes Created RF model "model2" with all attributes. 
train_New <- train[,c(-1,-8,-18)]
model2 <- randomForest(RESPONSE~.,data=train_New,ntree=500,mtry=2,importance=TRUE,proximity=TRUE)

varImpPlot(model2,sort=T) #Graph of Values of important variables 
importance(model2) #values as per importance

#Which predictor variables are actually used in randomForest
varUsed(model2)

#Partial dependence plot
#How the value of Response depend on Numerical values like shown below.
train0New <-as.data.frame(train_New)
partialPlot(model2,train_New,DURATION,"0")
partialPlot(model2,train_New,DURATION,"1")

partialPlot(model2,train_New,AMOUNT,"0")
partialPlot(model2,train_New,AMOUNT,"1")

partialPlot(model2,train_New,AGE,"0")
partialPlot(model2,train_New,AGE,"1")


#Extracting single tree in RF
getTree(model2,1,labelVar = TRUE)
#1st RF tree. -1 indicates its terminal node and has prediction non-NA value.


#Making Decision tree
library(rpart)
model_tree <-rpart(RESPONSE~CHK_ACCT+AMOUNT+DURATION+INSTALL_RATE+OTHER_INSTALL+SAV_ACCT+HISTORY+OWN_RES,data=train,method="class")

#Predicting on train data
Predict_Train_tree <- predict(model_tree,train,type ="class")

#Confusion Matrix for evaluating the model on training dataset
confusionMatrix(Predict_Train_tree,train$RESPONSE)

#Test data
#Predicting on test data
Predict_Test_tree <- predict(model_tree,test,type ="class")

#Confusion Matrix for evaluating the model on training dataset
confusionMatrix(Predict_Test_tree,test$RESPONSE)

#Comparing Decision tree and Random Forest
#The accuracy of the training data on Random Forest is 95.83% and accuracy of training data on Decision Tree is 80.5%
#The accuracy of the testing data on Random Forest is 73.5% and accuracy of testing data on Decision Tree is 70.5%


#-----------with chaged variables from VarUsed function-------------------------------------------------------------------------------
model3_RF <- randomForest(RESPONSE~CHK_ACCT+AMOUNT+DURATION+OTHER_INSTALL+SAV_ACCT+HISTORY+OWN_RES+EMPLOYMENT+AGE,data=train,ntree=500,mtry=2,importance=TRUE,proximity=TRUE)

library(caret)

#Testing on Training dataset
#Predictions on training dataset
Predict_Train_Model3_RF<-predict(model3_RF,train,type = "class")

#Confusion matrix for evaluating the model on training dataset
confusionMatrix(Predict_Train_Model3_RF,train$RESPONSE)


#Testing on Testing dataset
#Predictions on training dataset
Predict_Test_Model3_RF<-predict(model3_RF,test,type = "class")

#Confusion matrix for evaluating the model on Testing dataset
confusionMatrix(Predict_Test_Model3_RF,test$RESPONSE)


#Decision tree on new predictor Variables
model_tree_new <-rpart(RESPONSE~CHK_ACCT+AMOUNT+DURATION+OTHER_INSTALL+SAV_ACCT+HISTORY+OWN_RES+EMPLOYMENT+AGE,data=train,method="class")

#Predicting on train data
Predict_Train_tree_new <- predict(model_tree_new,train,type ="class")

#Confusion Matrix for evaluating the model on training dataset
confusionMatrix(Predict_Train_tree_new,train$RESPONSE)

#Test data
#Predicting on test data
Predict_Test_tree_new <- predict(model_tree_new,test,type ="class")

#Confusion Matrix for evaluating the model on training dataset
confusionMatrix(Predict_Test_tree_new,test$RESPONSE)


#After adding Age and Employment and removing Installment_Rate
#The accuracy of the training data on Random Forest is 99.67% and accuracy of training data on Decision Tree is 81.67%
#The accuracy of the testing data on Random Forest is 73.5% and accuracy of testing data on Decision Tree is 69.75%

#----------------------------------------------------------------------------------------------------------------------------------------------


#C.)
#The Random forest model is better as the accuracy of RF model is more than the DT model both on training and testing dataset.


#d.)