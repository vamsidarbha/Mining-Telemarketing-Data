library("e1071", lib.loc="~/R/win-library/3.1")
library("ROSE", lib.loc="~/R/win-library/3.1")
library("pROC", lib.loc="~/R/win-library/3.1")
library(caret) 

#Loading Training Data into train data frame
train = read.csv("C:\\Users\\PHANISANTHOSHVAMSIDE\\Downloads\\Study\\Fall\\srs\\bank-additional\\bank-additional-train.csv",as.is=TRUE,header=TRUE)

#Loading Test Data into test data frame
test = read.csv("C:\\Users\\PHANISANTHOSHVAMSIDE\\Downloads\\Study\\Fall\\srs\\bank-additional\\bank-additional-test.csv",as.is=TRUE,header=TRUE)

#Adding Extra Columns to Training Sample as Training set is baised with only 10 percent of positive Samples
balancedSample<-ovun.sample(y~., data=train,p=0.5, method="over")$data

#Writing the new genarated dataframe with balanced class labels to bank-additional-balancedSample.csv
write.csv(balancedSample,"C:\\Users\\PHANISANTHOSHVAMSIDE\\Downloads\\Study\\Fall\\srs\\bank-additional\\bank-additional-balancedSample.csv")

#Loading  balanced data file to trainbalanced dataframe
trainbalanced = read.csv("C:\\Users\\PHANISANTHOSHVAMSIDE\\Downloads\\Study\\Fall\\srs\\bank-additional\\bank-additional-balancedSample.csv",as.is=TRUE,header=TRUE)

#Building SVM Model with balanced Training Set
modelSVMBalanced = svm(y~.-ID,data=trainbalanced,type="C-classification",kernel="linear")

#Building SVM Model with Original Training Set(UnBalanced)
modelSVMUnBalanced = svm(y~.,data=train,type="C-classification",kernel="linear")

#Predicting Class label of test set using balanced SVM model built
predSVMBalanced = predict(modelSVMBalanced,newdata=test)

#Predicting Class label of test set using Unbalanced SVM model built
predSVMUnBalanced = predict(modelSVMUnBalanced,newdata=test)

#confusion matrix for model accuracies and evaluation of results.
confusionMatrix(predSVMUnBalanced,test$y, positive = 'yes')

confusionMatrix(predSVMBalanced,test$y, positive = 'yes')

--------------------------------------------------------------------------------------
#Random Forest Classification
library("randomForest", lib.loc="~/R/win-library/3.1")

#Converting all character colimns to numeric as randomforest only accepts numeric attributes.
trainConvertBalanced <- lapply(trainbalanced, function(x) if(is.character(x)) as.numeric(factor(x))
               else (x))

trainConvertUnbalanced <- lapply(train, function(x) if(is.character(x)) as.numeric(factor(x))
                       else (x))

testConvert <- lapply(test, function(x) if(is.character(x)) as.numeric(factor(x))
                      else (x))

#building random forest model for balanced dataset
model_randomForest_Balanced = randomForest(as.factor(y)~.-ID,data=trainConvertBalanced, importance=FALSE,proximity=FALSE, xtest=NULL, ytest=NULL, ntree=20,replace=TRUE,mtry=2, corr.bias=FALSE)
#Computing contribution of each attribute to randomforest classification for this dataset using Gini measure
importance(model_randomForest_Balanced)

#building random forest model for Unbalanced dataset
model_randomForest_UnBalanced = randomForest(as.factor(y)~.,data=trainConvertUnbalanced, importance=FALSE,proximity=FALSE, xtest=NULL, ytest=NULL, ntree=20,replace=TRUE,mtry=2, corr.bias=FALSE)
#Computing contribution of each attribute to randomforest classification for this dataset using Gini measure

importance(model_randomForest_UnBalanced)

#predicting class label for testset from model built on balanced dataset
pred_randomForest_Balanced = predict(model_randomForest_Balanced,newdata=testConvert)

#predicting class label for testset from model built on unbalanced dataset
pred_randomForest_UnBalanced = predict(model_randomForest_UnBalanced,newdata=testConvert)

#confusion matrix for model accuracies and evaluation of results.
confusionMatrix(pred_randomForest_Balanced,testConvert$y, positive = '2')

confusionMatrix(pred_randomForest_UnBalanced,testConvert$y, positive = '2')


------------------------------------------------------------------------------------------------------------
#Feature Analysis using RandomForests

#BankClientData features

model_randomForest_Balanced_BankClientData<-randomForest(as.factor(y)~age+job+marital+education+default+housing+loan
             ,data=trainConvertBalanced, importance=FALSE,proximity=FALSE, xtest=NULL, ytest=NULL, ntree=20,replace=TRUE,mtry=2, corr.bias=FALSE)

pred_randomForest_Balanced_BankClientData = predict(model_randomForest_Balanced_BankClientData,newdata=testConvert) 

confusionMatrix(pred_randomForest_Balanced_BankClientData,testConvert$y, positive = '2')


#PreviousContactInfo features

model_randomForest_Balanced_PreviousContactInfo=randomForest(as.factor(y)~contact+month+day_of_week+duration
             ,data=trainConvertBalanced, importance=FALSE,proximity=FALSE, xtest=NULL, ytest=NULL, ntree=20,replace=TRUE,mtry=2, corr.bias=FALSE)

pred_randomForest_Balanced_PreviousContactInfo = predict(model_randomForest_Balanced_PreviousContactInfo,newdata=testConvert) 

confusionMatrix(pred_randomForest_Balanced_PreviousContactInfo,testConvert$y, positive = '2')


#Socio Economic features

model_randomForest_Balanced_SocioEconomic=randomForest(as.factor(y)~emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed
             ,data=trainConvertBalanced, importance=FALSE,proximity=FALSE, xtest=NULL, ytest=NULL, ntree=20,replace=TRUE,mtry=2, corr.bias=FALSE)


pred_randomForest_Balanced_SocioEconomic = predict(model_randomForest_Balanced_SocioEconomic,newdata=testConvert) 

confusionMatrix(pred_randomForest_Balanced_SocioEconomic,testConvert$y, positive = '2')
