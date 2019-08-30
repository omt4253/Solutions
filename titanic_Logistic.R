setwd('F:/Imarticus/Practice')
titanic_train=read.csv("train.csv",header = T,stringsAsFactors = F)
str(titanic_train)

titanic_test=read.csv("test.csv",header=T,stringsAsFactors = F)
str(titanic_test)
#####################
titanic_train$Name = NULL
#titanic_train$Sex = NULL
#titanic_train$PassengerId = NULL


titanic_train$Survived = factor(titanic_train$Survived)
titanic_train$Pclass = factor(titanic_train$Pclass)
titanic_train$Sex = factor(titanic_train$Sex)
titanic_train$SibSp=factor(titanic_train$SibSp)
titanic_train$Parch=factor(titanic_train$Parch)
titanic_train$Embarked=factor(titanic_train$Embarked)

colSums(is.na(titanic_train))

######################Preprocessing###########
#rm(titanic_test)
####Duplicate sex columns#####
#titanic_test$Sex = NULL
#titanic_test$PassengerId = NULL
titanic_test$Name = NULL



titanic_test$Survived = factor(titanic_test$Survived)
titanic_test$Pclass = factor(titanic_test$Pclass)
titanic_test$Sex = factor(titanic_test$Sex)
titanic_test$SibSp=factor(titanic_test$SibSp)
titanic_test$Parch=factor(titanic_test$Parch)
titanic_test$Embarked=factor(titanic_test$Embarked)

colSums(is.na(titanic_test))
colSums(is.na(titanic_train))

titanic_train$Age[is.na(titanic_train$Age)]=median(titanic_train$Age,na.rm= T)


titanic_test$Age[is.na(titanic_test$Age)]=median(titanic_test$Age,na.rm= T)
titanic_test$Fare[is.na(titanic_test$Fare)]=median(titanic_test$Fare,na.rm= T)


colNames(titanic_train)
colNames(titanic_test)

####model1###########
titanic_train1 = titanic_train

logreg=glm(Survived ~ Age +SibSp ,family = 'binomial',data=titanic_train)
summary(logreg)

plot(logreg)


pred_sig=predict(logreg,titanic_test,type='response')
pred_sig
prediction_sig=ifelse(pred_sig <= 0.5,0,1)

barplot(prediction_sig)
plot(prediction_sig)

head(prediction_sig,20)

table(prediction_sig)

check2=data.frame(prediction_sig)
check1
write.csv(check2,"titanic2.csv")
#################################################
library(caret)
cfm=confusionMatrix(factor(prediction_sig),titanic_train$Survived,postive='1')

###model2################
##Parch clmn has value 9
#rm(titanic_test1)
titanic_test1 = titanic_test
titanic_test1$Parch=as.character(titanic_test1$Parch)
titanic_test1$Parch[titanic_test1$Parch ==9]=6
titanic_test1$Parch=as.factor(titanic_test1$Parch)
str(titanic_test1)
levels(titanic_test1$Parch)
levels(titanic_train1$Parch)



logreg2=glm(Survived ~ Age +SibSp+Parch ,family = 'binomial',data=titanic_train1)
summary(logreg2)
pred_sig2=predict(logreg2,titanic_test1,type='response')
pred_sig2
prediction_sig2=ifelse(pred_sig2 <= 0.4,0,1)
head(prediction_sig2)

table(prediction_sig2)


pairs(titanic_train)
####model3###################
install.packages('randomForest')
library(randomForest)
ra1=randomForest(Survived ~ +Age+SibSp+Parch,data = titanic_train)

pred3 =predict(ra1,titanic_test)
head(pred3)
table(pred3)

check3=data.frame(pred3)
write.csv(check3,"titanic3.csv")
###########model4##########
library(MASS)
logreg_o=glm(Survived ~. ,family = 'binomial',data=titanic_train)
summary(logreg_o)

stepAIC(logreg_o)

final_model=glm(formula = Survived ~ Pclass + Sex + Age + SibSp, family = "binomial", 
    data = titanic_train)
summary(final_model)

pred_final =predict(final_model,titanic_test)
head(pred_final)
prediction_sig=ifelse(pred_final <= 0.5,0,1)

table(prediction_sig)

check3=data.frame(prediction_sig)
write.csv(check3,"titanic_final.csv")

library(pROC)
roc(titanic_train$Survived,logreg_o,plot=TRUE)
