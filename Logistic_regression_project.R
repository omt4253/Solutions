setwd()
df_bank=read.csv("BankCreditCard.csv",header=T,stringsAsFactors = F)
dim(df_bank)
str(df_bank)

names(df_bank)

####convert numeric to factor
cols_to_factor=colnames(df_bank)[c(3,4,5,7:12,25)]
cols_to_factor

df_bank[cols_to_factor]=lapply(df_bank[cols_to_factor],factor)
str(df_bank)

####checking for NA
colSums(is.na(df_bank))

###drop custmr ID
df_bank[1]= NULL
str(df_bank)
####split data into train and test data
library(caret)

set.seed(2019)
train_indices=createDataPartition(df_bank$Default_Payment,p=0.7,list=F)
train=df_bank[train_indices,]
test=df_bank[-train_indices,]

dim(train)
dim(test)

prop.table(table(df_bank$Default_Payment))
prop.table(table(train$Default_Payment))
prop.table(table(test$Default_Payment))


logreg =glm(Default_Payment ~.,family='binomial',data=train)
summary(logreg)


###Evaluation
predProbs=predict(logreg,type='response')
trainPreds=ifelse(predProbs <= 0.5,0,1)

testPreds=ifelse(predict(logreg,test,type='response')<=0.5,0,1)


table(test$Default_Payment)
table(testPreds)

CF_train=confusionMatrix(factor(trainPreds),train$Default_Payment
                         ,positive = '1')

CF_train

CF_test=confusionMatrix(factor(testPreds),test$Default_Payment
                         ,positive = '1')

CF_test

install.packages("pROC")
library(pROC)
roc(train$Default_Payment,trainPreds,plot=T)


##########
summary(logreg)
logreg_sig=glm(Default_Payment ~ Credit_Amount + Gender +Marital +Repayment_Status_Jan
               +Repayment_Status_Feb + Repayment_Status_March
               +Jan_Bill_Amount+Feb_Bill_Amount+March_Bill_Amount
               +Previous_Payment_Jan+Previous_Payment_Feb+Previous_Payment_April
               +Previous_Payment_May,family='binomial',data=train)

summary(logreg_sig)
`

calc1<-function(a,b){
  if(a>b){
    bigger =a
    smaller = b
  }
  else {
    bigger =b
    smaller =a
    }
  
  print(bigger**smaller)
}
  
  
calc1(12,3)
calc1(3,12)


