#######HousePrice Prediction with linear regression#########

setwd("F:/Imarticus/Practice/HousePrice")

train=read.csv("train.csv")
test=read.csv("test.csv")

str(train)

train$istrain =1
test$istrain =0
test$SalePrice=0

all=rbind(train,test)
str(all)


########PreProcessing Data#############

all$MSSubClass=as.factor(all$MSSubClass)

colSums(is.na(all))

all$LotFrontage=median(all$LotFrontage,na.rm = T)

all$Alley=as.character(all$Alley)
all$Alley[is.na(all$Alley)]='No_Alley'
all$Alley=as.factor(all$Alley)

levels(all$BsmtQual)
all$BsmtQual=as.character(all$BsmtQual)
all$BsmtQual[is.na(all$BsmtQual)]='No_Basement'
all$BsmtQual=as.factor(all$BsmtQual)


levels(all$BsmtCond)
all$BsmtCond=as.character(all$BsmtCond)
all$BsmtCond[is.na(all$BsmtCond)]='No_Basement'
all$BsmtCond=as.factor(all$BsmtCond)

levels(all$BsmtExposure)
all$BsmtExposure=as.character(all$BsmtExposure)
all$BsmtExposure[is.na(all$BsmtExposure)]='No_Basement'
all$BsmtExposure=as.factor(all$BsmtExposure)

levels(all$BsmtFinType1)
all$BsmtFinType1=as.character(all$BsmtFinType1)
all$BsmtFinType1[is.na(all$BsmtFinType1)]='No_Basement'
all$BsmtFinType1=as.factor(all$BsmtFinType1)

levels(all$BsmtFinType2)
all$BsmtFinType2=as.character(all$BsmtFinType2)
all$BsmtFinType2[is.na(all$BsmtFinType2)]='No_Basement'
all$BsmtFinType2=as.factor(all$BsmtFinType2)

levels(all$FireplaceQu)
all$FireplaceQu=as.character(all$FireplaceQu)
all$FireplaceQu[is.na(all$FireplaceQu)]='No_Fireplace'
all$FireplaceQu=as.factor(all$FireplaceQu)

levels(all$GarageType)
all$GarageType=as.character(all$GarageType)
all$GarageType[is.na(all$GarageType)]='No_Garage'
all$GarageType=as.factor(all$GarageType)

levels(all$GarageFinish)
all$GarageFinish=as.character(all$GarageFinish)
all$GarageFinish[is.na(all$GarageFinish)]='No_Garage'
all$GarageFinish=as.factor(all$GarageFinish)

levels(all$GarageQual)
all$GarageQual=as.character(all$GarageQual)
all$GarageQual[is.na(all$GarageQual)]='No_Garage'
all$GarageQual=as.factor(all$GarageQual)


levels(all$GarageCond)
all$GarageCond=as.character(all$GarageCond)
all$GarageCond[is.na(all$GarageCond)]='No_Garage'
all$GarageCond=as.factor(all$GarageCond)

levels(all$PoolQC)
all$PoolQC=as.character(all$PoolQC)
all$PoolQC[is.na(all$PoolQC)]='No_Pool'
all$PoolQC=as.factor(all$PoolQC)

levels(all$Fence)
all$Fence=as.character(all$Fence)
all$Fence[is.na(all$Fence)]='No_Fence'
all$Fence=as.factor(all$Fence)

levels(all$MiscFeature)
all$MiscFeature=as.character(all$MiscFeature)
all$MiscFeature[is.na(all$MiscFeature)]='None'
all$MiscFeature=as.factor(all$MiscFeature)

colSums(is.na(all))

Na_cols=sapply(all, function(x) sum(is.na(x)))
missingCols=Na_cols[Na_cols>0]

missingCols
###################
library(VIM)
all_imp=kNN(all,k=5,variable=names(missingCols),
            numFun='median',catFun = 'maxCat',imp_var = F)
###################
all$GarageYrBlt[is.na(all$GarageYrBlt)]=median(all$GarageYrBlt,na.rm= T)
all$MasVnrArea[is.na(all$MasVnrArea)]=median(all$MasVnrArea,na.rm= T)

levels(all$MasVnrType)
all$MasVnrType=as.character(all$MasVnrType)
all$MasVnrType[is.na(all$MasVnrType)]='None'
all$MasVnrType=as.factor(all$MasVnrType)

levels(all$MSZoning)
all$MSZoning=as.character(all$MSZoning)
all$MSZoning[is.na(all$MSZoning)]='None'
all$MSZoning=as.factor(all$MSZoning)

levels(all$Utilities)
all$Utilities=as.character(all$Utilities)
all$Utilities[is.na(all$Utilities)]='None'
all$Utilities=as.factor(all$Utilities)

levels(all$Exterior1st)
all$Exterior1st=as.character(all$Utilities)
all$Exterior1st[is.na(all$Exterior1st)]='None'
all$Exterior1st=as.factor(all$Exterior1st)

levels(all$Exterior2nd)
all$Exterior2nd=as.character(all$Exterior2nd)
all$Exterior2nd[is.na(all$Exterior2nd)]='None'
all$Exterior2nd=as.factor(all$Exterior2nd)

all$BsmtFinSF1[is.na(all$BsmtFinSF1)]=median(all$BsmtFinSF1,na.rm= T)
all$BsmtFinSF2[is.na(all$BsmtFinSF2)]=median(all$BsmtFinSF2,na.rm= T)

all$BsmtUnfSF[is.na(all$BsmtUnfSF)]=median(all$BsmtUnfSF,na.rm= T)

all$TotalBsmtSF[is.na(all$TotalBsmtSF)]=median(all$TotalBsmtSF,na.rm= T)

levels(all$Electrical)
all$Electrical=as.character(all$Electrical)
all$Electrical[is.na(all$Electrical)]='None'
all$Electrical=as.factor(all$Electrical)

all$BsmtFullBath [is.na(all$BsmtFullBath )]=median(all$BsmtFullBath,na.rm= T)
all$BsmtHalfBath [is.na(all$BsmtHalfBath )]=median(all$BsmtHalfBath,na.rm= T)

levels(all$KitchenQual)
all$KitchenQual=as.character(all$KitchenQual)
all$KitchenQual[is.na(all$KitchenQual)]='None'
all$KitchenQual=as.factor(all$KitchenQual)

levels(all$Functional)
all$Functional=as.character(all$Functional)
all$Functional[is.na(all$Functional)]='None'
all$Functional=as.factor(all$Functional)

all$GarageCars [is.na(all$GarageCars )]=median(all$GarageCars,na.rm= T)

all$GarageArea [is.na(all$GarageArea )]=median(all$GarageArea,na.rm= T)

levels(all$SaleType)
all$SaleType=as.character(all$SaleType)
all$SaleType[is.na(all$SaleType)]='None'
all$SaleType=as.factor(all$SaleType)

########
numVars=all[sapply(all,is.numeric)]
numNames=names(numVars)
length(numNames)

library(VIM)
cormat=cor(numVars,use = 'complete.obs')
heatmap(cormat)
cormat1=cor(numVars[-38])
heatmap(cormat1)

library(caret)
highcorr=findCorrelation(cormat1,cutoff = 0.6,names =T)
hcmat=cor(numVars[highcorr])
view(hcmat)
###########Splitting data into train and test##########
final_train=all[which (all$istrain==1),]
final_test =all[which(all$istrain==0),]

final_train$istrain=NULL

final_test$istrain=NULL
final_test$SalePrice=NULL



numVars=final_train[sapply(final_train,is.numeric)]
numNames=names(numVars)
length(numNames)

###############HeatMap################
library(VIM)
cormat=cor(numVars,use = 'complete.obs')
heatmap(cormat)
cormat1=cor(numVars[-37])
heatmap(cormat1)

library(caret)
highcorr=findCorrelation(cormat1,cutoff = 0.6,names =T)
hcmat=cor(numVars[highcorr])
view(hcmat)



###################Buliding model and evaluation##############
reg=lm(SalePrice ~.,data=final_train)
summary(reg)

trainpred=predict(reg)
testpred=predict(reg,final_test)
  
install.packages("Metrics")
library(Metrics)

levels(final_test$MSSubClass)
levels(final_train$MSSubClass)
levels(all$MSSubClass)

rmse(final_train$SalePrice,trainpred)

library(MASS)
stepAIC(reg)

optimal_model=lm(formula = SalePrice ~ MSZoning + LotArea + Street + LandContour + 
                   Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + 
                   Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + 
                   YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + MasVnrType + 
                   MasVnrArea + ExterQual + BsmtQual + BsmtCond + BsmtExposure + 
                   BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
                   X2ndFlrSF + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
                   Functional + Fireplaces + GarageCars + GarageArea + GarageQual + 
                   GarageCond + WoodDeckSF + ScreenPorch + PoolArea + PoolQC + 
                   Fence + MoSold + SaleCondition, data = final_train)


install.packages('vegan')
library(vegan)
numVars_std=decostand(numVars,'standardize')
  