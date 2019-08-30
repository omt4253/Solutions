setwd("F:/Imarticus")
cols=c('Type','Alcohol','Malic','Ash','AlkalinityofAsh', 
       'Magnesium','Phenols','Flavonoids','Nonflavanoid','Proanthocyanins', 
       'Color','Hue','Dilution','Proline') 
       

wine=read.csv("wine.data.txt",col.names = cols,header = F)
head(wine)

str(wine)
wine$Type=as.factor(wine$Type)

library(ggplot2)
ggplot(aes(x=Type),data=wine) +
  geom_bar(fill='steelblue',colour='darkgrey',alpha=0.8)

install.packages("ggpubr")
library(ggpubr)
wine_attribute=function(attribute,varname='',bins=30){
  histogram=ggplot(data=wine)+
    geom_histogram(aes(x=attribute),bins=bins,
                   fill='steelblue',colour='darkgrey',alpha=0.8)+
    labs(x=varname)

  histYlog=histogram +scale_y_log10()+
    labs(y='log10(count)',x=varname)
  
  histxlog=histogram +scale_x_log10()+
    labs(x=paste('log10(',varname,')'))
  
  ##boxplot
  outliers=ggplot(wine,aes(x=1,y=attribute))+
    geom_jitter(alpha=0.1)+
    geom_boxplot(alpha=0.2,color='red')+
    labs(x='distance from mean',y=varname)
  
  
  ##arrange
  histplots=ggarrange(histogram,histxlog,histYlog,ncol=1,nrow = 3)
  ggarrange(outliers,histplots,ncol=2,widths = c(1,1.5))
}

#alcohol
options(repr.plot.height=3,repr.plot.width=7.5)
wine_attribute(wine$Alcohol,varname = 'Alcohol(% of vol)')
summary(wine$Alcohol)

##MAlic acid
wine_attribute(wine$Malic,varname = 'Malic(mg/L)')

##Ash
wine_attribute(wine$Ash,varname = 'Ash')

##alkalinity
wine_attribute(wine$AlkalinityofAsh,varname = 'AlkalinityofAsh')

##bivariate analysis
?ggcorr
install.packages("GGally")
library(GGally)

options(repr.plot.height=4,repr.plot.width=6)
ggcorr(wine[,1:14],geom='blank',label = T,
       hjust=0.9,layout.exp = 2)+
  geom_point(size=8,aes(color=coefficient >0,
                        alpha=abs(coefficient)>0.35)) +
  scale_alpha_manual(values=c('TRUE'=0.25,'FALSE'=0)) +
  guides(color=F,alpha=F)
##above fn needs integer inputs

wine$Type=as.integer(wine$Type)

variables=c('Type','Malic','AlkalinityofAsh','Phenols','Flavonoids',
            'Nonflavanoid','Proanthocyanins', 
            'Hue','Dilution','Proline') 
ggpairs(wine[variables],aes(alpha=0.3))




options(repr.plot.height=4,repr.plot.width=6)
ggplot(aes(x=factor(Type),y=Flavonoids),data=wine)+
  geom_jitter(alpha=0.3)+
  geom_boxplot(alpha=0.5,color='blue')+
  stat_summary(fun.y = 'mean',geom='point',color='darkblue',
               shape=4,size=4)+
  labs(x='Type',y='Flavonoids',title='Flavonoids vs Type')


####model
library(caret)
wine$Type=as.factor(wine$Type)
train_indices=createDataPartition(wine$Type,p=0.75,list=F)
train_indices[1:10]

train=wine[train_indices,]
test=wine[-train_indices,]

dim(wine)
dim(train)
dim(test)

install.packages("memisc")
library(memisc)

lda_model=lda(Type~.,train)
summary(lda_model)
lda_model

plot(lda_model,col=as.integer(wine$Type))

test_preds=predict(lda_model,test)
confusionMatrix(test$Type,test_preds)
