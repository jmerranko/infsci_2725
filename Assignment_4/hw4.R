library(ggplot2)
library(reshape2)
library(plyr)
library(scales)

train=read.csv("train.csv")

#coercing factors (this can actually be done within the plot commands too)
train$class=factor(train$Pclass)
train$survived=factor(train$Survived)

#create 6-level factor variable for all combinations of class and sex
train$class_sex=ifelse(train$Pclass==1 & train$Sex=="male","1st Class Male",0)
train$class_sex=ifelse(train$Pclass==2 & train$Sex=="male","2nd Class Male",train$class_sex)
train$class_sex=ifelse(train$Pclass==3 & train$Sex=="male","3rd Class Male",train$class_sex)
train$class_sex=ifelse(train$Pclass==1 & train$Sex=="female","1st Class Female",train$class_sex)
train$class_sex=ifelse(train$Pclass==2 & train$Sex=="female","2nd Class Female",train$class_sex)
train$class_sex=ifelse(train$Pclass==3 & train$Sex=="female","3rd Class Female",train$class_sex)

#boxplot of fare by class and gender
ggplot(train, aes(x=class_sex, y=Fare, fill=class_sex)) + geom_boxplot() +
  coord_cartesian(ylim = c(0, 300))

#histogram of fare by survival y/n
ggplot(train, aes(Fare, fill=survived)) + geom_histogram()

#facet grid of age, fare, survival, class, and gender
ggplot(train, aes(Fare,Age)) + geom_point() + facet_grid(class_sex~survived)

#logistic regression to test interactions
summary(glm(data=train,Survived~Sex+factor(Pclass)+factor(Pclass)*Sex,family="binomial"))
summary(glm(data=train,Survived~Age+Sex+Age*Sex,family="binomial"))
summary(glm(data=train,Survived~Age+factor(Pclass)+Age*factor(Pclass),family="binomial"))

#violin plot of fare by class
ggplot(train, aes(class, Fare))+geom_violin()

#variances of fare by class
var(train$Fare[train$Pclass==1])
var(train$Fare[train$Pclass==2])
var(train$Fare[train$Pclass==3])

#heatmap datasteps
heatm=train #make heatmap matrix of train that will change to only numeric values 
heatm$Age[is.na(heatm$Age)]=median(heatm$Age, na.rm=TRUE) #change age NAs to the median age value
heatm$Sex=as.numeric(heatm$Sex) #make numeric 
#heatm$Embarked=as.numeric(heatm$Embarked) #make numeric
mydata=heatm[, c(2,3,5,6,7,8,10)] #pick the columns of importance
#make a correlation matrix, use spearman because of ordinal values/mostly non-normal distribution
cmat=cor(mydata,method="spearman") 

melt_cmat=melt(cmat) #get single column of values for variables

#plot heatmap
ggplot(data=melt_cmat, aes(x=Var1, y=Var2, fill=value)) + geom_tile(color = "white")+
  scale_fill_gradient2(low = "dark green", high = "dark blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation")

