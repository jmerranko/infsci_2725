library(foreign)
library(randomForest)
library(party)
library(FSelector)
library(rpart)
library(tree)
library(randomForest)

#Iris dataset is pretty famous and is stored as under default R {datasets}
data(iris)
iris

#decision tree using {rpart} (retains Petal.Length and Petal.Width)
iris.tree=rpart(data=iris,Species~.)

#breakdown of splits with proportion of each outcome at each split
iris.tree

#plot tree
plot(iris.tree, uniform=TRUE, main="Classification Tree")
text(iris.tree, all=TRUE, cex=.75)

#prune tree to reduce overfitting (results in no change)
iris.prune=prune(iris.tree, cp=iris.tree$cptable[which.min(iris.tree$cptable[,"xerror"]),"CP"])
plot(iris.prune, uniform=TRUE, main="Classification Tree")
text(iris.prune, all=TRUE, cex=.75)

#create folds for 10-fold cross-validation
set.seed(1)
iris$uni=runif(nrow(iris))
quantile1=quantile(iris$uni,.1)
quantile2=quantile(iris$uni,.2)
quantile3=quantile(iris$uni,.3)
quantile4=quantile(iris$uni,.4)
quantile5=quantile(iris$uni,.5)
quantile6=quantile(iris$uni,.6)
quantile7=quantile(iris$uni,.7)
quantile8=quantile(iris$uni,.8)
quantile9=quantile(iris$uni,.9)
iris$fold=ifelse(iris$uni<quantile1,1,0)
iris$fold=ifelse(iris$uni>=quantile1 & iris$uni<quantile2,2,iris$fold)
iris$fold=ifelse(iris$uni>=quantile2 & iris$uni<quantile3,3,iris$fold)
iris$fold=ifelse(iris$uni>=quantile3 & iris$uni<quantile4,4,iris$fold)
iris$fold=ifelse(iris$uni>=quantile4 & iris$uni<quantile5,5,iris$fold)
iris$fold=ifelse(iris$uni>=quantile5 & iris$uni<quantile6,6,iris$fold)
iris$fold=ifelse(iris$uni>=quantile6 & iris$uni<quantile7,7,iris$fold)
iris$fold=ifelse(iris$uni>=quantile7 & iris$uni<quantile8,8,iris$fold)
iris$fold=ifelse(iris$uni>=quantile8 & iris$uni<quantile9,9,iris$fold)
iris$fold=ifelse(iris$uni>=quantile9,10,iris$fold)

#compute accuracy by fold and average accuracy using final model above (Species~Petal.Length+Petal.Width)
accuracy.by.fold=sapply(1:10, function(fold){
  train=iris[iris$fold!=fold,]#each iteration will use 9 folds as train set and 1 fold as test set
  test=iris[iris$fold==fold,]
  set.seed(1)
  tree=rpart(data=train,Species~Petal.Length+Petal.Width)#train model
  row=c(1:length(test[,1]))
  prob=data.frame(row,predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
  prob$prediction=sapply(1:length(test[,1]), function(row){#use predicted probabilities to determine predicted outcomes
    if (prob$setosa[row]>.5) {prob$prediction="setosa"}
    else if (prob$versicolor[row]>.5) {prob$prediction="versicolor"}
    else if (prob$virginica[row]>.5) {prob$prediction="virginica"}
  })
  prob$actual=test$Species#add actual observed outcomes to data frame
  prob$correct=ifelse(prob$prediction==prob$actual,1,0)#determine whether predictions match actuals
  return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
})
accuracy.by.fold

#average accuracy across 10 folds
mean(accuracy.by.fold)
median(accuracy.by.fold)

###

#decision tree using {tree} (retains Petal.Length, Petal.Width, and Sepal.Length)
iris.tree=tree(data=iris,Species~.)

#breakdown of splits with proportion of each outcome at each split
iris.tree

#plot tree
plot(iris.tree, main="Classification Tree")
text(iris.tree, cex=.75)

#prune tree to reduce overfitting (both prunings functions yield same model)
iris.prune=prune.tree(iris.tree, k=10)
plot(iris.prune, type="p")
text(iris.prune, cex=.75)
iris.prune=cv.tree(iris.tree, K=10)
plot(iris.prune, type="p") #consult plot to pick choose with low deviance and low size (4)
iris.prune=prune.tree(iris.tree, best=4)
plot(iris.prune, main="Classification Tree")
text(iris.prune, cex=.75)

#create folds for 10-fold cross-validation
set.seed(1)
iris$uni=runif(nrow(iris))
quantile1=quantile(iris$uni,.1)
quantile2=quantile(iris$uni,.2)
quantile3=quantile(iris$uni,.3)
quantile4=quantile(iris$uni,.4)
quantile5=quantile(iris$uni,.5)
quantile6=quantile(iris$uni,.6)
quantile7=quantile(iris$uni,.7)
quantile8=quantile(iris$uni,.8)
quantile9=quantile(iris$uni,.9)
iris$fold=ifelse(iris$uni<quantile1,1,0)
iris$fold=ifelse(iris$uni>=quantile1 & iris$uni<quantile2,2,iris$fold)
iris$fold=ifelse(iris$uni>=quantile2 & iris$uni<quantile3,3,iris$fold)
iris$fold=ifelse(iris$uni>=quantile3 & iris$uni<quantile4,4,iris$fold)
iris$fold=ifelse(iris$uni>=quantile4 & iris$uni<quantile5,5,iris$fold)
iris$fold=ifelse(iris$uni>=quantile5 & iris$uni<quantile6,6,iris$fold)
iris$fold=ifelse(iris$uni>=quantile6 & iris$uni<quantile7,7,iris$fold)
iris$fold=ifelse(iris$uni>=quantile7 & iris$uni<quantile8,8,iris$fold)
iris$fold=ifelse(iris$uni>=quantile8 & iris$uni<quantile9,9,iris$fold)
iris$fold=ifelse(iris$uni>=quantile9,10,iris$fold)

#compute accuracy by fold and average accuracy using final model above (Species~Petal.Length+Petal.Width+Sepal.Length)
accuracy.by.fold=sapply(1:10, function(fold){
  train=iris[iris$fold!=fold,]#each iteration will use 9 folds as train set and 1 fold as test set
  test=iris[iris$fold==fold,]
  set.seed(1)
  tree=tree(data=train, Species~Petal.Length+Petal.Width+Sepal.Length)#train model
  tree=prune.tree(tree, k=10)
  row=c(1:length(test[,1]))
  prob=data.frame(row,predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
  prob$prediction=sapply(1:length(test[,1]), function(row){#use predicted probabilities to determine predicted outcomes
    if (prob$setosa[row]>.5) {prob$prediction="setosa"}
    else if (prob$versicolor[row]>.5) {prob$prediction="versicolor"}
    else if (prob$virginica[row]>.5) {prob$prediction="virginica"}
  })
  prob$actual=test$Species#add actual observed outcomes to data frame
  prob$correct=ifelse(prob$prediction==prob$actual,1,0)#determine whether predictions match actuals
  return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
})
accuracy.by.fold

#average accuracy across 10 folds
mean(accuracy.by.fold)
median(accuracy.by.fold)

###Random Forest
accuracy.by.fold=sapply(1:10, function(fold){
  train=iris[iris$fold!=fold,]#each iteration will use 9 folds as train set and 1 fold as test set
  test=iris[iris$fold==fold,]
  set.seed(1)
  tree=randomForest(data=train, Species~Petal.Length+Petal.Width, ntree=1000)#train model
  prob=data.frame(predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
  prob$actual=test$Species#add actual observed outcomes to data frame
  prob$correct=ifelse(prob$predict.tree..newdata...test.==prob$actual,1,0)#determine whether predictions match actuals
  return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
})
accuracy.by.fold
mean(accuracy.by.fold)
median(accuracy.by.fold)

##########################################################################################################

#house votes data
house_votes=read.delim("C:/Users/merrankoja/Desktop/Data Analytics/HW7/house_votes.txt")
#house_votes=read.delim("C:/Users/merrankoja/Desktop/Data Analytics/HW7/house_votes_no_w.txt")

#decision tree using {rpart} (retains only physician_fee_freeze)
house_votes.tree=rpart(data=house_votes,Party~.)

#breakdown of splits with proportion of each outcome at each split
house_votes.tree

#plot tree
plot(house_votes.tree, main="Classification Tree")
text(house_votes.tree, all=TRUE, cex=.75)

#create folds for 10-fold cross-validation
set.seed(1)
house_votes$uni=runif(nrow(house_votes))
quantile1=quantile(house_votes$uni,.1)
quantile2=quantile(house_votes$uni,.2)
quantile3=quantile(house_votes$uni,.3)
quantile4=quantile(house_votes$uni,.4)
quantile5=quantile(house_votes$uni,.5)
quantile6=quantile(house_votes$uni,.6)
quantile7=quantile(house_votes$uni,.7)
quantile8=quantile(house_votes$uni,.8)
quantile9=quantile(house_votes$uni,.9)
house_votes$fold=ifelse(house_votes$uni<quantile1,1,0)
house_votes$fold=ifelse(house_votes$uni>=quantile1 & house_votes$uni<quantile2,2,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile2 & house_votes$uni<quantile3,3,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile3 & house_votes$uni<quantile4,4,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile4 & house_votes$uni<quantile5,5,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile5 & house_votes$uni<quantile6,6,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile6 & house_votes$uni<quantile7,7,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile7 & house_votes$uni<quantile8,8,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile8 & house_votes$uni<quantile9,9,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile9,10,house_votes$fold)

#compute accuracy by fold and average accuracy using final model above (Party~physician_fee_freeze)
accuracy.by.fold=sapply(1:10, function(fold){
  train=house_votes[house_votes$fold!=fold,]#each iteration will use 9 folds as train set and 1 fold as test set
  test=house_votes[house_votes$fold==fold,]
  set.seed(1)
  tree=rpart(data=train,Party~physician_fee_freeze)#train model
  row=c(1:length(test[,1]))
  prob=data.frame(row,predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
  prob$prediction=sapply(1:length(test[,1]), function(row){#use predicted probabilities to determine predicted outcomes
    if (prob$democrat[row]>.5) {prob$prediction="democrat"}
    else if (prob$republican[row]>.5) {prob$republican="republican"}
  })
  prob$actual=test$Party#add actual observed outcomes to data frame
  prob$correct=ifelse(prob$prediction==prob$actual,1,0)#determine whether predictions match actuals
  return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
})
accuracy.by.fold

#average accuracy across 10 folds
mean(accuracy.by.fold)
median(accuracy.by.fold)

###

#decision tree using {tree} (retains )
house_votes.tree=tree(data=house_votes,Party~.)

#breakdown of splits with proportion of each outcome at each split
house_votes.tree

#plot tree
plot(house_votes.tree, main="Classification Tree")
text(house_votes.tree, all=TRUE, cex=.75)

#prune tree to reduce overfitting (two methods yield different prunings)
house_votes.prune=prune.tree(house_votes.tree, k=10)#Party~adoption_of_the_budget_resolution+synfuels_corporation_cutback
plot(house_votes.prune, type="p")
text(house_votes.prune, cex=.75)
house_votes.prune=cv.tree(house_votes.tree, K=10)
plot(house_votes.prune, type="p")
house_votes.prune=prune.tree(house_votes.tree, best=3)#Party~physician_fee_freeze+synfuels_corporation_cutback
plot(house_votes.prune, main="Classification Tree")
text(house_votes.prune, cex=.75)

#create folds for 10-fold cross-validation
set.seed(1)
house_votes$uni=runif(nrow(house_votes))
quantile1=quantile(house_votes$uni,.1)
quantile2=quantile(house_votes$uni,.2)
quantile3=quantile(house_votes$uni,.3)
quantile4=quantile(house_votes$uni,.4)
quantile5=quantile(house_votes$uni,.5)
quantile6=quantile(house_votes$uni,.6)
quantile7=quantile(house_votes$uni,.7)
quantile8=quantile(house_votes$uni,.8)
quantile9=quantile(house_votes$uni,.9)
house_votes$fold=ifelse(house_votes$uni<quantile1,1,0)
house_votes$fold=ifelse(house_votes$uni>=quantile1 & house_votes$uni<quantile2,2,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile2 & house_votes$uni<quantile3,3,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile3 & house_votes$uni<quantile4,4,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile4 & house_votes$uni<quantile5,5,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile5 & house_votes$uni<quantile6,6,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile6 & house_votes$uni<quantile7,7,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile7 & house_votes$uni<quantile8,8,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile8 & house_votes$uni<quantile9,9,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile9,10,house_votes$fold)

#compute accuracy by fold and average accuracy using different combos from above with and without pruning
accuracy.by.fold=sapply(1:10, function(fold){
  train=house_votes[house_votes$fold!=fold,]#each iteration will use 9 folds as train set and 1 fold as test set
  test=house_votes[house_votes$fold==fold,]
  set.seed(1)
  tree=tree(data=train,Party~physician_fee_freeze+adoption_of_the_budget_resolution+synfuels_corporation_cutback)#train model
  #tree=prune.tree(tree, best=3)
  row=c(1:length(test[,1]))
  prob=data.frame(row,predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
  prob$prediction=sapply(1:length(test[,1]), function(row){#use predicted probabilities to determine predicted outcomes
    if (prob$democrat[row]>.5) {prob$prediction="democrat"}
    else if (prob$republican[row]>.5) {prob$republican="republican"}
  })
  prob$actual=test$Party#add actual observed outcomes to data frame
  prob$correct=ifelse(prob$prediction==prob$actual,1,0)#determine whether predictions match actuals
  return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
})
accuracy.by.fold

#average accuracy across 10 folds
mean(accuracy.by.fold)
median(accuracy.by.fold)

###Random Forest

#create folds for 10-fold cross-validation
set.seed(1)
iris$uni=runif(nrow(iris))
quantile1=quantile(iris$uni,.1)
quantile2=quantile(iris$uni,.2)
quantile3=quantile(iris$uni,.3)
quantile4=quantile(iris$uni,.4)
quantile5=quantile(iris$uni,.5)
quantile6=quantile(iris$uni,.6)
quantile7=quantile(iris$uni,.7)
quantile8=quantile(iris$uni,.8)
quantile9=quantile(iris$uni,.9)
iris$fold=ifelse(iris$uni<quantile1,1,0)
iris$fold=ifelse(iris$uni>=quantile1 & iris$uni<quantile2,2,iris$fold)
iris$fold=ifelse(iris$uni>=quantile2 & iris$uni<quantile3,3,iris$fold)
iris$fold=ifelse(iris$uni>=quantile3 & iris$uni<quantile4,4,iris$fold)
iris$fold=ifelse(iris$uni>=quantile4 & iris$uni<quantile5,5,iris$fold)
iris$fold=ifelse(iris$uni>=quantile5 & iris$uni<quantile6,6,iris$fold)
iris$fold=ifelse(iris$uni>=quantile6 & iris$uni<quantile7,7,iris$fold)
iris$fold=ifelse(iris$uni>=quantile7 & iris$uni<quantile8,8,iris$fold)
iris$fold=ifelse(iris$uni>=quantile8 & iris$uni<quantile9,9,iris$fold)
iris$fold=ifelse(iris$uni>=quantile9,10,iris$fold)

accuracy.by.fold=sapply(1:10, function(fold){
  train=house_votes[house_votes$fold!=fold,]#each iteration will use 9 folds as train set and 1 fold as test set
  test=house_votes[house_votes$fold==fold,]
  set.seed(1)
  tree=randomForest(data=train, Party~., ntree=1000)#train model
  prob=data.frame(predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
  prob$actual=test$Party#add actual observed outcomes to data frame
  prob$correct=ifelse(prob$predict.tree..newdata...test.==prob$actual,1,0)#determine whether predictions match actuals
  return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
})
accuracy.by.fold
mean(accuracy.by.fold)
median(accuracy.by.fold)

###Random Forest with Feature Selection Algorithm

rf=function(subset){
  rf.auc=(sapply(1:10, function(fold){
    set.seed(66)
    train=house_votes[house_votes$fold!=fold,]
    test=house_votes[house_votes$fold==fold,]
    train$fold=NULL
    test$fold=NULL
    tree=randomForest(data=train, ntree=1000, as.simple.formula(subset, class="Party"))
    prob=data.frame(predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
    prob$actual=test$Party#add actual observed outcomes to data frame
    prob$correct=ifelse(prob$predict.tree..newdata...test.==prob$actual,1,0)#determine whether predictions match actuals
    return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
  }))
  print(subset)
  print(mean(rf.auc))
  return(mean(rf.auc))
}

subset=hill.climbing.search(names(house_votes)[2:16], rf)
final=as.simple.formula(subset, "Party") 
#If you scroll through the iteration log, you will find a smaller subset that achieves the same accuracy.
#I chose this subset since the smaller the model, the better.

rf.accuracy=(sapply(1:10, function(fold){
  set.seed(66)
  train=house_votes[house_votes$fold!=fold,]
  test=house_votes[house_votes$fold==fold,]
  train$fold=NULL
  test$fold=NULL
  tree=randomForest(data=train, ntree=1000, final)
  prob=data.frame(predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
  prob$actual=test$Party#add actual observed outcomes to data frame
  prob$correct=ifelse(prob$predict.tree..newdata...test.==prob$actual,1,0)#determine whether predictions match actuals
  return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
}))
rf.accuracy
mean(rf.accuracy)
median(rf.accuracy)

###Random forest without votes of W

house_votes=na.omit(read.delim("C:/Users/merrankoja/Desktop/Data Analytics/HW7/house_votes_no_w.txt"))

set.seed(1)
house_votes$uni=runif(nrow(house_votes))
quantile1=quantile(house_votes$uni,.1)
quantile2=quantile(house_votes$uni,.2)
quantile3=quantile(house_votes$uni,.3)
quantile4=quantile(house_votes$uni,.4)
quantile5=quantile(house_votes$uni,.5)
quantile6=quantile(house_votes$uni,.6)
quantile7=quantile(house_votes$uni,.7)
quantile8=quantile(house_votes$uni,.8)
quantile9=quantile(house_votes$uni,.9)
house_votes$fold=ifelse(house_votes$uni<quantile1,1,0)
house_votes$fold=ifelse(house_votes$uni>=quantile1 & house_votes$uni<quantile2,2,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile2 & house_votes$uni<quantile3,3,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile3 & house_votes$uni<quantile4,4,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile4 & house_votes$uni<quantile5,5,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile5 & house_votes$uni<quantile6,6,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile6 & house_votes$uni<quantile7,7,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile7 & house_votes$uni<quantile8,8,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile8 & house_votes$uni<quantile9,9,house_votes$fold)
house_votes$fold=ifelse(house_votes$uni>=quantile9,10,house_votes$fold)

###Random Forest

accuracy.by.fold=sapply(1:10, function(fold){
  train=house_votes[house_votes$fold!=fold,]#each iteration will use 9 folds as train set and 1 fold as test set
  test=house_votes[house_votes$fold==fold,]
  set.seed(1)
  tree=randomForest(data=train, Party~., ntree=1000)#train model
  prob=data.frame(predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
  prob$actual=test$Party#add actual observed outcomes to data frame
  prob$correct=ifelse(prob$predict.tree..newdata...test.==prob$actual,1,0)#determine whether predictions match actuals
  return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
})
accuracy.by.fold
mean(accuracy.by.fold)
median(accuracy.by.fold)

###Random Forest with Feature Selection Algorithm
rf=function(subset){
  rf.auc=(sapply(1:10, function(fold){
    set.seed(66)
    train=house_votes[house_votes$fold!=fold,]
    test=house_votes[house_votes$fold==fold,]
    train$fold=NULL
    test$fold=NULL
    tree=randomForest(data=train, ntree=1000, as.simple.formula(subset, class="Party"))
    prob=data.frame(predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
    prob$actual=test$Party#add actual observed outcomes to data frame
    prob$correct=ifelse(prob$predict.tree..newdata...test.==prob$actual,1,0)#determine whether predictions match actuals
    return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
  }))
  print(subset)
  print(mean(rf.auc))
  return(mean(rf.auc))
}

subset=hill.climbing.search(names(house_votes)[2:16], rf)
final=as.simple.formula(subset, "Party")

rf.accuracy=(sapply(1:10, function(fold){
  set.seed(66)
  train=house_votes[house_votes$fold!=fold,]
  test=house_votes[house_votes$fold==fold,]
  train$fold=NULL
  test$fold=NULL
  tree=randomForest(data=train, ntree=1000, final)
  prob=data.frame(predict(tree, newdata=test))#test model and store predicted probabilities for each outcome
  prob$actual=test$Party#add actual observed outcomes to data frame
  prob$correct=ifelse(prob$predict.tree..newdata...test.==prob$actual,1,0)#determine whether predictions match actuals
  return(mean(prob$correct))#compute accuracy=(# of correct classifications)/(total # of test cases)
}))
rf.accuracy
mean(rf.accuracy)
median(rf.accuracy)

