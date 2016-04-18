library(foreign)
library(ROCR)
library(e1071)
library(randomForest)
library(party)
library(FSelector)
library(rpart)
library(gbm)
library(ggplot2)
set.seed(1)

model=read.csv("C:/Users/John/Desktop/Project/final_predictors.csv")
final_test=read.csv("C:/Users/John/Desktop/Project/final_test.csv")
model$Relevance=ifelse(model$relevance==3,3,NA)
model$Relevance=ifelse(model$relevance<3,2,model$Relevance)
model$Relevance=ifelse(model$relevance<2,1,model$Relevance)
model$Relevance=factor(model$Relevance)
model$brand_match_ratio=ifelse(is.na(model$brand_match_ratio),
                               median(na.omit(model$brand_match_ratio)),
                               model$brand_match_ratio)
final_test$brand_match_ratio=ifelse(is.na(final_test$brand_match_ratio),
                                    median(na.omit(final_test$brand_match_ratio)),
                                    final_test$brand_match_ratio)

ggplot(model, aes(x=brand_match_ratio)) + geom_histogram(binwidth=.1) + ylab("Count") + xlab("Product Brand Match Ratio")
ggplot(model, aes(x=title_match_ratio)) + geom_histogram(binwidth=.05) + ylab("Count") + xlab("Product Title Match Ratio")
ggplot(model, aes(x=details_match_ratio)) + geom_histogram(binwidth=.01) + ylab("Count") + xlab("Product Details Match Ratio")
ggplot(model, aes(x=relevance)) + geom_histogram(binwidth=.5) + ylab("Count") + xlab("Search Relevance")

ggplot(model, aes(x=brand_match_ratio, fill=Relevance)) + geom_density(alpha=.7) + ylab("Density") + xlab("Product Brand Match Ratio")
ggplot(model, aes(x=title_match_ratio, fill=Relevance)) + geom_density(alpha=.7) + ylab("Density") + xlab("Product Title Match Ratio")
ggplot(model, aes(x=details_match_ratio, fill=Relevance)) + geom_density(alpha=.7) + ylab("Density") + xlab("Product Details Match Ratio")

plot(x=model$brand_match_ratio, y=model$relevance, ylab="Search Relevance", xlab="Product Brand Match Ratio")
lm=glm(data=model, relevance~brand_match_ratio)
co=coef(lm)
abline(lm, col="blue")
plot(x=model$title_match_ratio, y=model$relevance, ylab="Search Relevance", xlab="Product Title Match Ratio")
lm=glm(data=model, relevance~title_match_ratio)
co=coef(lm)
abline(lm, col="blue")
plot(x=model$details_match_ratio, y=model$relevance, ylab="Search Relevance", xlab="Product Details Match Ratio")
lm=glm(data=model, relevance~details_match_ratio)
co=coef(lm)
abline(lm, col="blue")













ggplot(model, aes(x=Relevance, y=brand_match_ratio)) + geom_boxplot()
