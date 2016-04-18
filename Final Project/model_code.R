library(foreign)
library(ROCR)
library(e1071)
library(randomForest)
library(party)
library(FSelector)
library(rpart)
library(gbm)
set.seed(1)

model=read.csv("C:/Users/John/Desktop/Project/final_predictors_no_numbers.csv")
final_test=read.csv("C:/Users/John/Desktop/Project/final_test_no_numbers.csv")

model$brand_match_ratio=ifelse(is.na(model$brand_match_ratio),
                               median(na.omit(model$brand_match_ratio)),
                               model$brand_match_ratio)
final_test$brand_match_ratio=ifelse(is.na(final_test$brand_match_ratio),
                                    median(na.omit(final_test$brand_match_ratio)),
                                    final_test$brand_match_ratio)

model$brand_match_yn=ifelse(is.na(model$brand_match_ratio), 0, model$brand_match_ratio)
model$brand_match_yn=ifelse(model$brand_match_ratio>0,1,0)
final_test$brand_match_yn=ifelse(is.na(final_test$brand_match_ratio), 0, final_test$brand_match_ratio)
final_test$brand_match_yn=ifelse(final_test$brand_match_ratio>0,1,0)

summary(lm(data=model, relevance~brand_match_ratio))
#summary(lm(data=model, relevance~brand_match_yn))
summary(lm(data=model, relevance~title_match_ratio))
summary(lm(data=model, relevance~details_match_ratio))
summary(lm(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio))

tree=rpart(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio)
plot(tree, uniform=TRUE)
text(tree, all=TRUE, cex=.75)

rf1=randomForest(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, ntree=500, mtry=1)
rf2=randomForest(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, ntree=1000, mtry=1)
svm1cv=svm(seed=1, data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, kernel="linear", cross=2)
svm1cv$tot.MSE
svm2cv=svm(seed=1, data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, kernel="polynomial", order=2, cross=2)
svm2cv$tot.MSE
svm3cv=svm(seed=1, data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, kernel="polynomial", order=3, cross=2)
svm3cv$tot.MSE
svm4cv=svm(seed=1, data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, kernel="radial", cross=2)
svm4cv$tot.MSE
gbm1cv=gbm(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, distribution="gaussian", n.trees=500, cv.folds=2)
mean(gbm1cv$train.error)
gbm2cv=gbm(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, distribution="gaussian", n.trees=1000, cv.folds=2)
mean(gbm2cv$train.error)

#rf1=randomForest(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, ntree=500, mtry=3)
#rf2=randomForest(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, ntree=1000, mtry=3)
svm1=svm(seed=1, data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, kernel="linear")
svm2=svm(seed=1, data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, kernel="polynomial", order=2)
svm3=svm(seed=1, data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, kernel="polynomial", order=3)
svm4=svm(seed=1, data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, kernel="radial")
gbm1=gbm(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, distribution="gaussian", n.trees=500)
gbm2=gbm(data=model, relevance~details_match_ratio+brand_match_ratio+title_match_ratio, distribution="gaussian", n.trees=1000)

rf1_predict=predict(rf1, newdata=final_test)
rf2_predict=predict(rf2, newdata=final_test)
svm1_predict=predict(svm1, newdata=final_test)
svm2_predict=predict(svm2, newdata=final_test)
svm3_predict=predict(svm3, newdata=final_test)
svm4_predict=predict(svm4, newdata=final_test)
gbm1_predict=predict(gbm1, newdata=final_test, n.trees=500)
gbm2_predict=predict(gbm2, newdata=final_test, n.trees=1000)

rf1_predict_out=data.frame(id=final_test$id, relevance=rf1_predict)
rf2_predict_out=data.frame(id=final_test$id, relevance=rf2_predict)
svm1_predict_out=data.frame(id=final_test$id, relevance=svm1_predict)
svm2_predict_out=data.frame(id=final_test$id, relevance=svm2_predict)
svm3_predict_out=data.frame(id=final_test$id, relevance=svm3_predict)
svm4_predict_out=data.frame(id=final_test$id, relevance=svm4_predict)
gbm1_predict_out=data.frame(id=final_test$id, relevance=gbm1_predict)
gbm2_predict_out=data.frame(id=final_test$id, relevance=gbm2_predict)

write.csv(rf1_predict_out, "C:/Users/John/Desktop/Project/Predictions/rf1_predict_out_nn.csv")
write.csv(rf2_predict_out, "C:/Users/John/Desktop/Project/Predictions/rf2_predict_out_nn.csv")
write.csv(svm1_predict_out, "C:/Users/John/Desktop/Project/Predictions/svm1_predict_out_nn.csv")
write.csv(svm2_predict_out, "C:/Users/John/Desktop/Project/Predictions/svm2_predict_out_nn.csv")
write.csv(svm3_predict_out, "C:/Users/John/Desktop/Project/Predictions/svm3_predict_out_nn.csv")
write.csv(svm4_predict_out, "C:/Users/John/Desktop/Project/Predictions/svm4_predict_out_nn.csv")
write.csv(gbm1_predict_out, "C:/Users/John/Desktop/Project/Predictions/gbm1_predict_out_nn.csv")
write.csv(gbm2_predict_out, "C:/Users/John/Desktop/Project/Predictions/gbm2_predict_out_nn.csv")


