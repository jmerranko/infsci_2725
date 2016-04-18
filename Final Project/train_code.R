library(stringr)
library(tm)
library(qdap)
library(jsonlite)

product_data=fromJSON("C:/Users/John/Desktop/Project/product_data1.txt")
train=read.csv("C:/Users/John/Desktop/Project/train.csv")
train=as.data.frame(sapply(train, tolower))
join=merge(product_data, train, by="product_uid")

#join=join[1:100,]

#trim extra spaces
join$product_brand=str_replace_all(join$product_brand, pattern = "\\s+", " ")
join$search_term=str_replace_all(join$search_term, pattern = "\\s+", " ")
join$product_details=str_replace_all(join$product_details, pattern = "\\s+", " ")

#remove stopwords (may want to also remove " x" from product dimension notation)
#rm_words=function(string, words) {
#  stopifnot(is.character(string), is.character(words))
#  spltted=strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
#  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
#}
#join$product_brand=rm_words(join$product_brand, tm::stopwords("en"))
#join$search_term=rm_words(join$search_term, tm::stopwords("en"))
#join$product_details=rm_words(join$product_details, tm::stopwords("en"))

#split camelCase instances into two words
#split_camelcase <- function(string){
#  strings <- unlist(list(string))
#  strings <- gsub("(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])", " ", strings, perl = TRUE)
#}
#join$product_brand=split_camelcase(join$product_brand)
#join$product_details=split_camelcase(join$product_details)

#subset out numbers
join$brand_numbers=na.omit(ex_number(join$product_brand, 
                               pattern = "(\\s+)\\d+\\/\\d+|\\d+-\\d+\\/\\d+|\\d+[\\.]?\\d*",
                               replacement = "", trim = TRUE, extract = TRUE))
#join$brand_numbers=str_replace_all(join$brand_numbers, pattern = "\\s+", "")
join$search_numbers=na.omit(ex_number(join$search_term, 
                               pattern = "(\\s+)\\d+\\/\\d+|\\d+-\\d+\\/\\d+|\\d+[\\.]?\\d*",
                               replacement = "", trim = TRUE, extract = TRUE))
#join$search_numbers=str_replace_all(join$search_numbers, pattern = "\\s+", "")
join$product_numbers=na.omit(ex_number(join$product_details, 
                               pattern = "(\\s+)\\d+\\/\\d+|\\d+-\\d+\\/\\d+|\\d+[\\.]?\\d*",
                                replacement = "", trim = TRUE, extract = TRUE))
#join$product_numbers=str_replace_all(join$product_numbers, pattern = "\\s+", "")
join$title_numbers=na.omit(ex_number(join$product_title, 
                               pattern = "(\\s+)\\d+\\/\\d+|\\d+-\\d+\\/\\d+|\\d+[\\.]?\\d*",
                               replacement = "", trim = TRUE, extract = TRUE))
#join$title_numbers=str_replace_all(join$title_numbers, pattern = "\\s+", "")

#stemming
library(SnowballC)
library(RWeka)
stem_string <- function(x){
  myCorpus <- Corpus(VectorSource(x))
  myCorpus <- tm_map(myCorpus, stemDocument)
  myCorpusTokenized <- lapply(myCorpus, scan_tokenizer)
  myTokensStemCompleted <- lapply(myCorpusTokenized, stemCompletion, myCorpus)
  return(sapply(myTokensStemCompleted, paste, collapse = " "))
}
join$product_brand=stem_string(join$product_brand)
join$search_term=stem_string(join$search_term)
join$product_details=stem_string(join$product_details)
join$product_title=stem_string(join$product_title)

#bag o words
join$bag_product_brand=sapply(join$id, function(id){bag_o_words(join$product_brand[join$id==id])})
join$bag_search_term=sapply(join$id, function(id){bag_o_words(join$search_term[join$id==id])})
join$bag_product_details=sapply(join$id, function(id){bag_o_words(join$product_details[join$id==id])})
join$bag_product_title=sapply(join$id, function(id){bag_o_words(join$product_title[join$id==id])})

#add numbers to bags
#(I kept names the same so if we wanna ditch the numbers, we won't have to change any code below this)
join$bag_product_brand=sapply(join$id, function(id){
  bag_product_brand=join$bag_product_brand[join$id==id]
  brand_numbers=join$brand_numbers[join$id==id]
  c(bag_product_brand[[1]],na.omit(brand_numbers[[1]]))
})
join$bag_search_term=sapply(join$id, function(id){
  bag_search_term=join$bag_search_term[join$id==id]
  search_numbers=join$search_numbers[join$id==id]
  c(bag_search_term[[1]],na.omit(search_numbers[[1]]))
})
join$bag_product_title=sapply(join$id, function(id){
  bag_product_title=join$bag_product_title[join$id==id]
  title_numbers=join$title_numbers[join$id==id]
  c(bag_product_title[[1]],na.omit(title_numbers[[1]]))
})
join$bag_product_details=sapply(join$id, function(id){
  bag_product_details=join$bag_product_details[join$id==id]
  product_numbers=join$product_numbers[join$id==id]
  c(bag_product_details[[1]],na.omit(product_numbers[[1]]))
})

#word counts
join$brand_length=sapply(join$bag_product_brand,length)
join$search_length=sapply(join$bag_search_term,length)
join$details_length=sapply(join$bag_product_details,length)
join$title_length=sapply(join$bag_product_title,length)

#count words in search term that match brand, then divide by number of words in brand
join$brand_matches=sapply(join$id, function(id){
  brand=join$bag_product_brand[join$id==id]
  search=join$bag_search_term[join$id==id]
  length(na.omit(pmatch(brand[[1]], search[[1]], duplicates.ok=TRUE)))
})
join$brand_match_ratio=join$brand_matches/join$brand_length

#count words in search term that match details, then divide by number of words in details
join$details_matches=sapply(join$id, function(id){
  details=join$bag_product_details[join$id==id]
  search=join$bag_search_term[join$id==id]
  length(na.omit(pmatch(details[[1]], search[[1]], duplicates.ok=TRUE)))
})
join$details_match_ratio=join$details_matches/join$details_length

#count words in search term that match title, then divide by number of words in title
join$title_matches=sapply(join$id, function(id){
  title=join$bag_product_title[join$id==id]
  search=join$bag_search_term[join$id==id]
  length(na.omit(pmatch(title[[1]], search[[1]], duplicates.ok=TRUE)))
})
join$title_match_ratio=join$title_matches/join$title_length

#out=data.frame(relevance=join$relevance, details_match_ratio=join$details_match_ratio,
#               brand_match_ratio=join$brand_match_ratio, title_match_ratio=join$title_match_ratio)
#write.csv(out, "C:/Users/John/Desktop/Project/New/final_predictors.csv")

model=na.omit(join)
model$relevance_num=as.numeric(levels(model$relevance))[as.integer(model$relevance)]

plot(x=model$brand_match_ratio, y=model$relevance_num)
lm=glm(data=model, relevance_num~brand_match_ratio)
co=coef(lm)
abline(lm, col="blue")
summary(lm(data=model, relevance_num~brand_match_ratio))

plot(x=model$details_match_ratio, y=model$relevance_num)
lm=glm(data=model, relevance_num~details_match_ratio)
co=coef(lm)
abline(lm, col="blue")
summary(lm(data=model, relevance_num~details_match_ratio))

plot(x=model$title_match_ratio, y=model$relevance_num)
lm=glm(data=model, relevance_num~title_match_ratio)
co=coef(lm)
abline(lm, col="blue")
summary(lm(data=model, relevance_num~title_match_ratio))

summary(lm(data=model, relevance_num~details_match_ratio+brand_match_ratio+title_match_ratio))

###################################################################################################
#Below is where I plan to run some prediction models

library(foreign)
library(ROCR)
library(e1071)
library(randomForest)
library(party)
library(FSelector)
library(rpart)
library(gbm)
set.seed(1)

tree=rpart(data=model, relevance_num~details_match_ratio+brand_match_ratio+title_match_ratio)
plot(tree, uniform=TRUE)
text(tree, all=TRUE, cex=.75)
rf=randomForest(data=model, relevance_num~details_match_ratio+brand_match_ratio+title_match_ratio)
svm=svm(data=model, relevance_num~details_match_ratio+brand_match_ratio+title_match_ratio)
gbm=gbm(data=model, relevance_num~details_match_ratio+brand_match_ratio+title_match_ratio, distribution="gaussian")

final_test=read.csv("C:/Users/John/Desktop/Project/final_test.csv")

final_test$brand_match_ratio=ifelse(is.na(final_test$brand_match_ratio),
                                    median(na.omit(final_test$brand_match_ratio)),
                                    final_test$brand_match_ratio)
summary(final_test)

join_test[1:1000,]
