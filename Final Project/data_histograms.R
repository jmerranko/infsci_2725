library(ggplot2)

product_data=read.csv("product_data_word_count.csv")
summary(product_data$details_word_count)
search_train=read.csv("train_search_word_count1.csv")
summary(search_train$train_search_word_count)
search_test=read.csv("test_search_word_count1.csv")
summary(search_test$test_search_word_count)

ggplot(data=product_data, aes(details_word_count)) + geom_histogram(color="blue", aes(fill=..count..), binwidth=45) +
  labs(title="Number of Words per Product Data", x="Word Count", y="Frequency") + xlim(c(0,800)) +
  geom_vline(aes(xintercept=mean(details_word_count)), color="red", linetype="dashed", size=1)  
  
ggplot(data=search_train, aes(train_search_word_count)) + geom_histogram(color="blue", aes(fill=..count..), binwidth=1) +
  labs(title="Number of Words per Train Search Term", x="Word Count", y="Frequency") + xlim(c(0,14)) +
  geom_vline(aes(xintercept=mean(train_search_word_count)), color="red", linetype="dashed", size=1)

ggplot(data=search_test, aes(test_search_word_count)) + geom_histogram(color="blue", aes(fill=..count..), binwidth=1) +
  labs(title="Number of Words per Test Search Term", x="Word Count", y="Frequency") + xlim(c(0,14)) + 
  geom_vline(aes(xintercept=mean(test_search_word_count)), color="red", linetype="dashed", size=1)

predictor_data=read.csv("product_data_word_count.csv")