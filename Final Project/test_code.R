
test=read.csv("C:/Users/John/Desktop/Project/test.csv")
join_test=merge(product_data, test, by="product_uid")

#join_test=join_test[1:100,]

#trim extra spaces
join_test$product_brand=str_replace_all(join_test$product_brand, pattern = "\\s+", " ")
join_test$search_term=str_replace_all(join_test$search_term, pattern = "\\s+", " ")
join_test$product_details=str_replace_all(join_test$product_details, pattern = "\\s+", " ")

#remove stopwords (may want to also remove " x" from product dimension notation)
#rm_words=function(string, words) {
#  stopifnot(is.character(string), is.character(words))
#  spltted=strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
#  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
#}
#join_test$product_brand=rm_words(join_test$product_brand, tm::stopwords("en"))
#join_test$search_term=rm_words(join_test$search_term, tm::stopwords("en"))
#join_test$product_details=rm_words(join_test$product_details, tm::stopwords("en"))

#split camelCase instances into two words
#split_camelcase <- function(string){
#  strings <- unlist(list(string))
#  strings <- gsub("(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])", " ", strings, perl = TRUE)
#}
#join_test$product_brand=split_camelcase(join_test$product_brand)
#join_test$product_details=split_camelcase(join_test$product_details)

#subset out numbers
join_test$brand_numbers=na.omit(ex_number(join_test$product_brand, 
                                     pattern = "(\\s+)\\d+\\/\\d+|\\d+-\\d+\\/\\d+|\\d+[\\.]?\\d*",
                                     replacement = "", trim = TRUE, extract = TRUE))
#join_test$brand_numbers=str_replace_all(join_test$brand_numbers, pattern = "\\s+", "")
join_test$search_numbers=na.omit(ex_number(join_test$search_term, 
                                      pattern = "(\\s+)\\d+\\/\\d+|\\d+-\\d+\\/\\d+|\\d+[\\.]?\\d*",
                                      replacement = "", trim = TRUE, extract = TRUE))
#join_test$search_numbers=str_replace_all(join_test$search_numbers, pattern = "\\s+", "")
join_test$product_numbers=na.omit(ex_number(join_test$product_details, 
                                       pattern = "(\\s+)\\d+\\/\\d+|\\d+-\\d+\\/\\d+|\\d+[\\.]?\\d*",
                                       replacement = "", trim = TRUE, extract = TRUE))
#join_test$product_numbers=str_replace_all(join_test$product_numbers, pattern = "\\s+", "")
join_test$title_numbers=na.omit(ex_number(join_test$product_title, 
                                     pattern = "(\\s+)\\d+\\/\\d+|\\d+-\\d+\\/\\d+|\\d+[\\.]?\\d*",
                                     replacement = "", trim = TRUE, extract = TRUE))
#join_test$title_numbers=str_replace_all(join_test$title_numbers, pattern = "\\s+", "")

#bag o words
join_test$bag_product_brand=sapply(join_test$id, function(id){bag_o_words(join_test$product_brand[join_test$id==id])})
join_test$bag_search_term=sapply(join_test$id, function(id){bag_o_words(join_test$search_term[join_test$id==id])})
join_test$bag_product_details=sapply(join_test$id, function(id){bag_o_words(join_test$product_details[join_test$id==id])})
join_test$bag_product_title=sapply(join_test$id, function(id){bag_o_words(join_test$product_title[join_test$id==id])})

#add numbers to bags
#(I kept names the same so if we wanna ditch the numbers, we won't have to change any code below this)
join_test$bag_product_brand=sapply(join_test$id, function(id){
  bag_product_brand=join_test$bag_product_brand[join_test$id==id]
  brand_numbers=join_test$brand_numbers[join_test$id==id]
  c(bag_product_brand[[1]],na.omit(brand_numbers[[1]]))
})
join_test$bag_search_term=sapply(join_test$id, function(id){
  bag_search_term=join_test$bag_search_term[join_test$id==id]
  search_numbers=join_test$search_numbers[join_test$id==id]
  c(bag_search_term[[1]],na.omit(search_numbers[[1]]))
})
join_test$bag_product_title=sapply(join_test$id, function(id){
  bag_product_title=join_test$bag_product_title[join_test$id==id]
  title_numbers=join_test$title_numbers[join_test$id==id]
  c(bag_product_title[[1]],na.omit(title_numbers[[1]]))
})
join_test$bag_product_details=sapply(join_test$id, function(id){
  bag_product_details=join_test$bag_product_details[join_test$id==id]
  product_numbers=join_test$product_numbers[join_test$id==id]
  c(bag_product_details[[1]],na.omit(product_numbers[[1]]))
})

#word counts
join_test$brand_length=sapply(join_test$bag_product_brand,length)
join_test$search_length=sapply(join_test$bag_search_term,length)
join_test$details_length=sapply(join_test$bag_product_details,length)
join_test$title_length=sapply(join_test$bag_product_title,length)

#count words in search term that match brand, then divide by number of words in brand
join_test$brand_matches=sapply(join_test$id, function(id){
  brand=join_test$bag_product_brand[join_test$id==id]
  search=join_test$bag_search_term[join_test$id==id]
  length(na.omit(pmatch(brand[[1]], search[[1]], duplicates.ok=TRUE)))
})
join_test$brand_match_ratio=join_test$brand_matches/join_test$brand_length

#count words in search term that match details, then divide by number of words in details
join_test$details_matches=sapply(join_test$id, function(id){
  details=join_test$bag_product_details[join_test$id==id]
  search=join_test$bag_search_term[join_test$id==id]
  length(na.omit(pmatch(details[[1]], search[[1]], duplicates.ok=TRUE)))
})
join_test$details_match_ratio=join_test$details_matches/join_test$details_length

#count words in search term that match title, then divide by number of words in title
join_test$title_matches=sapply(join_test$id, function(id){
  title=join_test$bag_product_title[join_test$id==id]
  search=join_test$bag_search_term[join_test$id==id]
  length(na.omit(pmatch(title[[1]], search[[1]], duplicates.ok=TRUE)))
})
join_test$title_match_ratio=join_test$title_matches/join_test$title_length

join_test[join_test$brand_length==0,2]

#out_test=data.frame(id=join_test$id, details_match_ratio=join_test$details_match_ratio, brand_match_ratio=join_test$brand_match_ratio, title_match_ratio=join_test$title_match_ratio)
#write.csv(out_test, "C:/Users/John/Desktop/Project/final_test_no_numbers.csv")
