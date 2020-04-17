library(data.table)
library(tm)
library(wordcloud)
library(textreg)
library(dplyr)
library(purrr)
data=fread("/Users/spilios.tzouras/Documents/FERRATUM_LAST_COPY/Desktop_from_laptop/NordigenData.csv",  sep=";")
data  <- data[sample(nrow(data), 50000), ]


# removeSymbols <- function(x) gsub("[^A-Za-z0-9]"," ",x)
# removexxPattern <- function(x) gsub("([x])\\1+","",x)
# replacePunctuation <- content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))})
# 
# 
# desc_corpus <-  VCorpus(VectorSource(data$description))
# clean_corpus <- tm_map(desc_corpus,replacePunctuation)
# clean_corpus <- tm_map(clean_corpus,removeNumbers)
# clean_corpus <- tm_map(clean_corpus,content_transformer(tolower))
# clean_corpus <- tm_map(clean_corpus,removeWords, stopwords("en"))
# clean_corpus <- tm_map(clean_corpus,content_transformer(removeSymbols))
# clean_corpus <- tm_map(clean_corpus,content_transformer(removexxPattern))
# clean_corpus <- tm_map(clean_corpus,stripWhitespace)
# 
# 
# 
# clean_corpus <- function(corpus){
#   removeSymbols <- function(x) gsub("[^A-Za-z0-9]"," ",x)
#   removexxPattern <- function(x) gsub("([x])\\1+","",x)
#   removeSpacesEnds <- function(x) trimws(x, which ="both")
#   removeLessThanThreeLetters <- function(x) gsub("\\b\\w{1,2}\\b","",x)
#   res <- corpus %>% 
#     tm::tm_map(replacePunctuation) %>% 
#     tm::tm_map(removeNumbers) %>% 
#     tm::tm_map(content_transformer(tolower)) %>%
#     tm::tm_map(removeWords, stopwords("en")) %>%
#     tm::tm_map(content_transformer(removeSymbols)) %>%
#     tm::tm_map(content_transformer(removexxPattern)) %>%
#     tm::tm_map(content_transformer(removeLessThanThreeLetters))%>%
#     tm::tm_map(stripWhitespace) %>%
#     tm::tm_map(content_transformer(removeSpacesEnds))
#   return(res)
# }
# 
# 
# 
# split_clean_corpus <- function(data) {data %>% 
#   pull(description)%>%
#   VectorSource() %>%
#   VCorpus()%>%
#   clean_corpus
# 
# }




clean_corpus <- function(data){
  removeSymbols <- function(x) gsub("[^A-Za-z0-9]"," ",x)
  removexxPattern <- function(x) gsub("([x])\\1+","",x)
  removeSpacesEnds <- function(x) trimws(x, which ="both")
  removeLessThanThreeLetters <- function(x) gsub("\\b\\w{1,2}\\b","",x)
  res <- data %>% 
    pull(description)%>%
    VectorSource() %>%
    VCorpus()%>% 
    tm::tm_map(removeNumbers) %>% 
    tm::tm_map(content_transformer(tolower)) %>%
    tm::tm_map(removeWords, stopwords("en")) %>%
    tm::tm_map(content_transformer(removeSymbols)) %>%
    tm::tm_map(content_transformer(removexxPattern)) %>%
    tm::tm_map(content_transformer(removeLessThanThreeLetters))%>%
    tm::tm_map(stripWhitespace) %>%
    tm::tm_map(content_transformer(removeSpacesEnds))
  return(res)
}




create_pos_neg_corpus <- function(data){
  data %>% 
    dplyr::mutate(type=as.factor(ifelse(amount>=0,"positive","negative")))%>%
    split(.$type)%>%
    purrr::map(clean_corpus)
}



# create_pos_neg_corpus <- function(data){
#   
#   pos_corpus <- data %>% 
#     dplyr::filter(amount>=0)%>%
#     pull(description)%>%
#     VectorSource() %>%
#     VCorpus()%>%
#     clean_corpus
#   
#   neg_corpus <- data %>% 
#     dplyr::filter(amount<0)%>%
#     pull(description)%>%
#     VectorSource() %>%
#     VCorpus()%>%
#     clean_corpus
#   
#   return(res=list("pos_corpus"=pos_corpus,"neg_corpus"=neg_corpus))
# }


corpus_clean <- clean_corpus(data)
desc_tdm <- TermDocumentMatrix(corpus_clean)
desc_m <- as.matrix(desc_tdm)
desc_m[475:478, 2593:2594]
term_frequency <- rowSums(desc_m)
term_frequency <- sort(term_frequency,decreasing = TRUE)
term_frequency[1:50]

library(viridisLite)
color_pal <- cividis(n=5)
terms_vec <- names(term_frequency)
wordcloud(terms_vec,term_frequency,max.words = 50,colors = color_pal)

barplot(term_frequency[1:25],col=2,las=2)


        
clean_description <- convert.tm.to.character(clean_corpus(data))





pso_neg <- create_pos_neg_corpus(data)



pos_text <- data%>%filter(amount>=0)%>%clean_corpus %>% convert.tm.to.character%>%paste(collapse=" ")
neg_text <- data%>%filter(amount<0)%>%clean_corpus %>% convert.tm.to.character%>%paste(collapse=" ")
all_descr <- c(pos_text,neg_text)


all_descr%>%
  VectorSource%>%
  VCorpus%>%
  TermDocumentMatrix%>%as.matrix%>%
  commonality.cloud(,max.words =60,colors="steelblue")



all_descr%>%
  VectorSource%>%
  VCorpus%>%
  TermDocumentMatrix%>%
  `colnames<-`(c("Positive", "Negative"))%>%
  as.matrix%>%
  comparison.cloud(,colors=c("blue","red"),max.words=50)










sel=which(complete.cases(dt)==TRUE)
dt=dt[sel,]

x <- model.matrix(fml, dt)
y <- as.matrix(dt$target30, ncol=1)




trainX=x[1:round(0.8*dim(x)[1]),] ### 80% of the data as training set ######
testX=x[(round(0.8*dim(x)[1]) +1 ) : (round(0.8*dim(x)[1]) + round(0.2*dim(x)[1]) ),]






trainY=y[1:round(0.8*length(y))] ### 80% of the data as training set ######
testY=y[(round(0.8*length(y)) +1 ) : (round(0.8*length(y)) + round(0.2*length(y)) )]

model=cv.glmnet(trainX,trainY,nfolds=8,alpha=1,family="binomial")
l=model$lambda.min
finalmodel=glmnet(trainX,trainY,alpha=1,lambda=l)
cf=coef(finalmodel)


cvob1=cv.glmnet(x=as.matrix(trainX), y=trainY,family="binomial",alpha=1, 
                type.measure="auc", nfolds = 5, lambda = seq(0.001,0.1,by = 0.001),
                standardize=FALSE)




probabilityClassOne <- predict(finalmodel,testX, type = "response") 



#probabilityClassOne <- predict(finalmodel,x, type = "response") 

probabilityClassZero <-  1 - probabilityClassOne

result <- data.frame("One" = probabilityClassOne, "Zero" = probabilityClassZero)


par(mar = rep(2, 4))


require(ROCR)
pred <- prediction(result[,1], testY)
#pred <- prediction(result[,1], dt$Percentage[2753:3440])

perf <- performance(pred, "auc")
areaUnderCurve <- slot(perf, "y.values")
auc <- as.numeric(unlist(areaUnderCurve))


roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf,cex.lab=1.5,cex.main=1.5,main="ROC: ",lwd=2.5)
abline(a=0, b= 1)
2*auc



library("spacyr")
spacy_install()
spacy_initialize()
spacy_parse(clean_description[1:1000])