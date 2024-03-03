#### Packages installation

#install.packages("XML")
#install.packages("RCurl")
#install.packages("httr")
#install.packages("readxl")
#install.packages("tm") # A text mining package
#install.packages("SnowballC")
#install.packages("wordcloud") # word-cloud generator
#install.packages("RColorBrewer") # Colors

library("XML")
library("RCurl")
library("httr")
library("readxl")
library("tm") # A text mining package
library("SnowballC")
library("wordcloud") # word-cloud generator
library("RColorBrewer") # Colors


#Data Importation
Dataset <- read_excel("/Users/adamdanielgreen/Desktop/Business Statistics/Text Analytics-20231219/Data_test_analytics.xlsx")
head(Dataset)

#Data subsetting
Comments <- Dataset$Texts


#Remove potential emojis
Comments <- gsub("[^\x01-\x7F]","",Comments)


#All Data are put into a corpus (Collection of comments)
Comments_corpus <- Corpus(VectorSource(Comments))

#Data are converted to lower case
Comments_corpus <- tm_map(Comments_corpus, tolower)

# We remove punctuation
Comments_corpus <- tm_map(Comments_corpus, removePunctuation)

# We remove numbers
Comments_corpus <- tm_map(Comments_corpus, removeNumbers)

# We remove non necessar terms: stopwords
Comments_corpus <- tm_map(Comments_corpus,
                          function(x)removeWords(x,stopwords("en")))
Comments_corpus <- tm_map(Comments_corpus, removeNumbers)

# We remove our own stopwords
Comments_corpus <- tm_map(Comments_corpus, removeWords,
                          c("make","every","going","many","will","now","can","one",
                            "want","way","just"))

# The document-term matrix
data_final.tdm <- TermDocumentMatrix(Comments_corpus)
m <- as.matrix(data_final.tdm)
m[1:8,1:15]

#Most frequent terms in our matrix
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Word Frequency
barplot(d[1:15,]$freq, las = 3, names.arg = d[1:15,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

# Identify terms used at least 30 times
findFreqTerms(data_final.tdm, lowfreq=10)

# The word cloud
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=100, random.order=FALSE,
          colors=brewer.pal(4,"Dark2"))

# Sentiment Analysis: Global sentiment
# Package
#install.packages("sentimentr")
library("sentimentr")

# Sentiment Analysis: Global sentiment
Comments_bloc<-paste(Comments, collapse = "\n")
sentiment(Comments_bloc)

# Package for pipes
#install.packages("dplyr")
library("dplyr")

# Sentiment Analysis: Global sentiment
Comments_bloc %>%extract_sentiment_terms()

# Sentiment Analysis: Global sentiment
sentiment_by(Comments_bloc)


#Sentiment Analysis: sentiment by type
#install.packages("syuzhet")
#install.packages("ggplot2")
library(syuzhet)
library(ggplot2)

# Sentiment Analysis: sentiment by type
ew_sentiment<-get_nrc_sentiment(Comments, language = "english")
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

