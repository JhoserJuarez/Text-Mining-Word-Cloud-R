# Text Mining - WORD CLOUD

# From the below code you will be learning how to do basic cleansing of your Raw Text Data, how to make Corpus, 
# how to do Corpus Cleansing, to make TDM, how to come up with Word Cloud and word frequecny barplot, 
# and if there is field called Category or whateveer be in your Raw data, then how to come up with Word Cloud for 
# that specific rows only.

#Packages you require are given below
library(rJava)
library(tm)		
library(SnowballC)
library(wordcloud)
library(RWeka)	
library(RWekajars)
library(textir)
library(igraph)
library(qdap)		
library(maptpx)
library(RTextTools)
library(mlbench)
library(ggplot2)
library(e1071)
library(gsubfn)
library(stringr)
library(stringi)



# READ THE TEXT DATA CSV FILE
Raw_Data <- TextData
# My dataset "TextData" has two fields 
# 1. Text -  where all the raw text matter is given row wise
# 2. Category - where the Categoryname to which the text matter belongs to
# Row Header | Category       |  Text
# Row1       | Apple          |  apple last bought was not good
# Row2       | Orange         |  please use orange and other fruits if you feel not healthy
# Row3       | Apple          |  why did u bring just 1 kg of apple.....? 
# so on and so forth....
# to come up with a word cloud you don't need the Category field, just the Text field will do.
dim(Raw_Data)	# Check the Dataset Dimension



# WORD REPLACEMENT - Data Cleansing 
class(Raw_Data$Text)
x<-as.vector(Raw_Data$Text)
class(x)

x<- gsub("newword","word in Raw_Data",x,ignore.case=T)
# You can replace as many words or special characters in the Raw_Data by using gsub() in gsubfn package


# CORPUS Creation and Cleansing
Raw_Data$Text<-as.factor(x)

x1corpus = Corpus(VectorSource(Raw_Data$Text)) # Constructs a source for a vector as input


x1corpus <- tm_map(x1corpus, stripWhitespace) 	  # Removes white space
x1corpus <- tm_map(x1corpus, tolower)		          # Converts to lower case
x1corpus <- tm_map(x1corpus, removePunctuation)	  # Removes punctuation marks
x1corpus <- tm_map(x1corpus, removeNumbers)		    # Removes numbers in the documents
x1corpus <- tm_map(x1corpus, removeWords, c(stopwords('english')))   # You can add more stopwords as per your Business Requirements
x1corpus <- tm_map(x1corpus, PlainTextDocument)

        
#Corpus Inspection
strwrap(x1corpus[[1]])
corpus_data<-data.frame(inspect_text(x1corpus))
dim(corpus_data)
write.table(corpus_data,"Corpus_data.txt",sep="\t",row.names=FALSE)
#Corpus Inspection Ends


# WORD CLOUDS

#UniGram Word Cloud
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1)) #You can make Bigram, Trigram, Quadgram etc. 
                                                                                 # by simly changing min and max value to 2,3,4 etc. 
tdm.bigram = TermDocumentMatrix(x1corpus,control = list(tokenize = BigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
windows()
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, "UNIGRAM WORD CLOUD")
  wordcloud(freq.df$word,freq.df$freq,max.words=100,scale = c(4,.5),random.order = F, colors=1:10, main = "Title")
                  
#BarPlot for words in UniGram Word Cloud
windows()
  ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() +xlab("Unigrams") + 
  ylab("Frequency") + ggtitle("GENERAL - Most Frequent Unigrams")

  
#CATEGORYWISE WORD CLOUD
#Create Seperate Index Vector for Each Category
Category_index <- which(Raw_Data$Category == "Apple")
Category_index[1:3]

#TriGram Word Cloud Categorywise- Category1
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.bigram = TermDocumentMatrix(x1corpus[Category_index],control = list(tokenize = BigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
windows()
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, "APPLE - TRIGRAM WORD CLOUD")
  wordcloud(freq.df$word,freq.df$freq,max.words=50,scale = c(2,.5),random.order = F, colors=1:10, main = "Title")
        
# CODE END HERE
