#CSC Text Mining -WORD CLOUD

#Packages needed
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



# READ THE RAW TEXT DATA CSV FILE
Raw_TextData <- Raw_TextData
dim(Raw_TextData)	# Check the Dataset Dimension



# WORD REPLACEMENT - Data Cleansing 
class(Raw_TextData$Description)
x<-as.vector(Raw_TextData$Description)
class(x)

x<- gsub("o365","Officecloud",x,ignore.case=T)
x<- gsub("office 365","Officecloud",x,ignore.case=T)

x<- gsub("Mozilla firefox browser", "mozillafirefox", x, ignore.case = T)
x<- gsub("mozilla firefox", "mozillafirefox", x, ignore.case = T)
x<- gsub("mozila firefox", "mozillafirefox", x, ignore.case = T)
x<- gsub("firefox", "mozillafirefox", x, ignore.case = T)
x<- gsub("firefox browser", "mozillafirefox", x, ignore.case = T)
x<- gsub("Mozilla firefox web browser", "mozillafirefox", x, ignore.case = T)
x<- gsub("mozillafirefox mozillafirefox", "mozillafirefox", x, ignore.case = T)
x<- gsub("mozillamozillafirefox", "mozillafirefox", x, ignore.case = T)

x<- gsub("nw", "new", x, ignore.case = T)
x<- gsub("lotus note","Lotusnotes",x,ignore.case=T)
x<- gsub("lotus notes","Lotusnotes",x,ignore.case=T)
x<- gsub("lotusnotes","lotusnotes",x,ignore.case=T)
x<- gsub("lotusnotess","lotusnotes",x,ignore.case=T)

x<- gsub("un able", "unable", x, ignore.case = T)
x<- gsub("not able", "unable", x, ignore.case = T)
x<- gsub("needs", "need", x, ignore.case = T)

x<- gsub("log in", "login", x, ignore.case = T)
x<- gsub("log into", "login", x, ignore.case = T)

x<- gsub("user name", "username", x, ignore.case = T)

x<- gsub("snowneed", "snow need", x, ignore.case = T)
x<- gsub("snowofficecloud", "snow officecloud", x, ignore.case = T)
x<- gsub("snowoutlook", "snow outlook", x, ignore.case = T)
x<- gsub("snowservice", "snow service", x, ignore.case = T)
x<- gsub("snowskype", "snow skype", x, ignore.case = T)
x<- gsub("snowunable", "snow unable", x, ignore.case = T)

x<- str_replace_all(x, "[[:punct:]]", "   ")
x<- gsub('([[:punct:]])|\\s+',' ',x)

x<- gsub("can't","cannot", x, ignore.case = T)
x<- gsub("can t","cannot", x, ignore.case = T)
x<- gsub("cant","cannot", x, ignore.case = T)
x<- gsub("can not", "cannot", x, ignore.case = T)
x<- gsub("won't", "wouldnot", x, ignore.case = T)
x<- gsub("won t", "wouldnot", x, ignore.case = T)
x<- gsub("wont", "wouldnot", x, ignore.case = T)
x<- gsub("doesn't", "doesnot", x, ignore.case = T)
x<- gsub("doesn t", "doesnot", x, ignore.case = T)
x<- gsub("doesnt", "doesnot", x, ignore.case = T)
x<- gsub("does not", "doesnot", x, ignore.case = T)
x<- gsub("isn t","isnot", x, ignore.case = T)

x<- gsub("e-mail", "email", x, ignore.case = T)
x<- gsub("e mail", "email", x, ignore.case = T)
x<- gsub("mail", "email", x, ignore.case = T)
x<- gsub("eemail", "email", x, ignore.case = T)
x<- gsub("e email", "email", x, ignore.case = T)
x<- gsub("e emails", "email", x, ignore.case = T)

x<- gsub("Business Unit", "BusinessUnit", x, ignore.case = T)
x<- gsub("no error", "noerror", x, ignore.case = T)
x<- gsub("pass word", "password", x, ignore.case = T)
x<- gsub("global pass", "global password", x, ignore.case = T)
x<- gsub("password password", "password", x, ignore.case = T)
x<- gsub("passwordword", "password", x, ignore.case = T)


#Text Inspection
xText = Corpus(VectorSource(x))
Text_data<-data.frame(inspect_text(xText))
write.table(Text_data,"Text_data.txt",sep="\t",row.names=FALSE)
# Word Replacement ends here



# CORPUS Creation and Cleansing
Raw_TextData$Description<-as.factor(x)
class(Raw_TextData$Description)

x1corpus = Corpus(VectorSource(Raw_TextData$Description)) # Constructs a source for a vector as input


x1corpus <- tm_map(x1corpus, stripWhitespace) 	  # Removes white space
x1corpus <- tm_map(x1corpus, tolower)		          # Converts to lower case
x1corpus <- tm_map(x1corpus, removePunctuation)	  # Removes punctuation marks
x1corpus <- tm_map(x1corpus, removeNumbers)		    # Removes numbers in the documents
x1corpus <- tm_map(x1corpus, removeWords, c(stopwords('english'), "snowprobelm","snowissue", 
                                            "snowaccountalpha","snow", "issue", 
                                            "problem", "accountalpha", "issues", "e e", "indiaphone"))

x1corpus <- tm_map(x1corpus, PlainTextDocument)

        
#Corpus Inspection
strwrap(x1corpus[[1]])
corpus_data<-data.frame(inspect_text(x1corpus))
dim(corpus_data)
write.table(corpus_data,"Corpus_data.txt",sep="\t",row.names=FALSE)
#Corpus Inspection Ends




# WORD CLOUDS

#GENERAL - UniGram Word Cloud
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm.bigram = TermDocumentMatrix(x1corpus,control = list(tokenize = BigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
windows()
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "GENERAL - UNIGRAM WORD CLOUD")
wordcloud(freq.df$word,freq.df$freq,max.words=100,scale = c(4,.5),random.order = F, colors=1:10, main = "Title")
                  
                  #BarPlot for words in UniGram Word Cloud
                  windows()
                  ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                    geom_bar(stat = "identity") + coord_flip() +
                    xlab("Unigrams") + ylab("Frequency") +
                    ggtitle("GENERAL - Most Frequent Unigrams")



                  #Below Code for Checking Only   

                  #GENERAL-BiGram Word Cloud
                  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
                  tdm.bigram = TermDocumentMatrix(x1corpus,control = list(tokenize = BigramTokenizer))
                  freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
                  freq.df = data.frame(word=names(freq), freq=freq)
                  head(freq.df, 20)
                  windows()
                  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
                  par(mar=rep(0, 4))
                  plot.new()
                  text(x=0.5, y=0.5, "GENERAL - BIGRAM WORD CLOUD")
                  wordcloud(freq.df$word,freq.df$freq,max.words=70,scale = c(3,.5), random.order = F, colors=1:10, main = "Title")
                  #BarPlot for words in BiGram Word Cloud
                  windows()
                  ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                    geom_bar(stat = "identity") + coord_flip() +
                    xlab("Bigrams") + ylab("Frequency") +
                    ggtitle("GENERAL - Most Frequent Bigrams")


                  #GENERAL-TriGram Word Cloud
                  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
                  tdm.bigram = TermDocumentMatrix(x1corpus,control = list(tokenize = BigramTokenizer))
                  freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
                  freq.df = data.frame(word=names(freq), freq=freq)
                  head(freq.df, 20)
                  windows()
                  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
                  par(mar=rep(0, 4))
                  plot.new()
                  text(x=0.5, y=0.5, "GENERAL - TRIGRAM WORD CLOUD")
                  wordcloud(freq.df$word,freq.df$freq,max.words=50,scale = c(2,.5),random.order = F, colors=1:10, main = "Title")
                  #BarPlot for words in TriGram Word Cloud
                  windows()
                  ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                    geom_bar(stat = "identity") + coord_flip() +
                    xlab("Trigrams") + ylab("Frequency") +
                    ggtitle("GENERAL - Most Frequent Trigrams")





                  
#CATEGORYWISE WORD CLOUD

#Five Categories in the Dataset - Firefox, Lotus, Outlook, Skype, Word/Excel

#Create Seperate Index Vector for Each Category
Firefox_indices <- which(Raw_TextData$Category..Eye.ball.Analysis. == "Firefox")
Firefox_indices[1:3]
Lotus_indices <- which(Raw_TextData$Category..Eye.ball.Analysis. == "Lotus")
Lotus_indices[1:3]
Outlook_indices <- which(Raw_TextData$Category..Eye.ball.Analysis. == "Outlook")
Outlook_indices[1:3]
Skype_indices <- which(Raw_TextData$Category..Eye.ball.Analysis. == "Skype")
Skype_indices[1:3]
Wordexcel_indices <- which(Raw_TextData$Category..Eye.ball.Analysis. == "Word/Excel")
Wordexcel_indices[1:3]



#TriGram Word Cloud Categorywise- Firefox
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.bigram = TermDocumentMatrix(x1corpus[Firefox_indices],control = list(tokenize = BigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
windows()
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "FIREFOX - TRIGRAM WORD CLOUD")
wordcloud(freq.df$word,freq.df$freq,max.words=50,scale = c(2,.5),random.order = F, colors=1:10, main = "Title")
                         
                         #For Word Frequency Barplot    
                          windows()
                              ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                                geom_bar(stat = "identity") + coord_flip() +
                                xlab("Trigrams") + ylab("Frequency") +
                                ggtitle("FIREFOX - Most frequent Trigrams")

                            #BiGram Word Cloud Categorywise- Firefox
                            BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
                            tdm.bigram = TermDocumentMatrix(x1corpus[Firefox_indices],control = list(tokenize = BigramTokenizer))
                            freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
                            freq.df = data.frame(word=names(freq), freq=freq)
                            head(freq.df, 20)
                            windows()
                            layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
                            par(mar=rep(0, 4))
                            plot.new()
                            text(x=0.5, y=0.5, "FIREFOX - BIGRAM WORD CLOUD")
                            wordcloud(freq.df$word,freq.df$freq,max.words=100,scale = c(4,.5),random.order = F, colors=1:10, main = "Title")
                            windows()
                            ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                              geom_bar(stat = "identity") + coord_flip() +
                              xlab("Trigrams") + ylab("Frequency") +
                              ggtitle("FIREFOX - Most frequent Bigrams")



#TriGram Word Cloud Categorywise- Lotus
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.bigram = TermDocumentMatrix(x1corpus[Lotus_indices],control = list(tokenize = BigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
windows()
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "LOTUS - TRIGRAM WORD CLOUD")
wordcloud(freq.df$word,freq.df$freq,max.words=60,scale = c(2,.5),random.order = F, colors=1:10, main = "Title")

                          #For Word Frequency Barplot
                          windows()
                          ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                            geom_bar(stat = "identity") + coord_flip() +
                            xlab("Trigrams") + ylab("Frequency") +
                            ggtitle("LOTUS - Most frequent Trigrams")




#TriGram Word Cloud Categorywise- Outlook
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.bigram = TermDocumentMatrix(x1corpus[Outlook_indices],control = list(tokenize = BigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
windows()
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "OUTLOOK - TRIGRAM WORD CLOUD")
wordcloud(freq.df$word,freq.df$freq,max.words=80,scale = c(2,.5),random.order = F, colors=1:10, main = "Title")

                            #For Word Frequency Barplot 
                            windows()
                            ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                              geom_bar(stat = "identity") + coord_flip() +
                              xlab("Trigrams") + ylab("Frequency") +
                              ggtitle("OUTLOOK - Most frequent Trigrams")

                            #BiGram Word Cloud Categorywise- Outlook
                            BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
                            tdm.bigram = TermDocumentMatrix(x1corpus[Outlook_indices],control = list(tokenize = BigramTokenizer))
                            freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
                            freq.df = data.frame(word=names(freq), freq=freq)
                            head(freq.df, 20)
                            windows()
                            layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
                            par(mar=rep(0, 4))
                            plot.new()
                            text(x=0.5, y=0.5, "OUTLOOK - BIGRAM WORD CLOUD")
                            wordcloud(freq.df$word,freq.df$freq,max.words=100,scale = c(4,.5),random.order = F, colors=1:10, main = "Title")
                            windows()
                            ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                              geom_bar(stat = "identity") + coord_flip() +
                              xlab("Trigrams") + ylab("Frequency") +
                              ggtitle("OUTLOOK - Most frequent Bigrams")

                            
                            
                            

#TriGram Word Cloud Categorywise- Skype
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.bigram = TermDocumentMatrix(x1corpus[Skype_indices],control = list(tokenize = BigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 80)
windows()
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "SKYPE - TRIGRAM WORD CLOUD")
wordcloud(freq.df$word,freq.df$freq,max.words=80,scale = c(2,.5),random.order = F, colors=1:10, main = "Title")

                          #For Word Frequency Barplot
                          windows()
                          ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                            geom_bar(stat = "identity") + coord_flip() +
                            xlab("Trigrams") + ylab("Frequency") +
                            ggtitle("SKYPE - Most frequent Trigrams")




#TriGram Word Cloud Categorywise- Word/Excel
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.bigram = TermDocumentMatrix(x1corpus[Wordexcel_indices],control = list(tokenize = BigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
windows()
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "WORD/EXCEL - TRIGRAM WORD CLOUD")
wordcloud(freq.df$word,freq.df$freq,max.words=50,scale = c(2,.5),random.order = F, colors=1:10, main = "Title")

                          #For Word Frequency Barplot
                          windows()
                          ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
                            geom_bar(stat = "identity") + coord_flip() +
                            xlab("Trigrams") + ylab("Frequency") +
                            ggtitle("WORD/EXCEL - Most frequent Trigrams")



