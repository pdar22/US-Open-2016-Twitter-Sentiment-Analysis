setwd("/users/pranavdar/Desktop/PGPBABI/Web And Social Media Analytics")

#Load the required libraries
library(qdap)
library(stringi)
library(SnowballC)
library(ggplot2)
library(tm)

#Import the tweets
tweets <- read.csv("TwitterData.csv", stringsAsFactors = FALSE)

str(tweets)

#Convert date to correct format
tweets$created <- as.Date(tweets$created, format = "%d-%m-%y")

#Remove character string between < >
tweets$text <- genX(tweets$text, " <", ">")

#Create document corpus
mycorpus <- Corpus(VectorSource(tweets$text))

#Convert to lowercase
mycorpus <- tm_map(mycorpus, content_transformer(stri_trans_tolower))

#Remove the URLs
remove_URL <- function(x) gsub("http[^[:space:]]*", "", x)
mycorpus <- tm_map(mycorpus, content_transformer(remove_URL))

#Remove everything that isn't in English
removeNumPunc <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mycorpus <- tm_map(mycorpus, content_transformer(removeNumPunc))

#Remove stopwords
remove_stop <- c((stopwords('english')), c("rt", "use", "used", "via", "emp"))
mycorpus <- tm_map(mycorpus, removeWords, remove_stop)

#Remove single letter words
removeSingle <- function(x) gsub(" . ", " ", x)

#Remove extra whitespaces
mycorpus <- tm_map(mycorpus, stripWhitespace)

#Save a copy of the corpus for stem completion later
mycorpus_copy <- mycorpus

#Stem words in the corpus
mycorpus <- tm_map(mycorpus, stemDocument)
writeLines(strwrap(mycorpus[[250]]$content, 60))

#Function to correct/complete the text after stemming
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary = dictionary)
  x <- paste(x, sep = "", collapse = " ")
  PlainTextDocument(stripWhitespace(x))
}

#Stem complete and display the same tweet above with the completed and corrected text
mycorpus <- lapply(mycorpus, stemCompletion2, dictionary = mycorpus_copy)
mycorpus <- Corpus(VectorSource(mycorpus))
writeLines(strwrap(mycorpus[[250]]$content, 60))

#Correct mis-spelt words
wordFreq <- function(corpus, word) {
  results <- lapply(corpus, function(x) {grep(as.character(x), pattern = paste0("//<", word))})
  sum(unlist(results))
}

n.tenni <- wordFreq(mycorpus_copy, "tenni")
n.tennis <- wordFreq(mycorpus_copy, "tennis")
cat(n.tenni, n.tennis)

#Replace old words with correct new ones
replaceWords <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub), pattern = oldword, replacement = newword)
}
mycorpus <- replaceWords(mycorpus, "tenni", "tennis")
tdm <- TermDocumentMatrix(mycorpus, control = list(wordLengths = c(1, Inf)))
tdm

idx <- which(dimnames(tdm)$Terms %in% c("usopen", "grandslam"))
as.matrix(tdm[idx, 21:60])

#Find the most frequently used terms
freq_terms <- findFreqTerms(tdm, lowfreq = 25)
freq_terms <- findFreqTerms(tdm, lowfreq = 50)

term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq > 50)
df <- data.frame(term = names(term_freq), freq = term_freq)

ggplot(df, aes(reorder(term, freq), freq)) + geom_bar(stat = "identity") + 
  theme_bw() + coord_flip()

#Find associations
findAssocs(tdm, "usopen", 0.2)

#Topic Modelling to identify hidden/latent topics using LDA
dtm <- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm, 1, sum)

NullDocs <- dtm[rowTotals == 0, ]
dtm <- dtm[rowTotals > 0, ]

lda <- LDA(dtm, k = 5) #find 5 topics
term <- terms(lda, 7) #find 7 terms for every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda)
topics <- data.frame(date = (tweets$created), topic = topics)
qplot(date, ..count.., data = topics, geom = "density", fill = term[topic], position = "stack")

#Use qdap polarity function to detect sentiment
sentiments <- polarity(tweets$text)

sentiments <- data.frame(sentiments$all$polarity)
sentiments[["polarity"]] <- cut(sentiments[["sentiments.all.polarity"]], c(-5, 0.0, 5), 
                                labels = c("negative", "positive"))

table(sentiments$polarity)

#Plot sentiments by date
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(tweets$created)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")

#Create stream graph for sentiment by date
data <- data.frame(sentiments$polarity)
colnames(data)[1] <- "polarity"
data$date <- tweets$created
data$text <- NULL
data$count <- 1

graphdata <- aggregate(count ~ polarity + as.character.Date(date), data = data, FUN = length)
colnames(graphdata)[2] <- "Date"
str(graphdata)
