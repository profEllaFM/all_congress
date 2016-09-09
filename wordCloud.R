#install.packages("SnowballC")
#install.packages("wordcloud")

cleaned <- function(corpus){
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stemDocument)
  corpus
}  #function because we do this a lot over and over


library(tm)
library(SnowballC)
library(wordcloud)
library(foreign)   
library(RCurl) 

url = "https://raw.githubusercontent.com/profEllaFM/congress2013/master/billNomStatus.csv"
rawData = getURL(url)  
bills113 = read.csv(textConnection(rawData))

titleCorpus <- Corpus(VectorSource(bills113$billName))
titleCorpus <- tm_map(titleCorpus, PlainTextDocument)
titleCorpus <- tm_map(titleCorpus, removePunctuation)
titleCorpus <- tm_map(titleCorpus, removeWords, stopwords('english'))
titleCorpus <- tm_map(titleCorpus, stemDocument)
wordcloud(titleCorpus, max.words = 100, random.order = FALSE)

#remove act, 2013, and 2014
titleCorpus <- tm_map(titleCorpus, removeWords, c('act', '2013', '2014', stopwords('english')))
wordcloud(titleCorpus, max.words = 100, random.order = FALSE)

#Why is "act" still there? Let's get rid of it another way
#create the actual matrix of terms that the word cloud is drawing from
termDocMatrix <- TermDocumentMatrix(titleCorpus)
matrixTDM <- as.matrix(termDocMatrix)
vectorTDM <- sort(rowSums(matrixTDM),decreasing=TRUE)
wordDF <- data.frame(word = names(vectorTDM),freq=vectorTDM)
View(wordDF)
wordDF <- wordDF[-1, ]

wordcloud(wordDF$word, wordDF$freq, max.words = 100, random.order = FALSE)

#ok, but now we want to get rid of the offending terms from a column overall
#REGULAR EXPRESSIONS come in handy
#useful cheat sheet: https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/ 
bills113$billName <- gsub("act "," ", bills113$billName, ignore.case=TRUE ) 
bills113$billName <- gsub(" act"," ", bills113$billName, ignore.case=TRUE ) #not perfect, but preserving "practice" type words
#basic substitutions
tmpWd <- 'practice act actor enact factor nothing'
help(gsub)
gsub("act ","-- ",tmpWd, ignore.case=TRUE )
gsub(" act"," --",tmpWd, ignore.case=TRUE )
gsub("act","--",tmpWd, ignore.case=TRUE )
#remove some numbers
tmpWd <- '1003 2013 regular expressions'
tmpWd
gsub("201\\d","",tmpWd, ignore.case=TRUE )
gsub("\\d","",tmpWd, ignore.case=TRUE )
gsub(' \\d+'," ",tmpWd, ignore.case=TRUE )
bills113$billName <- gsub("201\\d","", bills113$billName, ignore.case=TRUE )


#compare democrat and republican bills
demBills <- subset(bills113, party=="Democrat")
repBills <- subset(bills113, party=="Republican")

titleCorpusD <- Corpus(VectorSource(demBills$billName))
titleCorpusD <- tm_map(titleCorpusD, PlainTextDocument)
titleCorpusD <- tm_map(titleCorpusD, removePunctuation)
titleCorpusD <- tm_map(titleCorpusD, removeWords, stopwords('english'))
titleCorpusD <- tm_map(titleCorpusD, stemDocument)  #we've done this a lot. It's cluttering our code
wordcloud(titleCorpusD, max.words = 100, random.order = FALSE)
wordcloud(titleCorpusD, max.words = 10, random.order = FALSE)

titleCorpusR <- Corpus(VectorSource(repBills$billName))
titleCorpusR <- cleaned(titleCorpusR)  #so I put those lines into a function. Now I call 1 line, instead of4
wordcloud(titleCorpusR, max.words = 100, random.order = FALSE)
wordcloud(titleCorpusR, max.words = 10,  random.order = FALSE)


#Comparing democrats and republicans
demTitlesCorp <- Corpus(VectorSource(paste(demBills$billName,collapse=" ") ))
demTitlesCorp <- cleaned(demTitlesCorp)
repTitlesCorp <- Corpus(VectorSource(paste(repBills$billName,collapse=" ") ))
repTitlesCorp <- cleaned(repTitlesCorp)

compareParty <- c(demTitlesCorp, repTitlesCorp)
termDocMatrixComp = TermDocumentMatrix(compareParty)
termDocMatrixComp = as.matrix(termDocMatrixComp)
colnames(termDocMatrixComp) <- c("Democrats","Republicans")

comparison.cloud(termDocMatrixComp,max.words=300,random.order=FALSE)
commonality.cloud(termDocMatrixComp,random.order=FALSE)

