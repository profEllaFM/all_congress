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

allCongPath <- "C:\\Users\\marciposafm\\Documents\\allCongress"
setwd(allCongPath)


nomStatus <- read.csv(file = file.path(allCongPath,'113\\billNomStatus.csv'))
nomStatus$billName <- gsub("\\<act\\>"," ", nomStatus$billName, ignore.case=TRUE ) 
nomStatus$billName <- gsub("\\<2013\\>"," ", nomStatus$billName, ignore.case=TRUE ) 
nomStatus$billName <- gsub("\\<2014\\>"," ", nomStatus$billName, ignore.case=TRUE ) 
nomStatus$description <- gsub("\\<act\\>"," ", nomStatus$description, ignore.case=TRUE ) 
nomStatus$description <- gsub("\\<2013\\>"," ", nomStatus$description, ignore.case=TRUE ) 
nomStatus$description <- gsub("\\<2014\\>"," ", nomStatus$description, ignore.case=TRUE ) 

titleCorpus <- Corpus(VectorSource(nomStatus$description))
titleCorpus <- Corpus(VectorSource(nomStatus$billName))
titleCorpus <- cleaned(titleCorpus)  #so I put those lines into a function. Now I call 1 line, instead of4
wordcloud(titleCorpus, max.words = 100, random.order = FALSE)

#compare democrat and republican bills
richDem <- subset(nomStatus, party=="Democrat" & over75k > 43)
poorDem <- subset(nomStatus, party=="Democrat" & over75k < 28)
richRep <- subset(nomStatus, party=="Republican" & over75k > 43)
poorRep <- subset(nomStatus, party=="Republican" & over75k < 28)

#Comparing rich and poor
rich <- richDem
poor <- poorDem
rich <- richRep
poor <- poorRep
poorTitlesCorp <- Corpus(VectorSource(paste(poor$description,collapse=" ") ))
poorTitlesCorp <- cleaned(poorTitlesCorp)
richTitlesCorp <- Corpus(VectorSource(paste(rich$description,collapse=" ") ))
richTitlesCorp <- cleaned(richTitlesCorp)

compareWealth <- c(poorTitlesCorp, richTitlesCorp)
termDocMatrixComp = TermDocumentMatrix(compareWealth)
termDocMatrixComp = as.matrix(termDocMatrixComp)
colnames(termDocMatrixComp) <- c("low SES ","high SES")

comparison.cloud(termDocMatrixComp,max.words=150,random.order=FALSE)
commonality.cloud(termDocMatrixComp,random.order=FALSE)
if(save == T){
  ggsave(p,filename=paste(varDescrip, billType, partyType,"minusEdu.jpeg", sep = ""))
}else{
  print(p)
}
