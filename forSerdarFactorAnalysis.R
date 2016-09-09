

extractSES <- function(tmpData){
  #extract a socioeconomic score from college and rich
  sesData <-subset(tmpData, select = c(over75k, prcntBA))
  fit <- princomp(na.omit(sesData), cor=TRUE)
  summary(fit) # print variance accounted for 
  summary(sesData)
  loadings(fit) # pc loadings 
  plot(fit,type="lines") # scree plot
  biplot(fit)
  fa <- fa(sesData)
  ses <- factor.scores(sesData, fa)$scores
  ses <- ses[,1]
  tmpData$ses <- ses
  tmpData$sesNorm <- (ses-(min(ses, na.rm=T)))*100/(max(ses, na.rm=T)-min(ses, na.rm=T)) #create a scale from 0 to 100 to represents percentages
  tmpData
}  

#rm(list = ls())
library(foreign)
library(xtable)
library(ggplot2)
library(gdata)
library(car)
library(aod)
library(Hmisc)
library(psych)

wd <- "C:\\Users\\marciposafm\\Documents\\allCongress"
setwd(wd)
allCongress <- read.csv("allCongressData.csv")
allCongress$fracServed[which(allCongress$fracServed < 0) ] <- 0

varNames <- sort(as.character(names(allCongress)))