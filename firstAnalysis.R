subsetByYear <- function(allCongress, rangecong){
  datasets <- vector("list",length(rangeCong)) 
  for(i in 1:length(datasets)){
    congNum <- as.numeric(as.character(rangeCong[i]))
    print(congNum)
    datasets[[i]] <- allCongress[which(allCongress$congNum == congNum), ]
    print(dim(datasets[[i]]))
  }
  datasets
}

#rm(list = ls())
#install.packages("stats")
library(foreign)
library(xtable)
library(MASS)
library(ggplot2)
library(gdata)
library(betareg)
library(car)
library(aod)
library(Hmisc)
library(psych)
library(stats)


wd <- "C:\\Users\\marciposafm\\Documents\\allCongress"
setwd(wd)
source("subsetByYearFunction.R")
allCongress <- read.csv("allCongressData.csv")
allCongress$fracServed[which(allCongress$fracServed < 0) ] <- 0
rangeCong = c(93:97, 103:113)
datasets <- subsetByYear(allCongress, rangeCong)  #prints dimensions of data by congress
varNames <- sort(as.character(names(allCongress)))

tmpData <-datasets[[10]]

m <- glm.nb(numPassH~sesNorm + prcntBlack*black+party*partyControl+dwnom1+
              rankChair +recentArrivalPrcnt+numberTerms +prcntExAliens ,
            data=tmpData, weights = tmpData$fracServed)
printCoefmat(summary(m)$coef)

m <- glm.nb(numPassH~over75k+ prcntBA + prcntBlack*black+party*partyControl+dwnom1+
              rankChair +recentArrivalPrcnt+numberTerms +prcntExAliens ,
            data=tmpData, weights = tmpData$fracServed)
printCoefmat(summary(m)$coef)

m <- glm.nb(numPassH~over75k*congNum*party+ prcntBA*congNum +partyControl+dwnom1+
              numberTerms +prcntBlack+ congNum , data=allCongress)
summary(m)

m <- glm.nb(numPassH~over75k*congNum*party+ partyControl+dwnom1+
              numberTerms +prcntBlack+ congNum , data=allCongress)
summary(m)

m <- glm.nb(numPassH~over75k*congNum*party +prcntBA*congNum + partyControl+
              congNum , data=allCongress)
xtable(m)
summary(m)

m <- glm.nb(numPassH~over75k*congNum*party + partyControl+
              congNum , data=allCongress)
xtable(m)
summary(m)

AIC(m, k=2)


m <- glm.nb(numPassH~sesNorm*congNum*party + partyControl+dwnom1+
              numberTerms +prcntBlack+ congNum , data=allCongress)
m <- glm.nb(numPassH~prcntBA*congNum +partyControl+
              congNum , data=allCongress)
xtable(m)
summary(m)


dems109 <- subset(allCongress, party=='Democrat' & congNum == 109)
dems93 <- subset(allCongress, party=='Democrat' & congNum == 93)
reps109 <- subset(allCongress, party=='Republican' & congNum == 109)
reps93 <- subset(allCongress, party=='Republican' & congNum == 93)

m <- glm.nb(numPassH~over75k + prcntBA+ prcntBlack+black+dwnom1+numberTerms  , data=dems109)
m <- glm.nb(numPassH~over75k + prcntBA+ prcntBlack+black+dwnom1+numberTerms  , data=dems93)
m <- glm.nb(numPassH~over75k + prcntBA+ prcntBlack+dwnom1+numberTerms  , data=reps109)
m <- glm.nb(numPassH~over75k + prcntBA+ prcntBlack+dwnom1+numberTerms  , data=reps93)

m <- glm.nb(numPassH~over75k + prcntBA+ prcntBlack+black+dwnom1+numberTerms  , data=dems109)
m <- glm.nb(numPassH~over75k + prcntBA+ prcntBlack+black+dwnom1+numberTerms  , data=dems93)
m <- glm.nb(numPassH~over75k + prcntBA+ prcntBlack+dwnom1+numberTerms  , data=reps109)
m <- glm.nb(numPassH~over75k + prcntBA+ prcntBlack+dwnom1+numberTerms  , data=reps93)

m <- glm.nb(numPassH~sesNorm + prcntBlack+dwnom1+numberTerms  , data=reps109)
m <- glm.nb(numPassH~sesNorm + prcntBlack+dwnom1+numberTerms  , data=reps93)
m <- glm.nb(numPassH~sesNorm + prcntBlack+dwnom1+numberTerms  , data=dems109)
m <- glm.nb(numPassH~sesNorm + prcntBlack+dwnom1+numberTerms  , data=dems93)

m <- glm.nb(numPassH~over75k + prcntBlack+black+dwnom1+numberTerms  , data=dems109)
m <- glm.nb(numPassH~over75k + prcntBlack+black+dwnom1+numberTerms  , data=dems93)
m <- glm.nb(numPassH~over75k + prcntBlack+dwnom1+numberTerms  , data=reps109)
m <- glm.nb(numPassH~over75k + prcntBlack+dwnom1+numberTerms  , data=reps93)
xtable(m)
summary(m)


###########OVERALL############



############CONSERVATIVENESS############################

repCongRepRep <- subset(allCongress, party == 'Republican' & partyControl == 'R')
demCongDemRep <- subset(allCongress, party == 'Democrat' & congNum < 95)


m <- glm(numPassH~over75k+ prcntBlack+numberTerms + dwnom1, data=repCongRepRep, weights = repCongRepRep$fracServed)
summary(m)

m <- glm(numPassH~over75k+ prcntBlack+numberTerms + dwnom1, data=demCongDemRep, weights = demCongDemRep$fracServed)
summary(m)

m <- glm(dwnom1~over75k+ prcntBA + prcntBlack+partyControl+
              numberTerms + prcntExAliens, data=allCongress, weights = allCongress$fracServed)
summary(m)

m <- glm(dwnom1~over75k+ prcntBA + prcntBlack+partyControl+
          numberTerms + prcntExAliens, data=repReps, weights = repReps$fracServed)
summary(m)

m <- glm(dwnom1~over200k+ prcntBA + prcntBlack+partyControl+
           numberTerms + prcntExAliens, data=allCongress, weights = allCongress$fracServed)
summary(m)

m <- glm(dwnom1~over200k+ prcntBA + prcntBlack+partyControl+
           numberTerms + prcntExAliens, data=demReps, weights = demReps$fracServed)
summary(m)

m <- glm(dwnom1~over200k+ prcntBA + prcntBlack+partyControl+
           numberTerms + prcntExAliens, data=repReps, weights = repReps$fracServed)
summary(m)

m <- glm(dwnom1~gini+ prcntBA + prcntBlack+partyControl+
           numberTerms + prcntExAliens, data=demReps, weights = demReps$fracServed)
summary(m)

m <- glm(dwnom1~gini+ prcntBA + prcntBlack+partyControl+
           numberTerms + prcntExAliens, data=repReps, weights = repReps$fracServed)
summary(m)

plot(dwnom1~gini, data = demReps)
plot(dwnom1~gini, data = repReps)
plot(over200k~gini, data = demReps)
plot(over200k~gini, data = repReps)
plot(over75k~gini, data = demReps)
plot(over75k~gini, data = repReps)
plot(dwnom1~over200k, data = demReps)
plot(dwnom1~over200k, data = repReps)
plot(dwnom1~over75k, data = demReps)
plot(dwnom1~over75k, data = repReps)
plot(prcntBlack~prcntHisp, data = allCongress)
plot(prcntBlack~prcntHisp, data = demReps)
plot(prcntBlack~prcntHisp, data = repReps)
