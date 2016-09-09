makeCoeffMatrix <- function(rangeCong){
  coeff <- as.data.frame(rangeCong)
  coeff$partyControl <- c('D','D','D','D','D','D',
                          'R','R','R','R','R','R',
                          'D','D','R','R')
  coeff$over75est <- NA
  coeff$over75se <- NA
  coeff$over75pval <- NA
  coeff$BAest <- NA
  coeff$BAse <- NA
  coeff$BApval <- NA
  coeff$prcntBlackEst <- NA
  coeff$prcntBlackSe <- NA
  coeff$prcntBlackPval <- NA
  coeff$whPrcntBlackEst <- NA
  coeff$whPrcntBlackSe <- NA
  coeff$whPrcntBlackPval <- NA
  coeff$BlPrcntBlackEst <- NA
  coeff$BlPrcntBlackSe <- NA
  coeff$BlPrcntBlackPval <- NA
  coeff
} #makes blank matrix

fillCoeff <- function(yVarName, partyChoice, datasets, rangeCong, coeff){
  for(i in 1:length(datasets)){
    #print(i)
    congNum <- as.numeric(as.character(rangeCong[i]))
    #print(congNum)
    data <- subset(datasets[[i]], party == partyChoice)
    yvar <- data[ , which(names(data)== yVarName)]
    if(partyChoice == 'Democrat'){
      m <- glm.nb(yvar~over75k + prcntBlack+black+dwnom1+ numberTerms, 
                  data=data, weights = data$fracServed)
      printCoefmat(summary(m)$coef)
    }else{
      m <- glm.nb(yvar~over75k + prcntBlack+dwnom1+ numberTerms , 
                  data=data, weights = data$fracServed)
      printCoefmat(summary(m)$coef)
    }
    vals <- summary(m)$coef
    coeff[i, 3:11] <- c(vals[,"Estimate"]["over75k"], vals[,"Std. Error"]["over75k" ],
                       vals[,"Pr(>|z|)"]["over75k"],vals[,"Estimate"]["prcntBA"], vals[,"Std. Error"]["prcntBA"],
                       vals[,"Pr(>|z|)"]["prcntBA"], vals[,"Estimate"]["prcntBlack"], vals[,"Std. Error"]["prcntBlack"],
                        vals[,"Pr(>|z|)"]["prcntBlack"])
    #print(coeff[i,])
  }
  if(partyChoice == 'Democrat'){
    coeff <- fillCoeffRace(yVarName, partyChoice, datasets, rangeCong, coeff)
  }
  coeff
}

fillCoeffRace <- function(yVarName, partyChoice, datasets, rangeCong, coeff){
  for(i in 2:length(datasets)){
    #for(i in 1:length(datasets)){
    #print(i)
    congNum <- as.numeric(as.character(rangeCong[i]))
    #print(congNum)
    data <- subset(datasets[[i]], party == 'Democrat' & black ==1)
    yvar <- data[ , which(names(data)== yVarName)]
    m <- glm.nb(yvar~over75k+ prcntBA + prcntBlack+dwnom1+numberTerms , 
                data=data, weights = data$fracServed)
    vals <- summary(m)$coef
    coeff[i, 15:17] <- c(vals[,"Estimate"]["prcntBlack"], vals[,"Std. Error"]["prcntBlack"],
                         vals[,"Pr(>|z|)"]["prcntBlack"])
    data <- subset(datasets[[i]], party == 'Democrat' & black ==0 & hispanic == 0)
    yvar <- data[ , which(names(data)== yVarName)]
    m <- glm.nb(yvar~over75k+ prcntBA + prcntBlack+dwnom1+numberTerms , 
                data=data, weights = data$fracServed)
    vals <- summary(m)$coef
    coeff[i, 12:14] <- c(vals[,"Estimate"]["prcntBlack"], vals[,"Std. Error"]["prcntBlack"],
                        vals[,"Pr(>|z|)"]["prcntBlack"])
    #print(coeff[i,])
  }  #only use for democratic representatives
  coeff
} #Democrats only
  #calls fillCoeff  

plotTime <- function(coeff, rangeCong, varEst, varSE, billType, partyType, varDescrip, save){
  p <- ggplot(coeff, aes(rangeCong, varEst, colour = partyControl)) + geom_line() +
    geom_errorbar(aes(ymin=varEst - 1.96*varSE, ymax=varEst + 1.96*varSE), width=.1)+
    geom_hline(aes(yintercept=0))+
    scale_color_manual(values=c("blue", "red"),name="party control",
                       breaks=c(0,1),labels=c("Democrat", "Republican"))+
    xlab("Congress number") + ylab(paste("Coefficient for wealth without education", sep ="") )+
    ggtitle(paste("Time Series: Wealth's Effect on Legislation, ", partyType, sep = ""))+
    theme(axis.title.x = element_text(colour=1, size=axisTitleX),
          axis.text.x= element_text(size=axisTextX),
          axis.title.y = element_text(angle=90,size=200),
          axis.text.y = element_text(angle=90,size=1000))+
    theme_bw()
  if(save == T){
    ggsave(p,filename=paste(varDescrip, billType, partyType,"minusEdu.jpeg", sep = ""))
  }else{
    print(p)
  }
} #plotting one variable against time

plotVars <- function(coeff, rangeCong, billType, partyType, varDescrip, save){
  varEst <- coeff[, which(names(coeff) == 'over75est')]
  varSE <- coeff[, which(names(coeff) == 'over75se')]
  varDescrip <- 'Wealth'
  plotTime(coeff, rangeCong, varEst, varSE, billType, partyType, varDescrip, save)

  # varEst <- coeff[, which(names(coeff) == 'BAest')]
  # varSE <- coeff[, which(names(coeff) == 'BAse')]
  # varDescrip <- 'Education '
  # plotTime(coeff, rangeCong, varEst, varSE, billType, partyType, varDescrip, save)

  # varEst <- coeff[, which(names(coeff) == 'prcntBlackEst')]
  # varSE <- coeff[, which(names(coeff) == 'prcntBlackSe')]
  # varDescrip <- 'XXRace '
  # plotTime(coeff, rangeCong, varEst, varSE, billType, partyType, varDescrip, save)
  # 
  # if(partyType == "Democrat MCs"){
  #   print(partyType)
  #   varEst <- coeff[, which(names(coeff) == 'BlPrcntBlackEst')]
  #   varSE <- coeff[, which(names(coeff) == 'BlPrcntBlackSe')]
  #   varDescrip <- 'XXBlack MC Race '
  #   plotTime(coeff, rangeCong, varEst, varSE, billType, partyType, varDescrip, save)
  # 
  #   varEst <- coeff[, which(names(coeff) == 'whPrcntBlackEst')]
  #   varSE <- coeff[, which(names(coeff) == 'whPrcntBlackSe')]
  #   varDescrip <- 'XXWhite MC Race '
  #   plotTime(coeff, rangeCong, varEst, varSE, billType, partyType, varDescrip, save)
  # }
}  # plotting each variable across time
    #calls plotTime

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

wd <- "C:\\Users\\marciposafm\\Documents\\allCongress"
setwd(wd)
source("subsetByYearFunction.R")
allCongress <- read.csv("allCongressData.csv")
allCongress$fracServed[which(allCongress$fracServed < 0) ] <- 0
rangeCong = c(93:97, 103:113)
datasets <- subsetByYear(allCongress, rangeCong)  #prints dimensions of data by congress


m <- glm.nb(numPassH~over75k+ prcntBA + prcntBlack+dwnom1+prcntNotEmploy+
              over75k*party+ prcntBA*party + prcntBlack*party+dwnom1*party+prcntNotEmploy*party, 
            data=repCong, weights = repCong$fracServed)
printCoefmat(summary(m)$coef)

m <- glm.nb(numPassH~over75k*party, 
            data=repCong, weights = repCong$fracServed)
printCoefmat(summary(m)$coef)

##Cycled through 4 repCongDemRep sets, and 5 outcomes.. not passPrcnt
m <- glm(dwnom1~over75k+ prcntBA, 
         data=repCongDemRep, weights = repCongDemRep$fracServed)
summary(m)

m <- glm.nb(numEnact~over75k+ prcntBA + prcntBlack*black+dwnom1+
              rankChair +recentArrivalPrcnt+ prcntExAliens + numberTerms, 
            data=repCongDemRep, weights = repCongRepRep$fracServed)
summary(m)

m <- glm.nb(numPassH~over75k+ prcntBA + prcntBlack+dwnom1+
              rankChair +recentArrivalPrcnt+ prcntExAliens + numberTerms, data=repCongRepRep)
summary(m)


coeff <- makeCoeffMatrix(rangeCong)

#  coeffRepsEnacted<- coeff


###### Graphing###########
setwd( paste(wd, "\\writeup", sep="") )
axisTitleX = 100
axisTitleY = 100
axisTextX = 30
legendTitle = 30
legendText = 20

billVars = c('numPassH', 'numEnact','numSpon')
billHash <- as.data.frame(billVars)
billHash$billTypes <- c('passed house bills', 'enacted bills','sponsored bills')

billVars = c('numPassH')
billHash <- as.data.frame(billVars)
billHash$billTypes <- c('passed house bills')

for(i in 1:dim(billHash)[1]){
  billVar <- billHash$billVars[i]
  billType <- billHash$billTypes[i]
  print(paste(billVar, billType, sep=" "))
  for(partyChoice in list("Democrat", "Republican")){
    coeff <- makeCoeffMatrix(rangeCong)
    coeff <- fillCoeff(billVar, partyChoice, datasets, rangeCong, coeff)
    partyType <- paste(partyChoice, " MCs", sep="")
    plotVars(coeff, rangeCong, billType, partyType, varDescrip, save = F)
  }  
}


