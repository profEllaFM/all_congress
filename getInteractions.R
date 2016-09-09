coeff <- makeCoeffMatrix(rangeCong)
coeff$interactEst <- NA
coeff$interactSE <- NA
coeff$interactPval <- NA
interactCol1 <- which(names(coeff) == "interactEst")
interactCol3 <- which(names(coeff) == "interactPval")

yVarName <- "numPassH"
for(i in 1:length(datasets)){
  print(i)
  congNum <- as.numeric(as.character(rangeCong[i]))
  print(congNum)
  data <- datasets[[i]]
  yvar <- data[ , which(names(data)== yVarName)]
  xvar <- data$over75k+ data$prcntBA + data$prcntBlack+data$dwnom1+data$prcntNotEmploy
  m <- glm.nb(yvar~over75k+ prcntBA + prcntBlack+black+dwnom1+prcntNotEmploy+
                over75k*party + prcntBA*party, 
              data=data, weights = data$fracServed)
  #   m <- glm.nb(yvar~over75k+ prcntBA + prcntBlack*black+dwnom1+prcntNotEmploy, 
  #               data=data, weights = data$fracServed)
  vals <- summary(m)$coef
  coeff[i, 3:5] <- c(vals[,"Estimate"]["over75k:partyRepublican"], 
                     vals[,"Std. Error"]["over75k:partyRepublican"],
                     vals[,"Pr(>|z|)"]["over75k:partyRepublican"])  
  coeff[i, 6:8] <- c(vals[,"Estimate"]["prcntBA:partyRepublican"], 
                     vals[,"Std. Error"]["prcntBA:partyRepublican"],
                     vals[,"Pr(>|z|)"]["prcntBA:partyRepublican"])
  print(coeff[i,])
}

chooseCoeff <- "ses:partyRepublican"
yVarName <- "numPassH"
for(i in 1:length(datasets)){
  print(i)
  congNum <- as.numeric(as.character(rangeCong[i]))
  print(congNum)
  data <- datasets[[i]]
  #data <- subset(datasets[[i]], party == 'Republican')
  #data <- subset(datasets[[i]], party == 'Democrat')
  yvar <- data[ , which(names(data)== yVarName)]
  #   m <- glm.nb(yvar~ses+ prcntBlack+dwnom1+prcntNotEmploy, 
  #               data=data, weights = data$fracServed)
  m <- glm.nb(yvar~ses*party + prcntBlack+black+dwnom1+prcntNotEmploy, 
              data=data, weights = data$fracServed)
  vals <- summary(m)$coef
  coeff[i, colSESest:colSESpval] <- c(vals[,"Estimate"][chooseCoeff], vals[,"Std. Error"][chooseCoeff],
                                      vals[,"Pr(>|z|)"][chooseCoeff])
  print(coeff[i,])
}


printCoefmat(vals)

#coeffInteractPassH <- coeff
coeff <- coeffInteractPassH
billType <- "passed House"

#coeffInteractEnact <- coeff
coeff <- coeffInteractEnact
billType <- "enacted"

#coeffInteractSpon <- coeff
coeff <- coeffInteractSpon
billType <- "sponsored"

#coeffInteractCosp <- coeff
coeff <- coeffInteractCosp
billType <- "cosponsored"


#coeffSESInteractSpon <- coeff
coeff <- coeffSESInteractSpon
billType <- "sponsored"

#coeffSESInteractPassH <- coeff
coeff <- coeffSESInteractPassH
billType <- "passed House"

#coeffSESInteractEnact <- coeff
coeff <- coeffSESInteractEnact
billType <- "enacted"

###### Graphing###########
setwd( paste(wd, "\\writeup\\interaction images", sep="") )
save = T
save = F

p <- ggplot(coeff, aes(rangeCong, over75est, colour = partyControl)) + geom_line() +
      geom_errorbar(aes(ymin=over75est - 1.96*over75se, ymax=over75est + 1.96*over75se), width=.1)+
      geom_hline(aes(yintercept=0))+
      scale_color_manual(values=c("blue", "red"),name="party control",
                         breaks=c(0,1),labels=c("Democrat", "Republican"))+
      xlab("Congress number") + ylab("Republican-Democrat interaction coefficients")+
      ggtitle(paste("Time Series: Wealth and Partisanship", sep = ""))+
      theme_bw()+guides(size=FALSE)
if(save == T){
  ggsave( p,filename=paste("interaction wealth", billType, ".jpeg", sep = "") )
}else{
  print(p)
}

p<-   ggplot(coeff, aes(rangeCong, BAest, colour = partyControl)) + geom_line() +
      geom_errorbar(aes(ymin=BAest - 1.96*BAse, ymax=BAest + 1.96*BAse), width=.1)+
      geom_hline(aes(yintercept=0))+
      scale_color_manual(values=c("blue", "red"),name="party control",
                         breaks=c(0,1),labels=c("Democrat", "Republican"))+
      xlab("Congress number") + ylab("Republican-Democrat interaction coefficients")+
      ggtitle(paste("Time Series: Education and Partisanship", sep = ""))+
      theme_bw()+guides(size=FALSE)
if(save == T){
  ggsave( p,filename=paste("interaction edu", billType, ".jpeg", sep = "") )
}else{
  print(p)
}

p<- ggplot(coeff, aes(rangeCong, interactEst, colour = partyControl)) + geom_line() +
      geom_errorbar(aes(ymin=interactEst - 1.96*interactSE, ymax=interactEst + 1.96*interactSE), width=.1)+
      geom_hline(aes(yintercept=0))+
      scale_color_manual(values=c("blue", "red"),name="party control",
                         breaks=c(0,1),labels=c("Democrat", "Republican"))+
      xlab("Congress number") + ylab("Republican-Democrat interaction coefficients")+
      ggtitle(paste("Time Series: Socioeconomic Status and Partisanship", sep = ""))+
      theme_bw()+guides(size=FALSE)
if(save == T){
  ggsave( p,filename=paste("interaction ses", billType, ".jpeg", sep = "") )
}else{
  print(p)
}
