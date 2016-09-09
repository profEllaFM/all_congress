## Graphing socioeconomic status

coeff <- makeCoeffMatrix(rangeCong)
coeff$sesEst <- NA
coeff$sesSE <- NA
coeff$sesPval <- NA
colSESest <- which(names(coeff)== "sesEst")
colSESpval <- which(names(coeff)== "sesPval")

chooseCoeff <- "ses:partyRepublican"
chooseCoeff <- "ses"
yVarName <- "numPassH"
partyChoice <- 'Democrat'
for(i in 1:length(datasets)){
  print(i)
  congNum <- as.numeric(as.character(rangeCong[i]))
  print(congNum)
  data <- datasets[[i]]
  data <- subset(datasets[[i]], party == partyChoice)
  yvar <- data[ , which(names(data)== yVarName)]
  m <- glm.nb(yvar~ses+ prcntBlack+black+dwnom1+prcntNotEmploy,
              data=data, weights = data$fracServed)
  # m <- glm.nb(yvar~ses*party + prcntBlack+black+dwnom1+numberTerms, 
  #                data=data, weights = data$fracServed)
  vals <- summary(m)$coef
  coeff[i, colSESest:colSESpval] <- c(vals[,"Estimate"][chooseCoeff], vals[,"Std. Error"][chooseCoeff],
                     vals[,"Pr(>|z|)"][chooseCoeff])
  print(coeff[i,])
}


#coeffRepPassHSES <- coeff
coeff<-coeffRepPassHSES 
billType <- "passed house bills"
partyType <- "Republican MCs"

#coeffRepEnactSES <- coeff
coeff<-coeffRepEnactSES 
billType <- "enacted bills"
partyType <- "Republican MCs"

#coeffRepSponSES <- coeff
coeff<-coeffRepSponSES 
billType <- "sponsored bills"
partyType <- "Republican MCs"

#coeffDemPassHSES <- coeff
coeff<-coeffDemPassHSES 
billType <- "passed house bills"
partyType <- "Democratic MCs"

#coeffDemEnactSES <- coeff
coeff<-coeffDemEnactSES 
billType <- "enacted bills"
partyType <- "Democratic MCs"

#coeffDemSponSES <- coeff
coeff<-coeffDemSponSES 
billType <- "sponsored bills"
partyType <- "Democratic MCs"

#coeffInteractSponSES <- coeff
coeff<-coeffInteractSponSES 
billType <- "sponsored bills"
partyType <- ""
###### Graphing###########
setwd( paste(wd, "\\writeup\\ses images", sep="") )
save = T
save= F

p <- ggplot(coeff, aes(rangeCong, sesEst, colour = partyControl)) + geom_line() +
  geom_errorbar(aes(ymin=sesEst - 1.96*sesSE, ymax=sesEst + 1.96*sesSE), width=.1)+
  geom_hline(aes(yintercept=0))+
  scale_color_manual(values=c("blue", "red"),name="party control",
                     breaks=c(0,1),labels=c("Democrat", "Republican"))+
  xlab("Congress number") + ylab("Coefficients for SES on successful legislation")+
  ggtitle(paste("Time Series: SES effect on legislation, ", partyType, sep = ""))+
  theme_bw()+guides(size=FALSE)
if(save == T){
  ggsave( p,filename=paste("ses ", billType, partyType,".jpeg", sep = "") )
}else{
  print(p)
}

getwd()
