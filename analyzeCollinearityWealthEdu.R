###uses getTimeCoeff.r
# this one focuses on impact on conservativism


###########SES vs wealth and income

summary(allCongress$over50k)
wealthyDemDists <- subset(allCongress, over75k > 30 & over75k < 36 & party == 'Democrat')
m <- glm(dwnom1~over75k + prcntBA + prcntBlack,  data=wealthyDemDists, weights = wealthyDemDists$fracServed)
printCoefmat(summary(m)$coef)

middleDemDists <- subset(allCongress, over50k > 50 & over75k < 56 & party == 'Democrat')
m <- glm(dwnom1~over75k + prcntBA + prcntBlack,  data=middleDemDists, weights = middleDemDists$fracServed)
printCoefmat(summary(m)$coef)

poorDemDists <- subset(allCongress, under10k > 12 & under10k < 16 & party == 'Democrat')
m <- glm(dwnom1~over75k +prcntBA + prcntBlack,  data=poorDemDists, weights = poorDemDists$fracServed)
printCoefmat(summary(m)$coef)

demRep <- subset(allCongress,  party == 'Democrat')
m <- glm(dwnom1~over75k +prcntBA + prcntBlack,  data=demRep, weights = demRep$fracServed)
xtable(summary(m)$coef)

repRep <- subset(allCongress,  party == 'Republican')
m <- glm(dwnom1~over75k +prcntBA + prcntBlack,  data=repRep, weights = repRep$fracServed)
xtable(summary(m)$coef)

##don't know what to make of this. Education makes democrats more liberal for any slice of income
# but it makes democrats more conservative for the full spectrum of incomes

coeff <- makeCoeffMatrix(rangeCong)
coeff$sesEst <- NA
coeff$sesSe <- NA
coeff$sesPval <- NA

yvarName <- "dwnom1"
partyChoice <- 'Republican'
for(i in 1:length(datasets)){
  print(i)
  congNum <- as.numeric(as.character(rangeCong[i]))
  print(congNum)
  data <- subset(datasets[[i]], party == partyChoice)
  yvar <- data[ , which(names(data)== yvarName)]
  m <- glm(yvar~over75k+ prcntBA + prcntBlack,  data=data, weights = data$fracServed)
  vals <- summary(m)$coef
  coeff[i, 3:5] <- c(vals[,"Estimate"]["over75k"], vals[,"Std. Error"]["over75k"],
                     vals[,"Pr(>|t|)"]["over75k"])
  coeff[i, 6:8] <- c(vals[,"Estimate"]["prcntBA"], vals[,"Std. Error"]["prcntBA"],
                     vals[,"Pr(>|t|)"]["prcntBA"])
  coeff[i, 9:11] <- c(vals[,"Estimate"]["prcntBlack"], vals[,"Std. Error"]["prcntBlack"],
                      vals[,"Pr(>|t|)"]["prcntBlack"])
  yvar <- data[ , which(names(data)== yvarName)]
  m <- glm(yvar~sesNorm + prcntBlack,  data=data, weights = data$fracServed)
  vals <- summary(m)$coef
  coeff[i, 18:20] <- c(vals[,"Estimate"]["sesNorm"], vals[,"Std. Error"]["sesNorm"],
                      vals[,"Pr(>|t|)"]["sesNorm"])
  print(coeff[i,])
}



# repNomCoeff <- coeff
# demBlackNomCoeff <- coeff
# demWhiteNomCoeff <- coeff

#repNom2Coeff <- coeff
#demBlackNom2Coeff <- coeff
#demWhiteNom2Coeff <- coeff


# coeffDemsEnacted <- coeff
coeff <- repNomCoeff
dimNom <- "first dimension "
partyType <- "Republican MCs"

partyType <- "Democratic MCs"

#  coeffRepsEnacted<- coeff
coeff <- demBlackNomCoeff
dimNom <- "first dimension"
partyType <- "black Democratic MCs"

# coeffDemPassH <- coeff
coeff <- demWhiteNomCoeff
dimNom <- "first dimension"
partyType <- "white Democratic MCs"

#coeffRepPassH <- coeff
dimNom <- "passed house bills"
partyType <- "Republican MCs"

###### Graphing###########
setwd( paste(wd, "\\writeup\\conservativeness images", sep="") )
save = T

if(save == T){
  fileName <- paste("wealth dwnominate ", dimNom, partyType,".jpeg", sep = "")
  jpeg(fileName,width = 980, height = 480)
}
ggplot(coeff, aes(rangeCong, over75est, colour = partyControl)) + geom_line() +
  geom_errorbar(aes(ymin=over75est - 1.96*over75se, ymax=over75est + 1.96*over75se), width=.1)+
  geom_hline(aes(yintercept=0))+
  scale_color_manual(values=c("blue", "red"),name="party control",
                     breaks=c(0,1),labels=c("Democrat", "Republican"))+
  xlab("congress number") + ylab("Coeff for wealthy on ideology")+
  ggtitle(paste("Time Series: Wealth's effect on ", dimNom, ", ", partyType, sep = ""))+
  theme_bw()+guides(size=FALSE)
if(save == T){
  dev.off()
}
if(save == T){
  fileName <- paste("education ", dimNom, partyType,".jpeg", sep = "")
  jpeg(fileName,width = 980, height = 480)
}
ggplot(coeff, aes(rangeCong, BAest, colour = partyControl)) + geom_line() +
  geom_errorbar(aes(ymin=BAest - 1.96*BAse, ymax=BAest + 1.96*BAse), width=.1)+
  geom_hline(aes(yintercept=0))+
  scale_color_manual(values=c("blue", "red"),name="party control",
                     breaks=c(0,1),labels=c("Democrat", "Republican"))+
  xlab("congress number") + ylab("Coeff for education on ideology")+
  ggtitle(paste("Time Series: Educations's effect on ", dimNom, ", ", partyType, sep = ""))+
  theme_bw()+guides(size=FALSE)
if(save == T){
  dev.off()
}

p <- ggplot(coeff, aes(rangeCong, sesEst, colour = partyControl)) + geom_line() +
      geom_errorbar(aes(ymin=sesEst - 1.96*sesSe, ymax=sesEst + 1.96*sesSe), width=.1)+
      geom_hline(aes(yintercept=0))+
      scale_color_manual(values=c("blue", "red"),name="party control",
                         breaks=c(0,1),labels=c("Democrat", "Republican"))+
      xlab("congress number") + ylab("Coeff for ses on ideology")+
      ggtitle(paste("Time Series: SES effect on ", dimNom, ", ", partyType, sep = ""))+
      theme_bw()+guides(size=FALSE)
if(save == T){
  ggsave(p,filename=paste("ses ", dimNom, partyType,".jpeg", sep = ""))
}

if(save == T){
  fileName <- paste("black pop ", dimNom, partyType,".jpeg", sep = "")
  jpeg(fileName,width = 980, height = 480)
}
ggplot(coeff, aes(rangeCong, prcntBlackEst, colour = partyControl)) + geom_line() +
  geom_errorbar(aes(ymin=prcntBlackEst - 1.96*prcntBlackSe, ymax=prcntBlackEst + 1.96*prcntBlackSe), width=.1)+
  geom_hline(aes(yintercept=0))+
  scale_color_manual(values=c("blue", "red"),name="party control",
                     breaks=c(0,1),labels=c("Democrat", "Republican"))+
  xlab("congress number") + ylab("Coeff for black pop on ideology")+
  ggtitle(paste("Time Series: Black pop effect on ", dimNom, ", ", partyType, sep = ""))+
  theme_bw()+guides(size=FALSE)
if(save == T){
  dev.off()
}

setwd( paste(wd, "\\writeup\\collinTime images", sep="") )
save = T

files <- paste("collin", rangeCong, "EduIncome.jpeg",sep="")
for(i in 1:length(rangeCong)){
  congNum = rangeCong[i]
  print(congNum)
  if(save == T){
    fileName <- files[i]
    jpeg(fileName,width = 980, height = 480)
  }
  data <- datasets[[i]]
  print(mean(data$prcntBA, na.rm=T))
  print(ggplot(data, aes(prcntBA, over75k, group = party, colour = party)) + geom_smooth(method = lm)+
    geom_point()+ xlim(0,65)+ylim(0,65)+
    xlab("percent college degrees") + ylab("percent over $75k")+
    scale_color_manual(values=c("blue", "red", "red"),name="party",
                       breaks=c(0,1,2),labels=c("Democrat", "Independent", "Republican"))+
    theme_bw()+guides(size=FALSE) )
  if(save == T){
    dev.off()
  }
}

for(i in 1:length(rangeCong)){
  congNum = rangeCong[i]
  print(congNum)
  if(save == T){
    fileName <- files[i]
    jpeg(fileName,width = 980, height = 480)
  }
  data <- datasets[[i]]
  data <- subset(data, party =="Republican")
  print(sum(data$hispanic))
  print(mean(data$prcntBA, na.rm=T))
  print(hist(data$prcntHS) )
  if(save == T){
    dev.off()
  }
}