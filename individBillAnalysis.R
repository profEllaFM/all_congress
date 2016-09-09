allCongPath <- "C:\\Users\\marciposafm\\Documents\\allCongress"
setwd(allCongPath)


nomStatus <- read.csv(file = file.path(allCongPath,'113\\billNomStatus.csv'))

nomStatus$welfare <- ifelse(nomStatus$clausen == 2, 1, 0)
nomStatus$civil <- ifelse(nomStatus$clausen == 4, 1, 0)
nomStatus$defense <- ifelse(nomStatus$clausen == 5, 1, 0)

sponNom <- aggregate(nomStatus, by=list(nomStatus$sponID), FUN=mean, na.rm=TRUE)

names(nomStatus)

demBills <- subset(nomStatus, party=="Democrat")
repBills <- subset(nomStatus, party=="Republican")
dems <- subset(sponNom, dwnom1<0 & firstDim !=0)
reps <- subset(sponNom, dwnom1>0 & firstDim !=0)

zeroBills <- subset(nomStatus, firstDim == 0)
scoredBills <- subset(nomStatus, firstDim > 0 | firstDim < 0)

m <- lm(firstDim~dwnom1, data=dems)
summary(m)

m <- lm(firstSpread~dwnom1, data=reps)
summary(m)

m <- lm(issue2~dwnom1, data=demBills)
summary(m)

m <- lm(issue2~dwnom1, data= repBills)
summary(m)


plot(firstSpread~dwnom1,  data=dems)
plot(firstSpread~dwnom1,  data=reps)

summary(nomStatus$meanIncome[which(nomStatus$welfare==1)])
summary(nomStatus$meanIncome[which(nomStatus$welfare!=0)])
summary(nomStatus$meanIncome)
t.test(nomStatus$meanIncome[which(nomStatus$welfare==1)], 
       nomStatus$meanIncome)

summary(nomStatus$prcntBlack[which(nomStatus$civil==1)])
summary(nomStatus$prcntBlack)
t.test(nomStatus$prcntBlack[which(nomStatus$civil==1)], 
       nomStatus$prcntBlack)

summary(nomStatus$meanIncome[which(nomStatus$defense==1)])
summary(nomStatus$meanIncome[which(nomStatus$defense!=1)])
summary(nomStatus$meanIncome)
t.test(nomStatus$meanIncome[which(nomStatus$defense==1)], 
       nomStatus$meanIncome)

summary(nomStatus$meanIncome[which(nomStatus$defense==1)])
summary(nomStatus$meanIncome[which(nomStatus$defense!=1)])
summary(nomStatus$meanIncome)
t.test(nomStatus$meanIncome[which(nomStatus$defense==1)], 
       nomStatus$meanIncome[which(nomStatus$welfare==1)])
for(i in 1:6){
  print(summary(nomStatus$prcntBlack[which(nomStatus$clausen > 0)]))
}

