###################
#Analyzing all bills and censuses
#
#Ella Foster-Molina    2/2016
#####################
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

fixNominateCSV <- function(nomBillRCOne, fixBillRC){  
  nomBillRCOne <- subset(nomBillRCOne, grepl("H R ", nomBillRCOne$bill, fixed = TRUE))
  nomBillRCOne$bill <- droplevels(nomBillRCOne$bill)
  nomBillRCOne$question <- droplevels(nomBillRCOne$question)
  nomBillRCOne$result <- droplevels(nomBillRCOne$result)
  nomBillRCOne$description <- droplevels(nomBillRCOne$description)
  nomBillRCOne$bill <- as.numeric(gsub("H R ", "", nomBillRCOne$bill))
  nomBillRCOne <- fixRCNum(fixBillRC$correction1st, fixBillRC$correction2nd, nomBillRCOne)
  nomBillRCOne <- nomBillRCOne[order(nomBillRCOne$bill),]
  nomBillRCOne
} #cleans nominate bill num roll call num file, find correct cong before hand?

fixRCNum <- function(correction1st, correction2nd, nomBillRCOne){
  nomBillRCOne$rcnum <- NA
  nomBillRCOne$rcnum[which(nomBillRCOne$session=="1st") ] <- nomBillRCOne$number[which(nomBillRCOne$session=="1st") ] +correction1st 
  nomBillRCOne$rcnum[which(nomBillRCOne$session=="2nd") ] <- nomBillRCOne$number[which(nomBillRCOne$session=="2nd") ] + correction2nd
  nomBillRCOne$number <- nomBillRCOne$rcnum
  nomBillRCOne$rcnum <- NULL
  names(nomBillRCOne)[names(nomBillRCOne)== "number"] <- 'rcnum'
  nomBillRCOne
} #used by fixNominateCSV, adjusts roll call numbers


createFinalAction <- function(nomBillRCOne){
  finalAction <- nomBillRCOne[which(is.na(nomBillRCOne$bill)),] #empty dataframe with col names
  oldBill <- nomBillRCOne$bill[1]
  for(row in 1:nrow(nomBillRCOne)){
    newBill <- nomBillRCOne$bill[row]
    if(newBill != oldBill){
      finalAction <- rbind(finalAction, nomBillRCOne[row,])
    }
    oldBill <- newBill
  }
  finalAction
} #just looks at last vote of each bill


#rm(list = ls())
library(foreign)
library(xtable)
library(MASS)
library(base)
library(reldist)
library(stringr)

allCongPath <- "C:\\Users\\marciposafm\\Documents\\allCongress" 
nominate <- read.dta(file.path(allCongPath,'nominateScores.DTA'))
allCongress <- read.csv(file.path(allCongPath,"allCongressData.csv"))
allCongress$fracServed[which(allCongress$fracServed < 0) ] <- 0
rangeCong = c(110:113)
datasets <- subsetByYear(allCongress, rangeCong)  #prints dimensions of data by congress
oneCong <- datasets[[which(rangeCong == 112)]]

congNum = 112
#allStatus <- read.csv(file.path(allCongPath,'113\\allStatusMatrix.txt'), sep= '\t', header=T)
#billStatus <- createBillStatus(allStatus) #all bills in 113th cong, final action, title, sponIDs
        #takes time, only do once
#write.csv(billStatus, file = file.path(allCongPath,'113\\billStatus.csv'))
billStatus <- read.csv(file.path(allCongPath,congNum,'billStatus.csv', fsep = '\\'), header=T)

nominatePath <- "C:\\Users\\marciposafm\\Documents\\allCongress\\nominateBills" 
fixBillRC <- read.csv(file.path(nominatePath, "fixBillRC.csv"), header=T)
nominateIssuesAll <- read.dta(file.path(nominatePath,'nominateIssuesAllCong.DTA'))  #has issue scores, yeas nays, decriptions
nominateBillScores <- read.table(file.path(nominatePath,'rollCallIdeology.DAT'))   #has ideology scores, yeas nays, descriptions
names(nominateBillScores) <- c("cong", "rcnum", "firstSpread","firstDim","secondSpread","secondDim")

nominateIndividPath <- "C:\\Users\\marciposafm\\Documents\\allCongress\\nominateBills\\roll_call_dictionaries" 
nomBillRCOne <- read.csv(file.path(nominateIndividPath,paste('h',congNum, 'desc.csv', sep=''), fsep = '\\'))  #used to match bill number to roll call number
nomBillRCOne <- fixNominateCSV(nomBillRCOne, fixBillRC[which(fixBillRC$cong==congNum), ])
nomBillRCOne <- nomBillRCOne[ order(nomBillRCOne$session, nomBillRCOne$rcnum), ]

finalAction <- createFinalAction(nomBillRCOne)
rm(nomBillRCOne)
nomAll <- merge(nominateBillScores,nominateIssuesAll, by = c("cong","rcnum"))
nom113 <- subset(nomAll, cong==congNum)
nom113 <- nom113[ order(nom113$rcnum), ]
finalActionAll <- merge(finalAction, nom113, by = "rcnum", all.x=T)
finalActionAll <- finalActionAll[ order(finalActionAll$session, finalActionAll$rcnum), ]

onlyVotesMine <- subset(billStatus, action != "REFERRED" & action != "REPORTED")
discrepancies <- merge(onlyVotesMine, finalAction, by.x="billNo", by.y="bill", all=T)
rm(onlyVotesMine)
rm(finalAction)
discrepancies <- subset(discrepancies, select = c(action, billNo, rcnum, billName))
discrepancies <- discrepancies[order(discrepancies$rcnum),  ]

billVote <- merge(billStatus, finalActionAll, by.x="billNo", by.y="bill", all.x=T)
billSponDistVote <- merge(billVote, oneCong, by = "sponID", all.x=T)
rm(billVote)
billSponDistVote$billNo <- as.numeric(as.character(billSponDistVote$billNo))
billSponDistVote <- billSponDistVote[order(billSponDistVote$billNo),  ]



getwd()
write.csv(billSponDistVote, file = file.path(allCongPath,'113\\billNomStatus.csv'))
