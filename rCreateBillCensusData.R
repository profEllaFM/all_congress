###################
#Analyzing all bills and censuses
#
#Ella Foster-Molina    2/2016
#####################
#rm(list = ls())
library(foreign)
library(xtable)
library(MASS)
library(base)
library(reldist)

#library(data.table)

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
}  #used after the fact at the end of main

#used at top of main
changeNominate <- function(wd, nominateFile){
  nominate <- read.dta(file.path(wd,'nominateScores.DTA'))
  rowsChangeParty <- which(grepl("9[[:digit:]]{4}", nominate$idno))  #getting rid of values that start with 0
  nominate$idno[rowsChangeParty] <- gsub("^9", "1",nominate$idno[rowsChangeParty])
  nominate <- replaceNomICPSR(nominate)
  nominate <- nominate[-which(grepl("199[[:digit:]]{2}", nominate$idno)),]  #president
  nominate
}

replaceNomICPSR <- function(nominate){
  fixes <- read.csv("replaceNomICPSR.csv", header=T)
  for(i in 1:length(fixes$district)){
    nominate$idno[which(nominate$idno == fixes$origNomICPSR[i])] <- fixes$newNomICPSR[i] 
  }
  nominate
}

#used by main for bills
createBillData <- function(congNum, nominate){
  wd <- "C:\\Users\\marciposafm\\Documents\\allCongress" 
  setwd(wd) 
  congNumPath <- paste("C:\\Users\\marciposafm\\Documents\\allCongress\\",congNum, sep = "") 
  
  sponsored <- billTrans("sponsorMatrix.txt", congNumPath, 'numSpon',T,"\t")
  cospon <- billTrans("cosponsorMatrix.txt", congNumPath, 'numCosp',T,"\t")
  passH <- billTrans("passHouseMatrix.txt", congNumPath, 'numPassH',T,"\t")
  enacted <- billTrans("enactedMatrix.txt", congNumPath, 'numEnact',T,"\t")
  info <- createBillLegInfo(congNum, nominate)
  committee <- createBillComInfo(congNum, passH)
  minorities <- createBillMinorityInfo(congNum, dim(passH)[1])
  
  #sponID 403617 has an extra name part that adds a superfluous row. Fixed by hand.    
  dataSets <- list(info,committee,minorities,sponsored, cospon, passH,enacted)
  
  billStuff <- Reduce(function(x, y) merge(x, y, all=TRUE), dataSets)
  billStuff$stateDist <- paste(billStuff$state, billStuff$district, sep='.')
  billStuff$passPrcnt <- 100*makeNumeric(billStuff$numEnact)/makeNumeric(billStuff$numSpon)
  print(dim(billStuff))
  billStuff
}

#used by createBillData
createBillLegInfo <- function(congNum, nominate){
  file <- paste("leg",congNum, ".txt", sep = "")
  legislatorInfo <- read.csv(file, header = F)
  colnames(legislatorInfo) = cbind('state', 'district', 'sponID', 'icpsr','lastName', 'firstName',
                                   'middleName', 'age', 'gender', 'party', 'numberTerms', 'daysServed')
  modeServeTime <-Mode(legislatorInfo$daysServed)
  legislatorInfo$fracServed <- makeNumeric(legislatorInfo$daysServed)/makeNumeric(modeServeTime)
  
  nominateCong <- subset(nominate, cong==congNum, select = c('idno','cd','statenm','dwnom1','dwnom2'))
  colnames(nominateCong)[which(names(nominateCong) == "idno")] <- "icpsr"
  nominateCong$icpsr[which(nominateCong$icpsr == 3769)] <- 15101  
  
  info <- merge(legislatorInfo, nominateCong, by = 'icpsr', all=T)
  info
}

#used by createBillLegInfo then createBillData
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#will have to change if add more committee info
#used by createBillData
createBillComInfo <- function(congNum, passH){
  if(109 <= congNum & congNum <=113 ){
    dir <- "C:\\Users\\marciposafm\\Documents\\allCongress\\committeeMembership\\" 
    fileName <- paste(dir, "committeeMembers",congNum,".txt", sep="")
    committee <- createComData(fileName)
  }else if(93 <= congNum & congNum <= 108 ){
    committee <- passH
    committee[ , c("comPower", "chair","rank","comName","numCom","rankChair")] <- NA
    committee$numPassH <- NULL
  }
  committee
}

#merges data from committee-individual to just by individual
#adds rank, power, and chair of all committees a person is on
#used by createBillComInfo then createBillData
createComData <- function(fileName){
  committee <- read.csv(fileName, sep = '\t')
  committee$comName <- as.character(committee$comName)
  #committee[which(duplicated(committee$sponID)==T),]
  names(committee)[which(names(committee)=="X")] = "numCom"
  committee$numCom <- 1
  uniqueDups <- unique(committee$sponID[duplicated(committee$sponID)])
  deleteItems <- c()
  for( id in uniqueDups){
    rankI <- 0
    power <- 0
    chairI <- 0
    rowID = which(committee$sponID == id)[1]
    for(memberRow in which(committee$sponID == id)){
      committee$numCom[rowID] = length(which(committee$sponID == id))
      power <- power +committee$comPower[memberRow]
      rankI <- rankI +committee$chair[memberRow]
      chairI <- chairI +committee$rank[memberRow]
      if(memberRow != rowID){
        deleteItems <- append(deleteItems,-memberRow)
        keepName <- committee$comName[rowID]
        delName <- committee$comName[memberRow]
        committee$comName[rowID] <- paste(keepName,delName, sep=' ')
      }
    }
    committee$comPower[rowID] <- power
    committee$rank[rowID]<-rankI
    committee$chair[rowID]<-chairI
  }
  committee <- committee[deleteItems,]
  committee$rankChair <- makeNumeric(committee$chair)+.5*makeNumeric(committee$rank)
  committee
}

#used by createBillData
createBillMinorityInfo <- function(congNum, numPeople){
  dir <- "C:\\Users\\marciposafm\\Documents\\allCongress\\" 
  file <- "\\minorityList.csv"  #104-108 don't have minority, but need minority columns so using cols of NAs
  minTmp <- read.csv(paste(dir, congNum, file, sep=""), header = T)  
  minorities <- subset(minTmp, select = c(sponID,black, hispanic))[1:numPeople,]#gets rid of excess rows
  minorities
}

# create summed columns of bill matrices
# result is number of sponsored/cosp/enacted/passH bills for each member of Congress
# each row is one sponsored identified by sponID
#used by createBillData
billTrans <- function(fileName, wd, colName, headerT, sepType){
  matrixIn <- read.table(file.path(wd, fileName), header=headerT, sep=sepType)  
  matrixOut <- colSums(matrixIn[,2:(dim(matrixIn)[2]-1)])
  matrixOut <- data.frame(colnames(matrixIn[2:(dim(matrixIn)[2]-1)]), matrixOut)
  colnames(matrixOut) = cbind('sponID',colName)
  matrixOut$sponID <- gsub("X", "", matrixOut$sponID)
  matrixOut
}

#uses isolateVariables function
# creates list of census data frames, then merges them for one congress
#used by createCensusFinal for censuses
mergedCensuses <- function(dataFileNames, congNum){
  lenFiles <- dim(dataFileNames)[2]
  censuses <- vector("list",lenFiles)
  names(censuses) <- names(dataFileNames)
  fileTypes = names(dataFileNames)
  
  for(i in 1:lenFiles){
    varFileName<- paste("variablesOfInterest",fileTypes[i],".csv",sep="")
    dataFileName <- dataFileNames[i]
    censuses[[i]] <- isolateVariables(dataFileName,varFileName)
  }
  
  census <- Reduce(function(x, y) merge(x, y, all=TRUE), censuses)
  census <- fixCensusErrors(census)
  census
}

fixCensusErrors <- function(census){
  if(congNum != 93){
    census <- census[-which(census$congDist== "Geography"),]
    census <- census[-which(grepl("Puerto Rico",census$congDist)),]
  }
  if(congNum == 109){
    census$prcntNotHisp[which(census$prcntNotHisp == "N")] = NA
  }
  write.table(census, file = "rescanVarTypes.csv",sep=',')
  census <- read.csv("rescanVarTypes.csv", header = T)
  census
}

#pulls the relevant variables from the entire census files
#ensures each variable has a descriptive name as determined from a .csv file I wrote
#used by mergedCenuses
isolateVariables <- function(dataFileName, varFileName){
  vars <- read.csv(varFileName, header=T)  
  usedVarRows = which(vars$varName != '')
  useOldVarNames = as.character(vars$variable[usedVarRows])
  useNewVarNames = as.character(vars$varName[usedVarRows])
  
  dataFileName <- as.character(dataFileName[1,1])
  censusTmp <- read.csv(dataFileName, header = T)
  census <- subset(censusTmp, select = useOldVarNames)
  names(census) <- useNewVarNames
  #print(dataFileName)
  #print(dim(census))
  census
}

makeEdu93 <- function(census){
  census$totalEdu <- rowSums(census[ , grep("V3", names(census), fixed = TRUE)])
  census$prcntHS <- getPrcnt(census, c("V366","V367","V368","V373","V374","V375","V380","V381","V382"), 
                             census$totalEdu) 
  census$prcntBA <- getPrcnt(census, c("V368","V375","V382"), 
                             census$totalEdu) 
  census <- census[ , -grep("V3", names(census), fixed = TRUE)]
  census
}  #used by for1973

makeIncome93 <- function(census){
  census$totalHouseholds <- rowSums(census[ , grep("V1", names(census), fixed = TRUE)])
  census$over200k <- getPrcnt(census, c("V140","V125"), 
                              census$totalHouseholds)  #over 50000 1973 $, 275k 2016 $
  census$over150k <- getPrcnt(census, c("V139","V124"), #over 25000 1973 $, 140k 2016 $  
                              census$totalHouseholds)+census$over200k
  census$over100k <- getPrcnt(census, c("V138","V123"), 
                              census$totalHouseholds) +census$over150k  #over 15000 1973 $, 83k 2016 $
  census$over75k <- getPrcnt(census, c("V137","V122"), 
                             census$totalHouseholds) +census$over100k  #over 12000 1973 $, 67k 2016 $
  census$over50k <-getPrcnt(census, c("V136","V121", "V135","V120"), 
                            census$totalHouseholds)  +census$over75k #over 9000 and 1000 1973 $, 50k 2016 $ 
  census$over35k <- getPrcnt(census, c("V133","V118", "V132","V117","V134", "V119"), 
                             census$totalHouseholds) +census$over50k #over 6000 and 7000 and 8000 1973 $, 33k 2016 $
  census$over25k <- getPrcnt(census, c("V131","V116", "V130","V115"), 
                             census$totalHouseholds) +census$over35k #over 4000 and 5000 1973 $, 22k 2016 $
  census$over15k <- getPrcnt(census, c("V129","V114"), 
                             census$totalHouseholds) +census$over25k  #over 3000 1973 $, 15k 2016 $
  census$over10k <-  getPrcnt(census, c("V128","V113"), 
                              census$totalHouseholds) +census$over15k #over 2000 1973 $, 10k 2016 $
  census$under10k <- getPrcnt(census, c("V127","V112", "V126", "V111"), 
                              census$totalHouseholds)  #under 1000 and 2000 1973 $
  census <- census[ , -grep("V1", names(census), fixed = TRUE)]
  census
}  #used by for1973

makeGini93 <- function(census){
  tmp <- data.frame(census$censusCode, census$congDist)
  tmp$totalHouseholds <- rowSums(census[ , grep("V1", names(census), fixed = TRUE)])  
  tmp$under10k <- getPrcnt(census, c("V127","V112", "V126", "V111"), 
                           tmp$totalHouseholds)/100  #under 1000 and 2000 1973 $
  tmp$l10h15k <-  getPrcnt(census, c("V128","V113"), 
                           tmp$totalHouseholds)/100 #over 2000 1973 $, 10k 2016 $
  tmp$l15h25k <- getPrcnt(census, c("V129","V114"), 
                          tmp$totalHouseholds)/100  #over 3000 1973 $, 15k 2016 $
  tmp$l25h35k <- getPrcnt(census, c("V131","V116", "V130","V115"), 
                          tmp$totalHouseholds)  #over 4000 and 5000 1973 $, 22k 2016 $
  tmp$l35h50k <- getPrcnt(census, c("V133","V118", "V132","V117","V134", "V119"), 
                          tmp$totalHouseholds)/100 #over 6000 and 7000 and 8000 1973 $, 33k 2016 $
  tmp$l50h75k <-getPrcnt(census, c("V136","V121", "V135","V120"), 
                         tmp$totalHouseholds)/100 #over 9000 and 1000 1973 $, 50k 2016 $ 
  tmp$l75h100k <- getPrcnt(census, c("V137","V122"), 
                           tmp$totalHouseholds)/100  #over 12000 1973 $, 67k 2016 $
  tmp$l100h150k <- getPrcnt(census, c("V138","V123"), 
                            tmp$totalHouseholds)/100  #over 15000 1973 $, 83k 2016 $
  tmp$l150h200k <- getPrcnt(census, c("V139","V124"), #over 25000 1973 $, 140k 2016 $  
                            tmp$totalHouseholds)/100
  tmp$over200k <- getPrcnt(census, c("V140","V125"), 
                           tmp$totalHouseholds)/100  #over 50000 1973 $, 275k 2016 $
  incomeMeans <- c(5, 12.5, 20, 30, 42, 62,87, 125, 175, 500)
  census$gini <- apply(tmp[, which(names(tmp)== "under10k"):ncol(tmp)], 1, function(i){
    gini(incomeMeans, i)
  })
  census
}  #use before makeIncome93, used by for1973

makeRace93 <- function(census){
  census$totalPopRaceFile <- rowSums(census[ , grep("RacePop", names(census), fixed = TRUE)])
  census$totalPopBirthPlace <-census$totalPopRaceFile #for consistent variables
  census$prcntWhiteAll <- getPrcnt(census, c("whiteRacePop"), census$totalPopRaceFile) 
  census$prcntBlack <- getPrcnt(census, c("blackRacePop"), census$totalPopRaceFile) 
  census$prcntAsian <- getPrcnt(census, c("chineseRacePop","indianRacePop","japaneseRacePop","filipinoRacePop"), 
                                census$totalPopRaceFile) 
  census$totalHisp <- census$totalPopHispUrban  #oddly low numbers, but matches sum of urban rural files
  census$prcntHisp <- getPrcnt(census, names(census)[grep("HispPop", names(census), fixed = TRUE)], 
                                census$totalHisp) 
  census$prcntForeignBorn <-  getPrcnt(census, c("popForeignBorn"),  census$totalPopRaceFile) 
  census <- census[ , -grep("RacePop", names(census), fixed = TRUE)]
  census <- census[ , -grep("HispPop", names(census), fixed = TRUE)]
  census
}  #used by for1973

makeEmploy93 <- function(census){
  census$totalEmploymentPop <- rowSums(census[ , grep("male", names(census), fixed = TRUE)])
  census$prcntUnemp <- getPrcnt(census, names(census)[grep("maleUnemployed", names(census), fixed = TRUE)], 
                                   census$totalEmploymentPop)  
  census$prcntNotEmploy <- getPrcnt(census, names(census)[grep("maleNotLaborU65", names(census), fixed = TRUE)], 
                                census$totalEmploymentPop) 
  census$recentArrivalPrcnt <- 100*rowSums(census[ , grep("moved", names(census), fixed = TRUE)])/census$totalEmploymentPop
      #should use a better population total for arrivals
  census <- census[ , -grep("male", names(census), fixed = TRUE)]
  census <- census[ , -grep("moved", names(census), fixed = TRUE)]
  census
}  #used by for1973

makeCook93 <- function(census){
  demPrcnt <- getPrcnt(census, names(census)[grep("demPresVote66", names(census), fixed = TRUE)], 
                                census$totalPresVote66)  
  repPrcnt <- getPrcnt(census, names(census)[grep("repPresVote66", names(census), fixed = TRUE)], 
                       census$totalPresVote66) 
  census$cook <- repPrcnt - demPrcnt
  census <- census[ , -grep("PresVote66", names(census), fixed = TRUE)]
  census
}  #used by for1973

addMissingCols93 <- function(census){
  missingCols <- c("abroadPrcnt","prcntExAliens","medianIncome","meanIncome","prcntBlackNotHisp",
                   "prcntMulti","prcntWhite","prcntNotHisp","prcntOld","medianAge")
  census[ , missingCols] <- NA
  census
}  #used by for1973

makeIncomeWPrcnt <- function(census){
  incomeVars <- census[ ,which(grepl("[[:digit:]]{2}k", names(census)))]
  census <- census[ , -which(grepl("[[:digit:]]{2}k", names(census)))]  #removing old income vars
  census$over200k <- incomeVars$over200k
  census$over150k <- incomeVars$l150h200k + census$over200k 
  census$over100k <- incomeVars$l100h150k + census$over150k 
  census$over75k <- incomeVars$l75h100k + census$over100k 
  census$over50k <- incomeVars$l50h75k + census$over75k 
  census$over35k <- incomeVars$l35h50k + census$over50k 
  census$over25k <- incomeVars$l25h35k + census$over35k 
  census$over15k <- incomeVars$l15h25k + census$over25k 
  census$over10k <- incomeVars$l10h15k + census$over15k 
  census$under10k <- incomeVars$under10k
  incomeMeans <- c(5, 12.5, 20, 30, 42, 62,87, 125, 175, 500)
  incomeVarNames <- c("under10k", "l10h15k","l15h25k","l25h35k","l35h50k",
                      "l50h75k","l75h100k","l100h150k","l150h200k","over200k")
  census$gini <- apply(incomeVars[, incomeVarNames], 1, function(i){
    gini(incomeMeans, i)
  })
  #census$gini <- NA
  census
}    #used by forOthers

#used by forOthers to create percentages instead of numbers
makeAllPrcnt <- function(census, colNames, divisor){
  tmp <- census
  tmp[ , names(tmp) %in% colNames] <- 100*census[ , names(census) %in% colNames]/divisor
  tmp
}

#create desired variables
#used by main for making correct var names
for1973 <- function(census){
  census <- makeEdu93(census)
  census <- makeGini93(census)
  census <- makeIncome93(census)
  census <- makeRace93(census)  
  census <- makeEmploy93(census)
  census <- makeCook93(census)
  census <- addMissingCols93(census)
  census
}

#create desired variables
#used by main for making correct var names
forOthers <- function(census, congNum){
  if(congNum == 106){
    census$prcntExAliens <- getPrcnt(census,  c("naturalizedCitizenEst"),  census$foreignBornEst)
    census$prcntBlackNotHisp <- NA
  }else if(congNum == 109){
    colNames <- c("prcntNotHisp", "prcntWhite","prcntBlackNotHisp","prcntHisp","prcntBlack","prcntWhiteAll",
                  "prcntMulti","prcntAsian","prcntOver18","prcntOver62")
    census <- makeAllPrcnt(census, colNames, census$totalPopRaceFile)
    census <- makeAllPrcnt(census, names(census)[which(grepl("[[:digit:]]{2}k", names(census)))],
                           census$totalHouseholds)
    census <- makeAllPrcnt(census, c("abroadPrcnt","recentArrivalPrcnt"), census$totalPopBirthPlace)
    census$prcntNotEmploy <- 100*census$prcntNotEmploy/census$totalEmploymentPop
    census$prcntForeignBorn <- 100*census$foreignBornEst/census$totalPopBirthPlace   
    census$prcntExAliens <- getPrcnt(census,  c("naturalizedCitizenEst"),  census$foreignBornEst)
  }else if(congNum == 110 | congNum == 111 | congNum == 113){
    census$prcntOver62 <- rowSums(census[ , which( grepl("[[:digit:]]to[[:digit:]]", names(census)) )])+census$prcntOver85
    census <- census[ , - which( grepl("[[:digit:]]to[[:digit:]]", names(census)) )]  #removing old age vars
    census$prcntOver18 <- NA
  }
  census$prcntYoung <- 100 - census$prcntOver18
  census$prcntOld <- census$prcntOver62
  census <- makeIncomeWPrcnt(census)
  census
}

#creating row names with format AL.1 instead of censusCode =1 congDist=1
createDistCode93 <- function(congNum, census, stateDist){
  census <- subset(census, congDist != 0)  #0's are full state numbers
  
  dir <- paste("C:\\Users\\marciposafm\\Documents\\allCongress\\",congNum,"\\census\\", sep="")
  stateCodes <- read.csv(file.path(dir,"stateCodes.csv"), header=T)
  stateCodes <- as.data.frame(stateCodes)
  
  census <- merge(census, stateCodes, by="censusCode")
  census$stateDist <- paste(census$stateAbbr, census$congDist, sep=".")
  zeroStates <- stateDist[which(grepl(".0", stateDist, fixed=T))]
  oneStates <- gsub(".0",".1", zeroStates, fixed=T)
  fixDist <- data.frame(zeroStates, oneStates)
  for(i in 1:length(fixDist$oneStates)){
    dist1 <- fixDist$oneStates[i]
    census$stateDist[which(census$stateDist == dist1)] <- as.character(fixDist$zeroStates[i])
  }
  census
}

#takes census district name Congressional District (at Large) (113th Congress), Alaska
#and makes it AK.0 or CA.50
createDistCode <- function(census, distReplace){
  census$state <- 'NA'
  census$district <- 99
  census$congDist <- as.character(census$congDist)
  
  
  for(i in 1:length(census$congDist)){
    stateName <- strsplit(census$congDist[i],", ")[[1]][2]
    census$state[i] <- distReplace$abrv[which(distReplace$full== stateName)]
    
    tmpDist <- strsplit(census$congDist[i],"District ")[[1]][2]
    distNum <- gsub(" ","",strsplit(tmpDist,"\\(")[[1]][1])
    if(distNum == ""){
      distNum = 0
    }else{
      distNum = as.numeric(distNum)
    }  
    census$district[i] <- distNum
  }
  
  census$stateDist <- paste(census$state, census$district, sep='.')
  census
}

#only works for numeric variables
#used generally
getPrcnt <- function(census, sumColNames, totalCol){
  prcntVar <- 100*rowSums(subset(census, select = sumColNames))/totalCol
  prcntVar
}

#make a factor into numeric with as.numeric(as.character())
#used generally
makeNumeric <- function(var){
  var <- as.numeric(as.character(var))
  var
}

#used by createCensusFinal for censuses
createDataFileNames <- function(congNum, censusFiles){
  dataFileNames <- censusFiles[which(censusFiles$congNum == congNum),]
  dataFileNames <- dataFileNames[,colSums(is.na(dataFileNames))<nrow(dataFileNames)]  #remove NA values
  dataFileNames$congNum <- NULL
  dataFileNames
}

#used by createCensusFinal for census
createNecessaryVars <- function(census, congNum, distReplace, correctStates){
  if(congNum == 93){
    census <- for1973(census)
    censusWAbrv <- createDistCode93(congNum, census, correctStates)
    print(censusWAbrv$stateDist[which(grepl(".0", censusWAbrv$stateDist, fixed=T))])
  }else{
    census <- forOthers(census, congNum)
    censusWAbrv <- createDistCode(census, distReplace)
  }
  censusWAbrv
}

#used by main for bills
#Adds columns of constant identifiers, such as billData$congNum == 111 for the entire dataframe
createConstantCongIDs <- function(billData, congNum, partyControl){
  print(which(partyControl$congNum == congNum))
  rowNum <- which(partyControl$congNum == congNum)
  billData[ , names(partyControl)] <- NA
  for(name in names(partyControl)){
    billData[ , which(names(billData) == name)] <- partyControl[ rowNum, which(names(partyControl) == name)]
  }
  billData
}

#used by main
createCensusFinal <- function(congNum, censusFiles, distReplace, correctStates, keeperVars){
  congNumPath <- paste("C:\\Users\\marciposafm\\Documents\\allCongress\\",congNum,"\\census", sep="")
  setwd(congNumPath)
  
  dataFileNames <- createDataFileNames(congNum, censusFiles)
  census <- mergedCensuses(dataFileNames, congNum)
  names(census) #all variables still need to be massaged
  print(congNum)
  census <- createNecessaryVars(census, congNum, distReplace, billDatasets[[which(rangeCong == 93)]]$stateDist)
  censusFinal <- subset(census, select = keeperVars)
  censusFinal
}

###Reference data... file names, replacements, nominate, party control, which vars to keep

allCongPath <- "C:\\Users\\marciposafm\\Documents\\allCongress" 
setwd(allCongPath)
nominate <- changeNominate(allCongPath , 'nominateScores.DTA')
partyControl <- read.csv(file.path(allCongPath,"partyControl.csv"), header=T)
censusFiles <- read.csv("censusFileNames.csv", header=T)
distReplace<- read.csv("stateFullAbvList.csv", header=T, stringsAsFactors=FALSE)  
keeperVars <- read.csv("keeperVars.csv", header=T, stringsAsFactors=FALSE)$varNames
censusCoverage <- read.csv("censusCoverage.csv", header=T)

###############JUST BILLS########
rangeCong = c(93:97, 103:113)
congNum <- rangeCong[4]


missingICPSR <- data.frame()
for(congNum in c(94:97, 103:113)){  #cong 93 is fine
  print(congNum)
  info <- createBillLegInfo(congNum, nominate)
  if(dim(info[is.na(info$sponID),])[1] > 0){
    missing <- subset(info[is.na(info$sponID),], select = c("icpsr","statenm", "cd"))
    missing$congNum <- congNum
    missingICPSR <- rbind(missingICPSR, missing)
  }
}  #have dealt with everything but ppl with two icpsr codes in one year
missingICPSR
                       
billDatasets <- vector("list",length(rangeCong))
for(i in 1:length(rangeCong)){ 
  print(rangeCong[i])
  billDatasets[[i]] <- createBillData(rangeCong[i], nominate)
}  #creates a list of dataframes, one for each congress

##########CENSUSES and MERGING CENSUS WITH BILLS###############
tmp <- billDatasets
censusRange <- c(93, 106, 109:113)
congNum <- censusRange[2]

billDatasets <- tmp
for(congNum in censusRange){
  censusFinal <- createCensusFinal(congNum, censusFiles, distReplace, 
                                   billDatasets[[which(rangeCong == 93)]]$stateDist, keeperVars)  
  censusCoverageCongNum <- censusCoverage[which(censusCoverage$censusCong == congNum), ]
  congYrs <- c(censusCoverageCongNum$billCongFirst: censusCoverageCongNum$billCongLast)
  #merge census with billData for every congress
  for(billCong in congYrs){
    print(billCong)
    billData <- billDatasets[[which(rangeCong ==billCong)]]   
    billData <- createConstantCongIDs(billData, billCong, partyControl)
    censusBillData <- merge(censusFinal, billData, all=TRUE )
    print(dim(censusBillData))
    billDatasets[[which(rangeCong ==billCong)]] <- censusBillData
  }
}



# tmpNum1 <- 2
# tmpMerged <- billDatasets[[tmpNum1]]
# summary(tmpMerged$congNum)
# tmpMerged[which(tmpMerged$sponID==403203),]
# tmpMerged <- rbind(billDatasets[[15]], billDatasets[[16]])
# View(tmpMerged)

tmpMerged <- Reduce(function(x, y) rbind(x, y, all=TRUE), billDatasets)

tmpMerged$party[grepl("Democrat", tmpMerged$party)] <- "Democrat"
tmpMerged$party[which(tmpMerged$party == "New Progressive")] <- "Democrat"
tmpMerged$party[which(tmpMerged$party == "AL")] <- NA
tmpMerged$party <- drop.levels(tmpMerged$party)

tmpMerged <- tmpMerged[ - which(tmpMerged$stateDist == TRUE), ]  #drop all1 all2 rows
                                        #these are logicals from the Reduce rbind function
tmpMerged <- extractSES(tmpMerged)
#write file being analyzed
setwd(allCongPath)
fileName = "allCongressData.csv"
write.table(tmpMerged, file= fileName, sep=",")
# 
# colNames <- as.character(colnames(censusWAbrv))
# write.table(colNames, file = "variables.csv",sep=',')
