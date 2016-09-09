#DEFUNCT


wd <- paste("C:\\Users\\marciposafm\\Documents\\allCongress", sep="")
setwd(wd)

partyControl <- read.csv("partyControl.csv", header=T)

allLeg <- data.frame()  #need to create it with appropriate columns



#census <- Reduce(function(x, y) merge(x, y, all=TRUE), censuses)


# varTmp <- censusBillData$fracServed
# numericVarTmp <- as.numeric(as.character(varTmp))
# summary(numericVarTmp)
# censusBillData$stateDist[which(numericVarTmp== max(numericVarTmp, na.rm=T))]