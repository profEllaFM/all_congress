# making datasets for each congress and an associated congress number

rangeCong = c(93:97, 103:113)
datasets <- vector("list",length(rangeCong))

for(i in 1:length(datasets)){
  congNum <- as.numeric(as.character(rangeCong[i]))
  print(congNum)
  print(i)
  datasets[[i]] <- allCongress[which(allCongress$congNum == congNum), ]
  print(dim(datasets[[i]]))
}

yvarName <- "numPassH"
data <- datasets[[which(rangeCong == 93)]]

subsetName <- "Democrat"
subsetData <- data[ which(data$party == subsetName) , ]
yvar <- subsetData[ , which(names(subsetData)== yvarName)]
m <- glm.nb(yvar~over75k+ prcntBA + prcntBlack*black+dwnom1+numberTerms, data=subsetData, weights = subsetData$fracServed)
xtable(summary(m)$coef)

subsetName <- "Republican"
subsetData <- data[ which(data$party == subsetName) , ]
yvar <- subsetData[ , which(names(subsetData)== yvarName)]
m <- glm.nb(yvar~over75k+ prcntBA + log(prcntBlack)+dwnom1+
              recentArrivalPrcnt+numberTerms, data=subsetData, weights = subsetData$fracServed)
xtable(summary(m)$coef)
