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