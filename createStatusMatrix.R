# AllCongress takes a long time per loop

### takes all status matrix from each year
### makes it into a file of final action, title, sponIDs
## one entry per bill, has final action taken, title of bill, sponIDs

createBillStatus <- function(allStatus){
  numName <- allStatus$X0
  billStatus <- as.data.frame(numName)
  billStatus$sponID <- NA
  billStatus$action <- NA
  row = 8
  for(row in 1:nrow(allStatus)){
    if(! is.na(allStatus[row, which(allStatus[row,] != 0 )][2] )  ){
      billStatus$sponID[row] <- colnames(allStatus[, which(allStatus[row,] != 0)])[2]
      billStatus$action[row] <- as.character(allStatus[row, which(allStatus[row,] != 0)[2]])
      #print(paste(billStatus$sponID[row], billStatus$action[row]))
    }
  }  
  billStatus$sponID <- as.numeric(gsub("X","", billStatus$sponID))
  tmp <- as.data.frame(str_split_fixed(billStatus$numName, " ", 2))
  billStatus$billNo <- tmp$V1
  billStatus$billName <- tmp$V2
  billStatus$numName <- NULL
  billStatus
}  #takes a minute bc ugly for loop
#creating sponsor, action, bill num, bill title from allStatus matrix

allCongPath <- "C:\\Users\\marciposafm\\Documents\\allCongress" 
rangeCong = c(109:113)
i = 112

for(i in rangeCong){
  print(i)
  congPath <- paste(allCongPath,'\\',i,'\\allStatusMatrix.txt', sep='')
  allStatus <- read.csv(file.path(allCongPath,i,'allStatusMatrix.txt', fsep = '\\'), sep= '\t', header=T)
  billStatus <- createBillStatus(allStatus) #all bills in 113th cong, final action, title, sponIDs
  #takes time, only do once
  write.csv(billStatus, file = file.path(allCongPath,i,'billStatus.csv', fsep = '\\'))
}
congPath <- paste(allCongPath,'\\',i,'\\allStatusMatrix.txt', sep='')
allStatus <- read.csv(file.path(allCongPath,i,'allStatusMatrix.txt', fsep = '\\'), sep= '\t', header=T)
billStatus <- createBillStatus(allStatus) #all bills in 113th cong, final action, title, sponIDs
                #takes time, only do once
write.csv(billStatus, file = file.path(allCongPath,i,'billStatus.csv', fsep = '\\'))
