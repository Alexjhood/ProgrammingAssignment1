corr <- function(directory,threshold=1,id=1:332){ 
  
  output <- vector(length=length(id))
  count <- 1
  
  for(i in id) { 
    
    if(i<10)    
      {table <- read.csv(paste(directory,"/00",i,".csv",sep=""))}
    if(i>9 & i<100)  
      {table <- read.csv(paste(directory,"/0",i,".csv",sep=""))}
    if(i>99)
      {table <- read.csv(paste(directory,"/",i,".csv",sep=""))}
    
    badsulfate <- is.na(table[,2])
    badnitrate <- is.na(table[,3])
    totalbad <- badsulfate | badnitrate
    totalgoodsulfate <- table[,2][!totalbad]
    totalgoodnitrate <- table[,3][!totalbad]
    Correlation <- cor(totalgoodsulfate,totalgoodnitrate)
    
    if(length(totalgoodsulfate)>=threshold)
      {output[count] <- Correlation}
    else
      {output[count] <- NaN}
    
    count <- count + 1
  }
  
  print(output)  
  
}
