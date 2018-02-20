complete <- function(directory,id=1:332){ 
  
  output <- matrix(nrow=length(id),ncol=2)
  colnames(output) <- c("id","nobs")
  
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
    totalgood <- table[,1][!totalbad]
    nobs <- length(totalgood) 
    output[count,1] <- i
    output[count,2] <- nobs
  
  count <- count + 1
    
  }

print(output)  
  
}