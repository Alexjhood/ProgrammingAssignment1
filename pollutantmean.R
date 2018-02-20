pollutantmean <- function(directory,pollutant,id=1:332){ 
  
pollutantmeans <- vector(mode = "numeric",length = length(id))  

for(i in id) { 
  
if(i<10)    
      {table <- read.csv(paste(directory,"/00",i,".csv",sep=""))}
if(i>9 & i<100)  
      {table <- read.csv(paste(directory,"/0",i,".csv",sep=""))}
if(i>99)
      {table <- read.csv(paste(directory,"/",i,".csv",sep=""))}
  
            tablemean <- table[[pollutant]]
            bad <- is.na(tablemean)
            tablemean <- tablemean[!bad]
            pollutantmeans[i-min(id)+1] <- mean(tablemean)
            
}

#print(pollutantmeans)

#Clear out all NAs and NaNs if existing for where there are no valid readings at a station

bad <- is.nan(pollutantmeans)
pollutantmeans <- pollutantmeans[!bad]
bad <- is.na(pollutantmeans)
pollutantmeans <- pollutantmeans[!bad]

#print(pollutantmeans)

print(mean(pollutantmeans))

}


