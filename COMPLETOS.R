setwd("~/GitHub/Programacion3/specdata")
completos<- function(directorio,id=1:332){
    informacion<- numeric()
  
        for (n in id){
            j <- read.csv(paste(formatC(n, width = 3, flag ="0")
                                , ".csv",sep=""),header = TRUE )
        
        suma <- 0
        for (m in 1:nrow(j)){
            l <- complete.cases( j[m,])
            
            if(l == TRUE){
                suma <- suma + 1
                
            }
            
        }
       informacion <- c(informacion, suma)
        
    }
    ID = id
    data1 <- data.frame(id=ID, nobs=informacion)
    print(data1)
}
completos( specdata, 1:2)