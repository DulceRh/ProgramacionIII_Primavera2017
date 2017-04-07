setwd("~/GitHub/programacion3/specdata")
mediacontaminante <- function(directorio, contaminante, id= 1:322){
    informacion<- numeric()
    
    for(n in  id){
        j <- read.csv(paste(formatC(n, width = 3, flag= "0")
                            ,".csv", sep=""),header =TRUE)
        
        if (contaminante == "nitrate"){
            informacion <- c(informacion, j$nitrate)
            
        }
        
        promedio <- mean(informacion, na.rm = TRUE)
        
    }
    
    promedio   
}
mediacontaminante("specdata", "nitrate", id= 1:322)
