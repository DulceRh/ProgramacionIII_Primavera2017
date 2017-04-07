corr <- function(directorio, horizonte=0){
    
    cor <- vector("numeric",0)
    m <- 1
    for (i in 1:332){
        
        j<-formatC(i,width = 3 ,flag = "0")
        l <- read.csv(paste(j, ".csv",sep=""),header = T)
        data1 <- data.frame(readen$sulfate,readen$nitrate)
        complet <- data1[complete.cases(data1),] 
        l <- nrow(compl)
        
        if (l>horizonte){
            length(cor) <- length(cor)+1
            cor[m] <- cor(compl[,1],compl[,2])
            m <- m+1
        } 
    }
    cor
}
cr <- corr("specdata", 150)
head(cr)