rankingcompleto <- function(resultado, num) {
    db <- read.csv("~/GitHub/outcome-of-care-measures.csv", colClasses = "character")
    l <- nrow(db)
    State <- unique(db[,7])
    Hospital <- vector("character", 54)
    
    if (resultado == "ataque") {
        c <- 11
    } else if (resultado == "falla") {
        c <- 17
    } else if (resultado == "neumon??a") {
        c <- 23
    } else {
        c <- 2
    }
    
    if (c>10) {
        for (w in 1:54) {
            estado<- State[w]
            h <- vector("numeric")
            d <- vector("numeric")
            h1 <- 0
            
            for (j in 1:l) {
                if(db[j,7] == estado) {
                    h1 <- length(h) + 1
                    length(h) <- h1
                    length(d) <- h1
                    h[h1] <- db[j,2]
                    d[h1] <- db[j,c]
                }
            }
            
            oldw <- getOption("warn")
            options(warn = -1)
            v <- as(d,"numeric")
            options(warn = oldw)
            x<-data.frame(h,v,stringsAsFactors = FALSE)
            y<- x[order(v,h),]
            
            if (num == "mejor") {
                Hospital[w] <- y[1,1]
            } else if (num == "peor") {
                lz <- nrow(y[complete.cases(y),])
                Hospital[w] <- y[lz,1]
            } else {
                Hospital[w] <- y[num,1]
            }
        }
        m <- data.frame(Hospital,State,stringsAsFactors = FALSE)
        n <- m[order(State,Hospital),]
        n
    } else {
        "RESULTADO INVALIDO"
    }
}        

rankingcompleto("neumon??a", "peor")