mejor <- function(estado, resultado){
    db <- read.csv("~/GitHub/outcome-of-care-measures.csv", colClasses = "character")
    h <- vector("numeric")
    d <- vector("numeric")
    l <- nrow(db)
    
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
        h1 <- 0
        for (j in 1:l) {
            if (db[j,7] == estado) {
                h1 <- length(h) + 1
                length(h) <- h1
                length(d) <- h1
                h[h1] <- db[j,2]
                d[h1] <- db[j,c]
            }
        }
        
        if (h1>0) {
            oldw <- getOption("warn")
            options(warn = -1)
            v <- as(d,"numeric")
            options(warn = oldw)
            x <- data.frame(h,v,stringsAsFactors = FALSE)
            y <- x[order(v,h),]
            y[1,1]
        } else {
            "ESTADO INVALIDO"
        }
    } else {
        "RESULTADO INVALIDO"
    }
}

mejor("TX", "falla")
 
