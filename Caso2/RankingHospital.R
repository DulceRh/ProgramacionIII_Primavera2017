rankhospital <- function(estado, resultado, num = "mejor") { 
    # Lectura de datos 
    # Revisi??n de la validez de estado y resultado 
    # Regresa el nombre del hospital con el puesto dado de la tasa m??s  
    # baja de mortalidad de 30 d??as
    
    data <- read.csv("~/GitHub/outcome-of-care-measures.csv", colClasses = "character")
    
    estados <- data[ , 7]
    resultados <- c("Infarto", "Falla Card??aca", "Neumon??a")
    
    if ((estado %in% estados) == FALSE) {
        stop(print("Estado inv??lido"))
    }
    else if ((resultado %in% resultados) == FALSE) {
        stop(print("Resultado inv??lido"))
    }
    new_data <- subset(data, State == estado)
    
    if (resultado == "Infarto") {
        Col_Res <- 11
    }
    else if (resultado == "Falla Card??aca") {
        Col_Res <- 17
    }
    else {
        Col_Res <- 23
    }
    
    if (is.numeric(num) == TRUE) {
        if (length(data[,2]) < num) {
            return(NA)
        }
    }
    
    new_data[, Col_Res] <- as.numeric(new_data[,Col_Res])
    mal <- is.na(new_data[, Col_Res])
    Info <- new_data[!mal, ]
    
    Nombre_Col_Res <- names(Info)[Col_Res]
    Nombre_Hosp <- names(Info)[2]
    index <- with(Info, order(Info[Nombre_Col_Res], Info[Nombre_Hosp]))
    Datos_Ordenados <- Info[index, ]
    
    if (is.character(num) == TRUE) {
        if (num == "mejor") {
            num = 1
        }
        else if (num == "peor") {
            num = length(Datos_Ordenados[, Col_Res])
        }
    }
    Datos_Ordenados[num, 2]
}

rankhospital("TX","Falla Card??aca", 4)
rankhospital("MD","Infarto", "peor")
rankhospital("MN","Infarto", 5000)