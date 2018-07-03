
library(dplyr)
library(ggplot2)
library(ggthemes)

train <- data.frame(data.table::fread('train.csv'),stringsAsFactors = F)
test <- data.frame(data.table::fread('test.csv'),stringsAsFactors = F)

feature_engineering <- function(tb){
    
    # Import library
    suppressWarnings(suppressMessages(library(dplyr)))
    
    tb[,'vive_en_pueblo'] <- ifelse(is.na(tb$AA5),0,1)
    
    tb[,'edad'] <- 2018 - tb$DG1
    tb$DG1 <- NULL
    
    tb[,'religion_hinduismo'] <- ifelse(tb$DG3A==4,1,0)
    tb$DG3A <- NULL
    
    tb[,'uid'] <- ifelse(tb$DG5_1==2,0,1)
    tb[,'tarjeta_PAN'] <- ifelse(tb$DG5_2==2,0,1)
    tb[,'pasaporte'] <- ifelse(tb$DG5_3==2,0,1)
    tb[,'licencia_conducir'] <- ifelse(tb$DG5_4==2,0,1)
    tb[,'ident_estudiante'] <- ifelse(tb$DG5_5==2,0,1)
    tb[,'tarjeta_votacion'] <- ifelse(tb$DG5_6==2,0,1)
    tb[,'cartilla_racionamiento'] <- ifelse(tb$DG5_7==2,0,1)
    tb[,'id_empleado'] <- ifelse(tb$DG5_8==2,0,1)
    tb[,'id_militar'] <- ifelse(tb$DG5_9==2,0,1)
    tb[,'libreta_depositos_banco'] <- ifelse(tb$DG5_10==2,0,1)
    tb[,'libreta_depositos_postal'] <- ifelse(tb$DG5_11==2,0,1)
    
    tb[,'escuela_pagada_boys'] <- ifelse(tb$DG12B_2==2,0,1)
    tb[,'escuela_pagada_girls'] <- ifelse(tb$DG12C_2==2,0,1)
    
    tb[,c('DG12B_1','DG12B_2','DG12C_1','DG12C_2')] <- NULL
    
    # Existen ciertas preguntas que tienen mucha relación. Estas se agruparán.
    # Y crearemos una variable suma, la cual indicará cuantas de estas preguntas
    # tienen como respuesta "si" para los entrevistados.
    
    vars_agregar <- c('DG13', 'DL4','DL25', 'DL26', 'MT4')
    
    # Borramos variables
    tb[,c('DG13_7','DG13_OTHERS')] <- NULL
    tb[,c('DL4_96','DL4_99','DL4_OTHERS')] <- NULL
    
    for(var in vars_agregar){
        
        vars_start <- names(tb)[grepl(pattern = paste0(var,'.*'),x = names(tb))]
        var_agg <- vars_start[1]
        tb[var] <- 0
        
        for(col in vars_start){
            tb[col] <- ifelse(tb[[col]]==2,0,1)
            tb[var] <- tb[[var]] + tb[[col]]
        }
        
        if(sum(is.na(tb[[var_agg]]))>0){
            tb[paste0(var,'_missing')] <- ifelse(is.na(tb[[var]]),1,0)
        }
        
    }
    
    tb[,'DG14_missing'] <- ifelse(is.na(tb$DG14),1,0)
    tb[,c('DG14','DG14_OTHERS')] <- NULL
    
    tb[,'main_earner_other'] <- ifelse(tb$DL0==2,0,1)
    tb$DL0 <- NULL
    
    tb[,'ama_de_casa'] <- ifelse(tb$DL1==7,1,0)
    tb[,'tiempo_completo'] <- ifelse(tb$DL1==1|tb$DL1==5,1,0)
    
    # Jobs: DL2
    tb[,'service_worker_np'] <- ifelse(tb$DL2==3,1,0)
    tb[,'house_cleaner'] <- ifelse(tb$DL2==9,1,0)
    tb[,'sastre'] <- ifelse(tb$DL2==12,1,0)
    tb$DL2 <- NULL
    
    tb[,'trabajo_secundario'] <- ifelse(tb$DL3==2,0,1)
    tb$DL3 <- NULL
    
    # DL5
    tb[,'mantenido'] <- ifelse(tb$DL5==5,1,0)
    tb[,'empleado_hogar'] <- ifelse(tb$DL5==17,1,0)
    tb$DL5 <- NULL
    
    tb[,'tiene_granja'] <- ifelse(tb$DL6==2,0,1)
    tb$DL6 <- NULL
    
    tb[,'trabaja_granja'] <- ifelse(tb$DL7==2,0,1)
    tb$DL7 <- NULL
    
    return(tb)
}

train_feat <- feature_engineering(train) 
test_feat <- feature_engineering(test) 

# Seleccionar columnas con un % de missing menor a 50%
cols_tol_miss <- names(train_feat)[colSums(is.na(train_feat))/nrow(train_feat)<0.5]
cols_tol_miss_train <- setdiff(cols_tol_miss,c('train_id'))
cols_tol_miss_test <- setdiff(cols_tol_miss,c('train_id','is_female'))

train_feat <- train_feat[,cols_tol_miss_train]
test_feat <- test_feat[,cols_tol_miss_test]

write.csv(train_feat,'./dataset_training/train_feat.csv',row.names = F)
write.csv(test_feat,'./dataset_training/test_feat.csv',row.names = F)
