
library(dplyr)
library(ggplot2)
library(ggthemes)

train <- data.frame(data.table::fread('train.csv'),stringsAsFactors = F)
test <- data.frame(data.table::fread('test.csv'),stringsAsFactors = F)

train[train==''] <- NA
test[test==''] <- NA

feature_engineering <- function(tb){
    
    # Import library
    suppressWarnings(suppressMessages(library(dplyr)))
    
    # Eliminar columas contantes
    tb[,c('MM2_16','DG5_96')] <- NULL
    
    tb[,'vive_en_pueblo'] <- ifelse(is.na(tb$AA5),0,1)
    tb[,c('AA5','AA6')] <- NULL
    
    tb[,'edad'] <- 2018 - tb$DG1
    tb$DG1 <- NULL
    
    tb[,'religion_hinduismo'] <- ifelse(tb$DG3A==4,1,0)
    #tb$DG3A <- NULL
    
    # tb[,'uid'] <- ifelse(tb$DG5_1==2,0,1)
    # tb[,'tarjeta_PAN'] <- ifelse(tb$DG5_2==2,0,1)
    # tb[,'pasaporte'] <- ifelse(tb$DG5_3==2,0,1)
    # tb[,'licencia_conducir'] <- ifelse(tb$DG5_4==2,0,1)
    # tb[,'ident_estudiante'] <- ifelse(tb$DG5_5==2,0,1)
    # tb[,'tarjeta_votacion'] <- ifelse(tb$DG5_6==2,0,1)
    # tb[,'cartilla_racionamiento'] <- ifelse(tb$DG5_7==2,0,1)
    # tb[,'id_empleado'] <- ifelse(tb$DG5_8==2,0,1)
    # tb[,'id_militar'] <- ifelse(tb$DG5_9==2,0,1)
    # tb[,'libreta_depositos_banco'] <- ifelse(tb$DG5_10==2,0,1)
    # tb[,'libreta_depositos_postal'] <- ifelse(tb$DG5_11==2,0,1)
    
    tb[,'escuela_pagada_boys'] <- ifelse(tb$DG12B_2==2,0,1)
    tb[,'escuela_pagada_girls'] <- ifelse(tb$DG12C_2==2,0,1)
    
    tb[,c('DG12B_1','DG12B_2','DG12C_1','DG12C_2')] <- NULL
    
    # Existen ciertas preguntas que tienen mucha relación. Estas se agruparán.
    # Y crearemos una variable suma, la cual indicará cuantas de estas preguntas
    # tienen como respuesta "si" para los entrevistados.
    
    vars_agregar <- c('DG5','DG13', 'DL4','DL25', 'DL26', 'MT4','MT18_','MT18A',
                      'FF6_','FF10','FF14','FF19',
                      'MM2_','MM3_','MMP1_','IFI1','IFI3',
                      'FL7','FB16','FB19A','FB19B','FB22','FB26','FB27','FB29','FF7')
    
    # Borramos variables redundantes
    tb[,c('DG13_7','DG13_OTHERS')] <- NULL
    tb[,c('DL4_96','DL4_99','DL4_OTHERS')] <- NULL
    tb[,c('DL26_12','DL26_99')] <- NULL
    tb[,c('MT18_96')] <- NULL
    tb[,c('FF10_96')] <- NULL
    tb[,c('FF14_96')] <- NULL
    tb[,c('MM2_15','MM2_16')] <- NULL
    tb[,c('MMP1_96')] <- NULL
    tb[,c('FB16_96')] <- NULL
    tb[,c('FB19A_96')] <- NULL
    tb[,c('FB19B_96')] <- NULL
    tb[,c('FB22_96')] <- NULL
    tb[,c('FB26_96','FB26_99')] <- NULL
    tb[,c('FB27_96')] <- NULL
    tb[,c('FB29_96')] <- NULL
    tb[,c('FF7_96')] <- NULL
    
    for(var in vars_agregar){
        
        vars_start <- names(tb)[grepl(pattern = paste0(var,'.*'),x = names(tb))]
        var_agg <- vars_start[1]
        tb[var] <- 0
        
        for(col in vars_start){
            
            if(sum(!is.na(unique(tb[[col]],na.rm=F)))==3){
                
                tb[col] <- ifelse(tb[[col]]==2,-1,
                                     ifelse(tb[[col]]==99,0,1))
            }else{
                
                tb[col] <- ifelse(tb[[col]]==2,0,1)
                
            }
            
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
    
    tb[,'LN2_RIndLngBEOth_missing'] <- ifelse(is.na(tb$LN2_RIndLngBEOth),1,0)
    tb$LN2_RIndLngBEOth <- NULL
    
    tb[,'LN2_WIndLngBEOth_missing'] <- ifelse(is.na(tb$LN2_WIndLngBEOth),1,0)
    tb$LN2_WIndLngBEOth <- NULL
    
    tb[,'DL12_missing'] <- ifelse(is.na(tb$DL12),1,0)
    tb[,'DL13_missing'] <- ifelse(is.na(tb$DL13),1,0)
    tb[,c('DL12','DL12_OTHERS','DL13')] <- NULL
    
    tb[,'female_head_spouse'] <- ifelse(tb$DL15==4,0,1)
    tb[,'DL15'] <- NULL
    
    tb[,'cant_possessions'] <- ifelse(tb$DL16==2,0,1) + ifelse(tb$DL17==2,0,1) +
        ifelse(tb$DL18==2,0,1) + ifelse(tb$DL19==2,0,1) + ifelse(tb$DL20==2,0,1) +
        ifelse(tb$DL21==2,0,1) + ifelse(tb$DL22==2,0,1) + ifelse(tb$DL23==2,0,1) 
    
    tb[,paste0('DL',seq(16,23))] <- NULL
    
    tb[,'necesidades_basicas'] <- ifelse(tb$DL24 %in% 1:3,1,0)
    
    # situaciones dificiles DL25
    
    tb[,'DL27_missing'] <- ifelse(is.na(tb$DL27),1,0)
    tb[,'DL28_missing'] <- ifelse(is.na(tb$DL28),1,0)
    
    tb[,'spouse_decide_cel'] <- ifelse(tb$MT1A==2,1,0)
    tb[,'hijo_decide_cel'] <- ifelse(tb$MT1A==4,1,0)
    tb[,'sister_decide_cel'] <- ifelse(tb$MT1A==8,1,0)
    
    tb[,'MT1A'] <- NULL
    
    tb[,'tiene_cel'] <- ifelse(tb$MT2==2,0,1)
    tb[,'MT2'] <- NULL
    
    # MT4: caracteristicas del celular.
    
    tb[,'MT5'] <- NULL
    
    tb[,'cel_regalo_cel'] <- ifelse(tb$MT6==2,1,0)
    tb[,'MT6'] <- NULL
    
    tb[,c('MT6A','MT7')] <- NULL
    
    tb[,'no_tengo_a_quien_llamar'] <- ifelse(tb$MT9==2,1,0)
    tb[,'no_permiso_usar_cel'] <- ifelse(tb$MT9==3,1,0)
    tb[,'uso_cel_contra_religion'] <- ifelse(tb$MT9==6,1,0)
    tb[,'sin_dinero_para_cel'] <- ifelse(tb$MT9==7,1,0)
    tb[,'no_necesito_cel'] <- ifelse(tb$MT9==10,1,0)
    
    tb[,c('MT6A')] <- NULL
    
    tb[,'sim_activo'] <- ifelse(tb$MT10==2,1,0)
    tb[,c('MT10')] <- NULL
    
    tb[,'MT11_missing'] <- ifelse(is.na(tb$MT11),1,0)
    tb[,c('MT11')] <- NULL
    
    tb[,'opinion_mb_provider'] <- ifelse(is.na(tb$MT14C_1),1,0)
    tb[,c('MT14C_1','MT14C_2','MT14C_3','MT14C_4')] <- NULL
    
    tb[,'sim_de_otros'] <- ifelse(tb$MT15==2,0,1)
    tb[,'MT15'] <- NULL
    
    # MT17_1 - MT17_13 Variables ordinales. No treatment
    
    # Features about bank:
    
    tb[,'titular_cuenta_banca'] <- ifelse(tb$FF1==2,0,1)
    tb[,'FF1'] <- NULL
    
    tb[,'otro_usa_tu_cuenta'] <- ifelse(tb$FF2==2,1,0)
    tb[,'cuenta_pagos_gobierno'] <- ifelse(tb$FF2A==2,1,0)
    tb[,'cuenta_requeri_organi'] <- ifelse(tb$FF2A==4,1,0)
    tb[,'cuenta_alguien_lopidio'] <- ifelse(tb$FF2A==7,1,0)
    tb[,'cuenta_pago_recibos'] <- ifelse(tb$FF2A==8,1,0)
    
    tb[,c('FF2','FF2A')] <- NULL
    
    tb[,'no_sabe_como_abrir_cuenta'] <- ifelse(tb$FF3==2,1,0)
    tb[,'no_sabe_xq_no_abre_cuenta'] <- ifelse(tb$FF3==99,1,0)
    tb[,c('FF3')] <- NULL
    
    tb[,'usas_cuenta_de_otro'] <- ifelse(tb$FF4==2,0,1)
    tb[,'FF5_missing'] <- ifelse(is.na(tb$FF5),1,0)
    
    tb[,c('FF4','FF5')] <- NULL
    
    # FF16_1, FF16_2 Variable ordinal, según horizonte de tiempo. No treatment
    
    tb[,'conoce-mobile_money'] <- ifelse(tb$MM1==2,0,1)
    tb[,'MM1'] <- NULL
    
    # IFI14_1 al IFI14_7, ordinales, No treatment
    
    # Comportamiento de transacción hacia el banco:
    
    tb[,'ir_banco_caminar'] <- ifelse(tb$IFI16_1==1,1,0)
    tb[,'ir_banco_missing'] <- ifelse(is.na(tb$IFI16_1),1,0)
    tb[,'ir_banco_nosabe'] <- ifelse(tb$IFI16_1==99,1,0)
    
    tb[,'ir_ATM_caminar'] <- ifelse(tb$IFI16_2==1,1,0)
    tb[,'ir_ATM_missing'] <- ifelse(is.na(tb$IFI16_2),1,0)
    tb[,'ir_ATM_nosabe'] <- ifelse(tb$IFI16_2==99,1,0)
    
    tb[,c('IFI16_1','IFI16_2')] <- NULL
    
    # IFI17_1 y IFI17_2, preguntas con respuestas escala ordinal. No treatment
    
    tb[,'grupo_ahorro'] <- ifelse(is.na(tb$IFI24),1,0)
    tb[,'no_grupo_ahorro_no_conoce'] <- ifelse(tb$IFI24==4,1,0)
    
    # tb[,'IFI24'] <- NULL
    
    # Manejo/Gestión del dinero.
    # FL1,FL2 variables escala de tiempo ordinal. No treatment
    
    tb[,'FL3_missing'] <- ifelse(is.na(tb$FL3),1,0)
    tb[,'FL3'] <- NULL
    
    tb[,'depende_esposo_cons_financ'] <- ifelse(tb$FL4==2,1,0)
    tb[,'depende_nosabe_cons_financ'] <- ifelse(tb$FL4==99,1,0)
    #tb[,'FL4'] <- NULL
    
    tb[,'gasto_menor_ingresos'] <- ifelse(tb$FL6_1==2,0,1)
    tb[,'fondo_emergencia'] <- ifelse(tb$FL6_2==2,0,1)
    tb[,'pago_bill_atiempo'] <- ifelse(tb$FL6_3==2,0,1)
    tb[,'ahorro_mayor_deuda'] <- ifelse(tb$FL6_4==2,0,1)
    
    tb[,c('FL6_1','FL6_2','FL6_3','FL6_4')] <- NULL
    
    # Opiniones gestión financieras: Acuerdo - desacuerdo
    # FL8_1 al FL8_7 no treatment -> escala ordinal.
    
    # FL9A,FL9B,FL9C,FL10  mean_encoding
    
    # FL11 NO TREATMENT
    
    # PREGUNTAS DE CALCULO ARITMETICO
    tb[,'FL12'] <- ifelse(tb$FL12==2,-1,ifelse(tb$FL12==99,0,1))
    tb[,'FL13'] <- ifelse(tb$FL13==2,-1,ifelse(tb$FL13==99,0,1))
    tb[,'FL14'] <- ifelse(tb$FL14==2,-1,ifelse(tb$FL14==99,0,1))
    tb[,'FL15'] <- ifelse(tb$FL15==2,-1,ifelse(tb$FL15==99,0,1))
    tb[,'FL16'] <- ifelse(tb$FL16==2,-1,ifelse(tb$FL16==99,0,1))
    tb[,'FL17'] <- ifelse(tb$FL17==2,-1,ifelse(tb$FL17==99,0,1))
    tb[,'FL18'] <- ifelse(tb$FL18==2,-1,ifelse(tb$FL18==99,0,1))
    tb[,'total_ev_arit'] <- tb$FL12 + tb$FL13 + tb$FL14 + tb$FL15 +
        tb$FL16 + tb$FL17 + tb$FL18
    
    # FB1_1 AL FB1_2, flags, no treatment:
    # tb[,'FB2'] <- NULL
    
    tb['necesito_cred_anteriormente'] <- ifelse(tb$FB3==2,0,1)
    tb[,'FB3'] <- NULL
    
    tb['pago_loans_antes_deadline'] <- ifelse(tb$FB18==1,1,0)
    tb['nopido_loans'] <- ifelse(tb$FB18==5,1,0)
    # tb[,'FB18'] <- NULL
    
    # Pregunta para el perfil:
    tb[,'gasto_loans_medicine'] <- ifelse(tb$FB19==1,1,0)
    # tb[,'FB19'] <- NULL
    
    # FB20: mean_encoding
    
    tb[,'conoce_tasa_interes'] <- ifelse(tb$FB23_1==2,0,1)
    tb[,'FB23_1'] <- NULL
    
    # FB24: mean_encoding
    
    # EVALUACION LINGUISTICA: LN
    
    # LN1A,LN1B escala ordinal, no treatment:
    
    # LN2_1 AL LN2_4 escala ordinal, no treatment:
    
    # PREGUNTAS SOBRE DECISIONES EN EL HOGAR
    
    tb[,'decide_gasto_esposo'] <- ifelse(tb$GN1==2,1,0)
    tb[,'decide_gasto_otro'] <- ifelse(tb$GN1==96,1,0)
    tb[,'decide_gasto_missing'] <- ifelse(is.na(tb$GN1),1,0)
    
    tb[,'decide_gastodiario_esposo'] <- ifelse(tb$GN2==2,1,0)
    tb[,'decide_gastodiario_otro'] <- ifelse(tb$GN1==96,1,0)
    
    tb[,'controla_props_esposo'] <- ifelse(tb$GN3==2,1,0)
    tb[,'controla_props_otro'] <- ifelse(tb$GN3==96,1,0)
    
    tb[,'decide_finanserv_esposo'] <- ifelse(tb$GN4==2,1,0)
    tb[,'decide_finanserv_otro'] <- ifelse(tb$GN4==96,1,0)
    
    tb[,'decide_finanservp_esposo'] <- ifelse(tb$GN5==2,1,0)
    tb[,'decide_finanservp_otro'] <- ifelse(tb$GN5==96,1,0)
    
    tb[,'decsiones_tot_esposo'] <- ifelse(is.na(tb$decide_gasto_esposo),0,tb$decide_gasto_esposo) + 
        ifelse(is.na(tb$decide_gastodiario_esposo),0,tb$decide_gastodiario_esposo) + 
        ifelse(is.na(tb$controla_props_esposo),0,tb$controla_props_esposo) + 
        ifelse(is.na(tb$decide_finanserv_esposo),0,tb$decide_finanserv_esposo) +
        ifelse(is.na(tb$decide_finanservp_esposo),0,tb$decide_finanservp_esposo)
    
    # DG8a - DG9c variables numéricas.
    
    return(tb)
}

train_feat <- feature_engineering(train) 
test_feat <- feature_engineering(test) 

# Seleccionar columnas con un % de missing menor a 50%
cols_tol_miss <- names(train_feat)[colSums(is.na(train_feat))/nrow(train_feat)<0.7]
cols_tol_miss_train <- cols_tol_miss
cols_tol_miss_test <- c('test_id',setdiff(cols_tol_miss,c('train_id','is_female')))

train_feat <- train_feat[,cols_tol_miss_train]
test_feat <- test_feat[,cols_tol_miss_test]

library(data.table)

# transform datasets
setDT(train_feat)
setDT(test_feat)

# Mean Encoding:
mean_encoding <- function(train,test,cat_vars,target,add_freq_enc=F){
    
    for(var in cat_vars){
        
        df_mean <- train[,c(var,target),with=F][,.(suma=sum(get(target))/nrow(train)),by=get(var)]
        colnames(df_mean) <- c(var,paste0('mean_',var))
        
        train <- data.table(dplyr::left_join(train,df_mean,by=var))
        test <- data.table(dplyr::left_join(test,df_mean,by=var))
        
        if(add_freq_enc){
            
            df_count <- train[,c(var,target),with=F][,.(cuenta=.N/nrow(train)),by=get(var)]
            colnames(df_count) <- c(var,paste0('freq_',var))
            
            train <- data.table(dplyr::left_join(train,df_count,by=var))
            test <- data.table(dplyr::left_join(test,df_count,by=var))
            
        }
        
    }
    
    train[,(cat_vars):=NULL]
    test[,(cat_vars):=NULL]
    
    return(list(train,test))
}


cat_feat <- c('AA3','DG3','DG3A','IFI24','FL4','FL9A','FL9B','FL9C','FL10','FB2','FB18','FB19','FB20',
              'FB24','LN1A','LN1B')

train_feat2 <- copy(train_feat)
test_feat2 <- copy(test_feat)

data_sets <- mean_encoding(train_feat,test_feat,cat_feat,'is_female',add_freq_enc = T)

train_feat <- data_sets[[1]]
test_feat <- data_sets[[2]]

fwrite(train_feat,'./dataset_training/train_feat.csv')
fwrite(test_feat,'./dataset_training/test_feat.csv')
