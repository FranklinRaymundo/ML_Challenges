#########################################################
############# CODIGO R PARA MODELAMIENTO ################
#########################################################

######## 1- LIMPIANDO MEMORIA Y SELECCION DE WORKSPACE ########
rm(list=ls())
gc()

setwd("C:/Users/oscar/Desktop/Scripts/R")

# # Desactivamos la notacion cientifica
options(scipen=999)
# # Activamos la notacion cientifica
# options(scipen=0)

######## 2- LIBRERIAS NECESARIAS #######
library(xgboost)
library(Hmisc)
library(data.table)
library(dplyr)
library(sqldf)
library(ggplot2)
library(caret)
library(dummies)
library(lightgbm)
library(MLmetrics)

######## 3- CARGANDO LA DATA: ##########
data_train<-fread("data_train.csv",encoding = "Latin-1")
data_test<-fread("data_test.csv",encoding = "Latin-1")
data_train<-as.data.frame(data_train)
data_test<-as.data.frame(data_test)

# Consolidando la data:
data_train <- mutate(data_train, subset="train")
Target<-data_train$FlagVenta
data_train$FlagVenta<-NULL
data_test <- mutate(data_test, subset="test")

# Uniendo la data de train y test:
df<-bind_rows(data_train,data_test)
rm(data_train,data_test)
gc()
######## 4- LIMPIEZA DE LA DATA ##########
# Separacion del Target del DataFrame:
# Limpieza de data:
# df[df==''] <- NA
# df[df==' '] <- NA
# Separación de Llaves, Variables Numéricas, Variables Categóricas:
# Separando las llaves del Dataframe:
str(df)
vars_keys<-c("Periodo","NumeroDocumento")
df<-df[,!names(df)%in%vars_keys]

# Luego de revisar los tipos de datos, si es necesario cambiar el formato se hara,
# luego separar las categoricas de las numericas, para trabajarlas y hacer limpieza

df$Flag_XL_3UM_Total<-as.factor(df$Flag_XL_3UM_Total)
df$SegmentoRFMTC<-as.factor(df$SegmentoRFMTC)
df$CategoriaUso<-as.factor(df$CategoriaUso)
df$FlagPREST<-as.factor(df$FlagPREST)
df$Flag_SB_MaxEntidad_TC_6<-as.factor(df$Flag_SB_MaxEntidad_TC_6)
df$FlagAdicionales<-as.factor(df$FlagAdicionales)
df$Max_CicloFacturacion_1<-as.factor(df$Max_CicloFacturacion_1)
df$SegmentoSSFFTC<-as.factor(df$SegmentoSSFFTC)

# Variables Categoricas:
vars_cat<-names(rapply(df, class=c("character","factor"), f=class, how="unlist"))
data_cat<-df[,names(df)%in%vars_cat]
data_cat <- mutate_if(data_cat,is.character,as.factor)
vars_cat <- vars_cat[vars_cat != "subset"]

# Variables Numericas:
vars_num<-names(rapply(df, class=c("numeric","integer"), f=class, how="unlist"))
data_num<-df[,names(df)%in%vars_num]

######## 5- ANALISIS EXPLORATORIO ########
# Tratamiento de variables Numericas:
# a) Removiendo variables que tienen alto % de missing (> a 70%)
var_missing<-colnames(data_num)[colSums(is.na(data_num)) > dim(data_num)[1]*0.7]
data_num<-data_num[,!names(data_num)%in%var_missing]

# b) Removiendo variables con variabilidad cercana a 0
var_zero = nearZeroVar(data_num, names=TRUE)
data_num = data_num[,!names(data_num) %in% var_zero]

# Tratamiento de variables Categoricas:
# Se utilizará una plantilla en excel, para hacer la sintaxis para todas las categóricas:
# Frecuencias_Categoricas.xls (Plantilla en excel)
View(as.data.frame(vars_cat))
data_cat<-add_count(data_cat,subset,Flag_XL_3UM_Total)%>%rename("n_Flag_XL_3UM_Total"=n)
data_cat<-add_count(data_cat,subset,SegmentoRFMTC)%>%rename("n_SegmentoRFMTC"=n)
data_cat<-add_count(data_cat,subset,CategoriaUso)%>%rename("n_CategoriaUso"=n)
data_cat<-add_count(data_cat,subset,CreditCardType_1)%>%rename("n_CreditCardType_1"=n)
data_cat<-add_count(data_cat,subset,Macrozona)%>%rename("n_Macrozona"=n)
data_cat<-add_count(data_cat,subset,Genero)%>%rename("n_Genero"=n)
data_cat<-add_count(data_cat,subset,FlagPREST)%>%rename("n_FlagPREST"=n)
data_cat<-add_count(data_cat,subset,Flag_SB_MaxEntidad_TC_6)%>%rename("n_Flag_SB_MaxEntidad_TC_6"=n)
data_cat<-add_count(data_cat,subset,SituacionLaboral)%>%rename("n_SituacionLaboral"=n)
data_cat<-add_count(data_cat,subset,FlagAdicionales)%>%rename("n_FlagAdicionales"=n)
data_cat<-add_count(data_cat,subset,Max_CicloFacturacion_1)%>%rename("n_Max_CicloFacturacion_1"=n)
data_cat<-add_count(data_cat,subset,SegmentoSSFFTC)%>%rename("n_SegmentoSSFFTC"=n)

# Creando variables con el conteo de las frecuencias: (PARA REVISAR)
# for (i in 1:length(vars_cat)){
# data_cat<-add_count(data_cat,subset,vars_cat[[i]])%>%rename(paste0("n_",vars[[i]]==n))
# }


# Revisando los niveles de las categóricas:
# Guardamos los niveles de las variables categoricas del train:
niveles_vcat_train <- list()
for (i in 1:length(vars_cat)) {
  niveles_vcat_train[[i]] <- levels(factor(df[df$subset=="train",][[vars_cat[i]]]))
}
names(niveles_vcat_train) <- vars_cat
niveles_vcat_train
#save(niveles_vcat_train,file="niveles_vcat_train.RData")

# Verificamos que los niveles en el test sean los mismos:
niveles_vcat_test <- list()
for (i in 1:length(vars_cat)) {
  niveles_vcat_test[[i]] <- levels(factor(df[df$subset=="test",][[vars_cat[i]]]))
}
names(niveles_vcat_test) <- vars_cat
niveles_vcat_test
#save(niveles_vcat_test,file="niveles_vcat_test.RData")

# Corregimos los niveles del test asignando NA a los registros que tengan algun
# nivel que no este en los originales
for (i in 1:length(niveles_vcat_test)) {
  noestan <- niveles_vcat_test[[i]][!(niveles_vcat_test[[i]] %in% niveles_vcat_train[[i]])]
  if (length(noestan)>0) {
    df[df$subset=='test' & df[[vars_cat[i]]] %in% noestan,vars_cat[[i]]] <- NA
  }
}

# Guardamos los niveles de las variables categoricas
niveles_vcat_train <- list()
for (i in 1:length(vars_cat)) {
  niveles_vcat_train[[i]] <- levels(factor(df[df$subset=="train",][[vars_cat[i]]]))
}
names(niveles_vcat_train) <- vars_cat
save(niveles_vcat_train,file="niveles_vcat_train.RData")

# Verificamos que los niveles en la data test sean los mismos
niveles_vcat_test <- list()
for (i in 1:length(vars_cat)) {
  niveles_vcat_test[[i]] <- levels(factor(df[df$subset=="test",][[vars_cat[i]]]))
}
names(niveles_vcat_test) <- vars_cat

rm(niveles_vcat_test,niveles_vcat_train,i,noestan)
gc()

#####################################################################
#################### TRATAMIENTO DE OUTLIERS ########################
#####################################################################
# Se realizará algunos exploratorios, de tal manera de revisar outliers
# Además, el tratamiento que se realizará será acotando los extremos con percentiles
# percentil 1-99 o  2-98


#################################################
###### Obtención de variables finales:###########
#################################################
df<-data.frame(data_num,data_cat)
rm(data_cat,data_num)
gc()

####################################################################
################### 5- FEATURE ENGINEERING #########################
####################################################################

# Realizar exploratorio con la data:

# Cortes de las variables numéricas, y combinación de cosas para crear nuevas:
# Número de grupos, esto suponiendo que lo cortaremos en partes iguales de 5
data_final<-mutate(data_final,VARX=cut2(data_final$varx,g=5)) 

# En caso de que en el exploratorio se vean otras cosas, utilizar lo siguiente para crear nuevas:
# De acuerdo a algunas condiciones:
data$varx[data$var1==1&data$var2]<-'UNDERGRAD'
data$varx[data$var1==1&data$var2]<-'UNDERGRAD'
data$varx[data$var1==1&data$var2]<-'UNDERGRAD'
data$varx[data$var1==1&data$var2]<-'UNDERGRAD'

# Construcción de variables por ratios:
df$varxnew<-ifelse(condicion1,valororiginal,casocontrario)

######## 6- PREPARANDO LA DATA PARA XGBOOST #######
# Construcción de Dummies:
# En vars_cat, se puede poner solo las variables que se quieran dummisear
# En caso que no tenga muchos niveles, se pueden poner como dummies todas, en otro caso
# Solo seleccionar el conteo, y las que tengan menos de 10 niveles, ponerlas como dummies.
df <- dummy.data.frame(df, names = vars_cat , sep = "_")
train <- filter(df,subset=="train")
train$subset<-NULL
test <- filter(df,subset=="test")
test$subset<-NULL
rm(df)
gc()

# Guardando el nombre de las variables del dataset:
vars_in <- colnames(train)

# Generamos la matrix entrenamiento tipo para el xgb.train y el xgb.test
xgb_train <- xgb.DMatrix(data=as.matrix(train),label=Target,missing=NA)
xgb_test <- xgb.DMatrix(data=as.matrix(test),missing=NA)

# Guardamos las matrices en una lista para obtener el valor de la metrica mientras se ejecuta el algoritmo:
watchlist <- list(train=xgb_train)

######## 8- TUNEO DE PARAMETROS (CROSS VALIDATION) ############
### Tuning de parametros (auc)
param_grid_cv <- expand.grid(
  nfold=5,
  max_depth=c(2,4),
  eta=c(.05,.1,.15,.2),
  nrounds=c(50,100,150),
  train_auc_mean=NA,
  test_auc_mean=NA,
  nronda_cv=NA,
  n_ronda_mean_sd_test=NA,
  dif_train_test=NA)

### Tuning de parametros (logloss)
# param_grid_cv <- expand.grid(
#   nfold=5,
#   max_depth=c(2,3,4),
#   eta=c(.1,.15,0.2),
#   nrounds=c(100,150,200),
#   train_logloss_mean=NA,
#   test_logloss_mean=NA,
#   nronda_cv=NA,
#   n_ronda_mean_sd_test=NA,
#   dif_train_test=NA)

#Con el K Folds:   # Data con 106398 registros con 67 variables:
start_time=Sys.time()  
for (i in 1:nrow(param_grid_cv)) {
  # Fijamos los parametros
  param <- list(
    objective="binary:logistic",
    max_depth=param_grid_cv$max_depth[i],
    min_child_weight=1,
    gamma=0,
    subsample=.8,
    colsample_bytree=.8,
    eta=param_grid_cv$eta[i],
    max_delta_step=0,
    eval_metric="auc"
    #eval_metric="logloss"
  )  # otras metricas: auc, error, logloss
  # Semilla para la reproductibilidad
  set.seed(1)
  # Modelo
  fit <- xgb.cv(data=xgb_train,
                params=param,
                #print.every.n = 10,
                nrounds=param_grid_cv$nrounds[i],
                #nthread=3,
                #watchlist=watchlist,
                nfold=param_grid_cv$nfold[i],
                #label=target_tr,
                missing=NA)
  
  # En caso que sea AUC:
  param_grid_cv$n_ronda_mean_sd_test[i] <- which.max(fit$evaluation_log[,test_auc_mean]-fit$evaluation_log[,test_auc_std])
  param_grid_cv$test_auc_mean[i] <- max(fit$evaluation_log[,test_auc_mean])
  param_grid_cv$train_auc_mean[i] <- fit$evaluation_log[fit$evaluation_log[,test_auc_mean]==param_grid_cv$test_auc_mean[i],train_auc_mean]
  param_grid_cv$nronda_cv[i] <- which.max(fit$evaluation_log[,test_auc_mean])
  param_grid_cv$dif_train_test[i]<-param_grid_cv$train_auc_mean[i]-param_grid_cv$test_auc_mean[i]
  print(cat(paste0('El mayor auc es en la iteración: ', param_grid_cv$n_ronda_mean_sd_test[i], '\n')))
  
  # En caso que sea Log-loss:
  # param_grid_cv$n_ronda_mean_sd_test[i] <- which.min(fit$evaluation_log[,test_logloss_mean]-fit$evaluation_log[,test_logloss_std])
  # param_grid_cv$test_logloss_mean[i] <- min(fit$evaluation_log[,test_logloss_mean])
  # param_grid_cv$train_logloss_mean[i] <- fit$evaluation_log[fit$evaluation_log[,test_logloss_mean]==param_grid_cv$test_logloss_mean[i],train_logloss_mean]
  # param_grid_cv$nronda_cv[i] <- which.min(fit$evaluation_log[,test_logloss_mean])
  # param_grid_cv$dif_train_test[i]<-param_grid_cv$test_logloss_mean[i]-param_grid_cv$train_logloss_mean[i]
  # print(cat(paste0('El minimo logloss es en la iteración: ', param_grid_cv$n_ronda_mean_sd_test[i], '\n')))
  
  print("---------------------------------------------------------------------------------------")
}

######## 9- COMBINACION DE PARAMETROS PARA MEJOR MODELO #########
param_xgb_final<-list(objective="binary:logistic",
                      max_depth=2,
                      min_child_weight=1,
                      gamma=0,
                      subsample=.8,
                      colsample_bytree=.6,
                      eta=0.1,
                      max_delta_step=0,
                      eval_metric="auc",
                      eval_metric="logloss")

set.seed(1)
fit_xgb_final<- xgb.train(data=xgb_train,
                          params=param_xgb_final,
                          #nthread=7,
                          watchlist=watchlist,
                          nrounds=60,
                          missing=NA)

######## 10- GENERACION DE MEJOR MODELO COMBINANDO SEMILLAS ########
resultados_xgb_ensambled <- as.data.frame(matrix(data=NA,nrow=nrow(data_test),ncol=5))
names(resultados_xgb_ensambled)<-c("predict1","predict2","predict3","predict4","predict5")

k=0
for (j in c(5,seq(10,25,5))){
  k=k+1
  print(j)
  set.seed(j)
  fit_xgb_final_sed<- xgb.train(data=xgb_train,
                                params=param_xgb_final,
                                nthread=7,
                                watchlist=watchlist,
                                nrounds=30,
                                missing=NA)
  resultados_xgb_ensambled[,k]<-predict(fit_xgb_final_sed,data_test)
}

# Promediando los scores de las 5 semillas para submitearlos en la competencia:
resultados_xgb_ensambled$score_ensambled<-apply(resultados_xgb_ensambled,1,mean)
# plot(resultados_xgb_ensambled$predict1,resultados_xgb_ensambled$score_ensambled)
# lines(resultados_xgb_ensambled$predict1,resultados_xgb_ensambled$predict1,col='blue')

# Exportando la data para el submiteo:
fwrite(resultados_xgb_emsambled,"submit_x.csv")

######## 11- VALIDACION DEL MODELO VS UN PERIODO FUERA DEL MODELADO ######


######## 12- DECILEO Y ANALISIS BIVARIADO ##########
#Creando un dataframe con las probabilidades y el target real:
# pred_decil_tr<-data.frame(target_tr,predict_tr)
# 
# #DECILES
# pred_decil_tr<-mutate(pred_decil_tr,G10=cut2(pred_decil_tr$predict_tr,g=10))
# levels(pred_decil_tr$G10)<-c("D10",paste0("D0",9:1))
# table(pred_decil_tr$G10,pred_decil_tr$target_tr)
# 
# #Distribucion por Decil
# prop.table(table(pred_decil_tr$G10,pred_decil_tr$target_tr),1)*100
# #Efectividad por Decil
# prop.table(table(pred_decil_tr$G10,pred_decil_tr$target_tr),2)*100


