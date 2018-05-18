
# Paquetes:

## Paralelizaciòn:
# install.packages('parallel')

## Manipulaciòn
# install.packages('data.table')
# install.packages('dplyr')

## Texto
# install.packages('stringr')

# Fechas:
# install.packages('lubridate')


library(data.table)

rcc <- fread('INFORMACION_RCC.csv')

# Funcion adicionar:

add_periodo <- function(periodo,n){
  vr <- periodo%%100 + n
  periodo + n + 88*(floor(vr/12.1)+ifelse(vr%%12==0&n<0,-1,0))
}


# Función vectorizada:

cosechas <- function(data,id='CODIGOCLIENTE',period='MES',vars_cont,histo=3,salto=1){
  
  library(data.table)
  
  columns_s <- c(period,id,vars_cont)
  
  dta <- data[,columns_s,with = F]
  
  dta[,vars_cont] <- lapply(dta[,vars_cont,with=F],function(x) as.numeric(x))
  
  dta[,period] <- as.numeric(dta[[period]])
  
  dta$ini <- add_periodo(dta[[period]],-(salto+1))
  dta$fin <- add_periodo(dta[[period]],-(salto+histo))
  
  m <- length(vars_cont)
  prex <- c(rep('SUM_',m),rep('MEAN_',m),rep('SD_',m),rep('COUNT_',m))
  names_vars <- paste(prex,vars_cont,sep = '')
  
  for(name in names_vars){
    dta[,name] <- 0.0  
  }
  
  # Función agregadora
  agg <- function(y){
    
    vals <- dta[get(id)==dta[y,get(id)] & 
                  data.table::between(get(period),dta[y,fin],dta[y,ini]),vars_cont,with=F]
    
    
    vals2 <- vals[,c(lapply(.SD,sum,na.rm=T),
                     lapply(.SD,mean,na.rm=T),
                     lapply(.SD,sd,na.rm=T),
                     lapply(.SD,function(x)sum(!is.na(x))))]
    
    #ifelse(length(vals)==0,0,vals2)
    
    if(nrow(vals)==0){
      dtf <- setNames(data.table(matrix(rep(0.0,m*4),nrow = 1, ncol = m*4)), names_vars)
    }else{
      dtf <- vals2
    }
    
    return(dtf)
  }
  
  # data_fin <- setNames(data.table(matrix(nrow = 0, ncol = m*4)), names_vars)
  # 
  # for(i in 1:nrow(dta)){
  #   data_fin <- rbindlist(list(data_fin,agg(i)))
  #   #dta[i,names_vars] <- agg(i)
  # }
  
  no_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(no_cores)
  
  parallel::clusterExport(cl,'data.table')
  
  # data_fin <- data.table(t(sapply(1:nrow(dta),agg)))
  
  data_fin <- data.table(t(parallel::parSapply(cl,1:nrow(dta),agg)))
  
  stopCluster(cl)
  
  return(data_fin)
  
}


# Medición de tiempo corrida:
ini <- Sys.time()
final <- cosechas(data = rcc[1:10000,],id = 'CODIGOCLIENTE',period = 'MES',
                  vars_cont = c('CAP_ENDEU'))
fin <- Sys.time()

fin-ini


