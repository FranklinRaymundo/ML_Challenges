
# Importing libraries
library(xgboost)
library(dplyr)

# Loading train/test sets
train<-data.table::fread("./dataset_training/train_feat.csv")
test<-data.table::fread("./dataset_training/test_feat.csv")
train<-as.data.frame(train)
test<-as.data.frame(test)

# Separate data
target_name <- 'is_female'
target <- train[[target_name]]
train[,target_name] <- NULL
train[,'train_id'] <- NULL
test_id <-  test$test_id
test[,'test_id'] <- NULL

# Generamos la matrix entrenamiento tipo para el xgb.train y el xgb.test
xgb_train <- xgb.DMatrix(data=as.matrix(train),label=target,missing=NA)
xgb_test <- xgb.DMatrix(data=as.matrix(test),missing=NA)

watchlist <- list(train=xgb_train)

### Tuneo de parámetros
param_grid_cv <- expand.grid(
  nfold=5,
  max_depth=c(3,6,9),
  eta=c(.05,.1,.15,.2),
  nrounds=c(100,150),
  train_auc_mean=NA,
  test_auc_mean=NA,
  nronda_cv=NA,
  n_ronda_mean_sd_test=NA,
  dif_train_test=NA)


# Tuning parameters K-Fold = 5
start_time=Sys.time()  
for (i in 1:nrow(param_grid_cv)) {

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
  )  

  set.seed(1)

  fit <- xgb.cv(data=xgb_train,
                params=param,
                nrounds=param_grid_cv$nrounds[i],
                nfold=param_grid_cv$nfold[i],
                missing=NA)
  
  param_grid_cv$n_ronda_mean_sd_test[i] <- which.max(fit$evaluation_log[,test_auc_mean]-fit$evaluation_log[,test_auc_std])
  param_grid_cv$test_auc_mean[i] <- max(fit$evaluation_log[,test_auc_mean])
  param_grid_cv$train_auc_mean[i] <- fit$evaluation_log[fit$evaluation_log[,test_auc_mean]==param_grid_cv$test_auc_mean[i],train_auc_mean]
  param_grid_cv$nronda_cv[i] <- which.max(fit$evaluation_log[,test_auc_mean])
  param_grid_cv$dif_train_test[i]<-param_grid_cv$train_auc_mean[i]-param_grid_cv$test_auc_mean[i]
  print(cat(paste0('El mayor auc es en la iteración: ', param_grid_cv$n_ronda_mean_sd_test[i], '\n')))
  

  print("---------------------------------------------------------------------------------------")
}

######## 9- COMBINACION DE PARAMETROS PARA MEJOR MODELO #########
param_xgb_final<-list(objective="binary:logistic",
                      max_depth=6,
                      min_child_weight=1,
                      gamma=0,
                      subsample=.8,
                      colsample_bytree=.6,
                      eta=0.05,
                      max_delta_step=0,
                      eval_metric="auc")

set.seed(1)
fit_xgb_final<- xgb.train(data=xgb_train,
                          params=param_xgb_final,
                          #nthread=7,
                          watchlist=watchlist,
                          nrounds=150,
                          missing=NA)

# # Save model
# xgb_model <- xgb.save.raw(fit_xgb_final)
# saveRDS(xgb_model,'./predictive_models/xgb_model.Rda')

# Submission
preds <- predict(fit_xgb_final,xgb_test)
submit <- data.frame(test_id=test_id,is_female=preds)
data.table::fwrite(submit,'./submissions/submit_xgboost_all_feat_mean.csv')

# Create test_explanation

explainer_xgb <- lime::lime(train,fit_xgb_final)

saveRDS(explainer_xgb,'explainer_xgb.rda')

# explanation_xgb <- lime::explain(
#     x = test[1:100,],
#     explainer = explainer_xgb,
#     n_permutations = 5000,
#     dist_fun = 'gower',
#     kernel_width = .75,
#     n_features = 10,
#     feature_select = 'highest_weights',
#     labels = '1'
# )
# 
# saveRDS(explanation_xgb,'explanation_xgb.Rda')
