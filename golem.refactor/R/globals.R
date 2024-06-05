library(parsnip) # Necessary for predict to work with XGBoost model

lm_modl <- readRDS(app_sys("models/lm.RDS"))
xgb_modl <- readRDS(app_sys("models/xgb.RDS"))
