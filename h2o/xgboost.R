# Train & Cross-validate a (shallow) XGB-GBM
source("h2o-utils.R")
source("xgb-config.R")

#############################
###### LightGBM model  ######
#############################

list[search_criteria.lightgbm.default, hyper_params.lightgbm.default] =  lightgbm.hyper_params_search_crit()
list[search_criteria.lightgbm.depth, hyper_params.lightgbm.depth] =  lightgbm.hyper_params_search_crit.explore.depth()

baseline.lightgbm.helper = function(XY, X, independentCols, YName, keep_cvpreds=T) {
    h2o.xgboost(
               x = independentCols,
               y = YName,
               training_frame=XY,
               validation_frame=X,
               distribution = "gaussian",
               tree_method="hist",
               grow_policy="lossguide",
               ntrees = 5000,
               max_depth = 12,
               min_rows = 1,
               learn_rate = 0.02,
               nfolds = 10,
               fold_assignment = "Modulo",
               stopping_rounds=10,
               stopping_tolerance=1e-5,
               stopping_metric="RMSE",
               keep_cross_validation_predictions = keep_cvpreds,
               seed = 123456)

} 

baseline.lightgbmboost = function(
                    XY, YName="logerror", 
                    independentCols = setdiff(colnames(XY), YName)) {
    set.seed(123456)
    list[XTrain, YTrain, XHoldout, YHoldout] = splitTrainingWrapper(
                        XY, split_percent=0.90, YName="logerror") 
    XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o")
    XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o")
    my_xgb <- baseline.lightgbm.helper(XYTrain.h2o, XYTest.h2o, independentCols, YName, keep_cvpreds=keep_cvpreds)
    return(my_xgb)
}

lightgbm.grid = function(
                    XY, YName="logerror", 
                    independentCols = setdiff(colnames(XY), YName), 
                    hyper_params=hyper_params.lightgbm.default, 
                    search_criteria=search_criteria.lightgbm.default, 
                    grid_id="depth_grid",
                    keep_cvpreds=T) {

    cat(paste(hyper_params, sep=":", collapse=", \n"))
    print("")

    set.seed(123456)
    list[XTrain, YTrain, XHoldout, YHoldout] = splitTrainingWrapper(
                        XY, split_percent=0.90, YName=YName) 
    XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o")
    XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o")
    h2o.grid.helper(XYTrain.h2o, 
                    XYTest.h2o, 
                    independentCols, 
                    YName, 
                    hyper_params, 
                    search_criteria,
                    grid_id, 
                    algorithm="xgboost",
                    keep_cvpreds=keep_cvpreds,
                    tree_method="hist",
                    grow_policy="lossguide")
}



#######################################################
################ XGB routines #########################
######################################################
list[search_criteria.xgb.default, hyper_params.xgb.default] =  xgb.hyper_params_search_crit()
list[search_criteria.xgb.depth, hyper_params.xgb.depth] =  xgb.hyper_params_search_crit.explore.depth()

baseline.xgboost.helper = function(
                   XY, X, independentCols, YName, 
                   booster="gbtree", 
                   ntrees = 5000,
                   max_depth = 12,
                   min_rows = 1,
                   sample_rate=0.8,
                   col_sample_rate=0.8,
                   col_sample_rate_per_tree=0.8,
                   learn_rate = 0.02,
                   reg_lambda=0.8,
                   reg_alpha=0.4,
                   nfolds = 10,
                   keep_cvpreds=T,
                   ...) {
    h2o.xgboost(
               x = independentCols,
               y = YName,
               training_frame = XY,
               validation_frame=X,
               distribution = "gaussian",
               ntrees=ntrees,
               max_depth=max_depth,
               min_rows=min_rows,
               sample_rate=sample_rate,
               col_sample_rate=col_sample_rate,
               col_sample_rate_per_tree=col_sample_rate_per_tree,
               learn_rate=learn_rate,
               nfolds=nfolds,
               reg_lambda=reg_lambda,
               reg_alpha=reg_alpha,
               fold_assignment = "Modulo",
               stopping_rounds=10,
               stopping_tolerance=1e-5,
               stopping_metric="RMSE",
               booster=booster,
               keep_cross_validation_predictions = keep_cvpreds,
               seed = 123456,
               ...)

} 

baseline.xgboost = function(
                    XY, YName="logerror", 
                    independentCols = setdiff(colnames(XY), YName),
                    booster="gbtree", 
                    keep_cvpreds=T,
                    train.full=F,
                    ...) {
    set.seed(123456)
    if(train.full==F) {
        list[XTrain, YTrain, XHoldout, YHoldout] = splitTrainingWrapper(
                            XY, split_percent=0.90, YName=YName) 
        XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o")
        XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o")
        my_xgb <- baseline.xgboost.helper(
                                  XYTrain.h2o, XYTest.h2o, independentCols, YName, 
                                  booster=booster, keep_cvpreds=keep_cvpreds,...)
    } else {
        XYTrain.h2o = as.h2o(x = XY, destination_frame="XTrain.h2o")
        my_xgb <- baseline.xgboost.helper(
                                  XYTrain.h2o, NULL, independentCols, YName, 
                                  booster=booster, keep_cvpreds=keep_cvpreds,...)
    }
    my_xgb <- baseline.xgboost.helper(
                              XYTrain.h2o, XYTest.h2o, independentCols, YName, 
                              booster=booster, keep_cvpreds=keep_cvpreds,...)
    # Train & Cross-validate another (deeper) XGB-GBM
    return(my_xgb)
}


xgb.grid = function(
                    XY, YName="logerror", 
                    independentCols = setdiff(colnames(XY), YName), 
                    hyper_params=hyper_params.xgb.default, 
                    search_criteria=search_criteria.xgb.default, 
                    booster="gbtree",
                    keep_cvpreds=T,
                    grid_id="depth_grid") {

    print(paste(hyper_params, sep=":", collapse=", "))

    set.seed(123456)
    list[XTrain, YTrain, XHoldout, YHoldout] = splitTrainingWrapper(
                        XY, split_percent=0.90, YName="logerror") 
    XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o")
    XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o")
    h2o.grid.helper(XYTrain.h2o, XYTest.h2o, independentCols, YName, 
                    hyper_params=hyper_params, 
                    search_criteria=search_criteria, 
                    algorithm="xgboost",
                    grid_id, 
                    keep_cvpreds=keep_cvpreds,
                    booster=booster)
}

