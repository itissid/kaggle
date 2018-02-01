# This files only contains encapsulated light GBM and XGBoost parameters used by those models.
# Its easier to keep track with such an arrangement.
# list[xgb.h2o.baseline.na.omit.mean.impute, xgb.linear.h2o.baseline.na.omit.mean.impute] = baseline.xgboost(XY)
xgb.hyper_params_search_crit.explore.depth = function() {
   hyper_params = list( ntrees = 2000,
                         max_depth = seq(1, 10, 1),
                         min_rows = 1,
                         learn_rate = 0.02,
                         sample_rate = 0.8,
                         col_sample_rate = 0.8,
                         col_sample_rate_per_tree = 0.8 
                         #,nbins_cats = nbins_cats_opts
    )
    search_criteria = list(strategy = "RandomDiscrete",
                       max_runtime_secs = 1800,
                       max_models = 200,
                       stopping_metric = "RMSE",
                       stopping_tolerance = 0.00005,
                       stopping_rounds = 5,
                       seed = 123456)
    return(list(search_criteria, hyper_params))
}

xgb.hyper_params_search_crit = function(
            ntrees_opts = 2000,       # early stopping will stop earlier
            max_depth_opts = seq(3, 7, 1), # Best was 4
            min_rows_opts = seq(1, 10, 1),
            learn_rate_opts = seq(0.01, 0.03, 0.003),
            sample_rate_opts = seq(0.3, 0.95, 0.1),
            col_sample_rate_opts = seq(0.55, .9, 0.02),
            col_sample_rate_per_tree_opts = seq(0.65, 0.85, 0.02)) {
            # no categorical features in this dataset
            #nbins_cats_opts = seq(100,10000,100)


    hyper_params = list( ntrees = ntrees_opts,
                         max_depth = max_depth_opts,
                         min_rows = min_rows_opts,
                         learn_rate = learn_rate_opts,
                         sample_rate = sample_rate_opts,
                         col_sample_rate = col_sample_rate_opts,
                         col_sample_rate_per_tree = col_sample_rate_per_tree_opts
                         #,nbins_cats = nbins_cats_opts
    )
    search_criteria = list(strategy = "RandomDiscrete",
                       max_runtime_secs = 3600,
                       max_models = 200,
                       stopping_metric = "RMSE",
                       stopping_tolerance = 0.00005,
                       stopping_rounds = 5,
                       seed = 123456)
    return(list(search_criteria, hyper_params))
}
##############################################
######## Light GBM grid opts
#########################################k
  
lightgbm.hyper_params_search_crit.explore.depth = function() {
   hyper_params = list( ntrees = 2000,
                         max_depth = seq(3, 13, 1),
                         min_rows = 1,
                         learn_rate = 0.02,
                         sample_rate = 0.8,
                         col_sample_rate = 0.5,
                         col_sample_rate_per_tree = 0.5,
                         # The additional light GBM options,
                         # The values are just defaults for me to change later

                         # As seen from https://www.kaggle.com/aharless/xgboost-lightgbm-and-ols/code
                         # And https://www.kaggle.com/guolinke/simple-lightgbm-starter-lb-0-06487
                         max_bins = 255, # Max number of bins for featuees
                         max_leaves = 50, # Max leaves in one tree(I think). Again this controls overfitting
                         min_sum_hessian_in_leaf=1, # like min_data in leaf, deals with overfit
                         min_data_in_leaf=500
    )
    search_criteria = list(strategy = "RandomDiscrete",
                       max_runtime_secs = 5000,
                       max_models = 200,
                       stopping_metric = "RMSE",
                       stopping_tolerance = 0.00005,
                       stopping_rounds = 5,
                       seed = 123456)
    return(list(search_criteria, hyper_params))
}

lightgbm.hyper_params_search_crit = function(
            ntrees_opts = 5000,       # early stopping will stop earlier
            max_depth_opts = seq(4, 12, 2),
            min_rows_opts = c(1, 5, 10),
            learn_rate_opts = seq(0.01, 0.03, 0.003),
            sample_rate_opts = seq(0.3, 0.95, 0.1),
            col_sample_rate_opts = seq(0.55, .9, 0.02),
            col_sample_rate_per_tree_opts = seq(0.65, 0.85, 0.02)) {


    hyper_params = list( ntrees = ntrees_opts,
                         max_depth = max_depth_opts,
                         min_rows = min_rows_opts,
                         learn_rate = learn_rate_opts,
                         sample_rate = sample_rate_opts,
                         col_sample_rate = col_sample_rate_opts,
                         col_sample_rate_per_tree = col_sample_rate_per_tree_opts,
                         # The additional light GBM options,
                         # The values are just defaults for me to change later
                         max_bins = 256,
                         max_leaves = 0,
                         min_sum_hessian_in_leaf = 100,
                         min_data_in_leaf=0
                         #,nbins_cats = nbins_cats_opts
    )
    search_criteria = list(strategy = "RandomDiscrete",
                       max_runtime_secs = 3600,
                       max_models = 200,
                       stopping_metric = "RMSE",
                       stopping_tolerance = 0.00005,
                       stopping_rounds = 5,
                       seed = 123456)
    return(list(search_criteria, hyper_params))
}
