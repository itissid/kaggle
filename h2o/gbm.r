library(h2o)
# Strategy to tune the boosting models
# 1. Start with a short number of trees like 100  and tune the learning rate: [2-10]/trees.
#     Observe the curve of the training metric to figure out the learning rate 
# 2. Once you have a good hold of learning rate we can try tuning the tree depth. 
# 3. And then the rest of the parameters
hyper_params_search_crit = function(
            ntrees_opts = 1000,       # early stopping will stop the earlier.
            max_depth_opts = 6, # Start with 6 for now
            min_rows_opts = 5, # How many leaf nodes to average over, doing the log of # rows would be ok
            learn_rate_opts = seq(0.01, 0.1, 0.02),
            sample_rate_opts = 0.8,
            col_sample_rate_opts = 0.8,
            col_sample_rate_per_tree_opts = 0.8) {
            # Creates Hyper parameters for GBM
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
                       max_runtime_secs = 5000,
                       max_models = 200,
                       stopping_metric = "RMSE",
                       stopping_tolerance = 1e-4, 
                       stopping_rounds = 5,
                       seed = 123456)
    return(list(search_criteria, hyper_params))
}

# Once we have a handle on the learning rage lets
hyper_params_search_crit.depth = function(
            # Creates Hyper parameters for Just varying the depth, because that seemed to be the only important one after you adjust for the
            ntrees_opts = 1000,
            max_depth_opts = c(2, 12, 2),
            min_rows_opts = 5,
            learn_rate_opts = 0.01, # from the previous
            sample_rate_opts = 0.8,
            col_sample_rate_opts = 0.8,
            col_sample_rate_per_tree_opts = 0.8) {
            # no categorical features in this dataset


    list[search_criteria, hyper_params] =  hyper_params_search_crit(
            ntrees_opts = ntrees_opts,
            max_depth_opts = max_depth_opts,
            min_rows_opts = min_rows_opts,
            learn_rate_opts = learn_rate_opts, # from the previous
            sample_rate_opts = sample_rate_opts,
            col_sample_rate_opts = col_sample_rate_opts,
            col_sample_rate_per_tree_opts = col_sample_rate_per_tree_opts) 
    return(list(search_criteria, hyper_params))
}


hyper_params_search_crit.final = function(
            # Creates Hyper parameters for Just varying the depth, because that seemed to be the only important one after you adjust for the
            ntrees_opts = 3000,      
            max_depth_opts = 12,
            min_rows_opts = 1,
            learn_rate_opts = 0.01,
            sample_rate_opts = seq(0.3, 0.95, 0.1),
            col_sample_rate_opts = seq(0.55, .9, 0.02),
            col_sample_rate_per_tree_opts = seq(0.65, 0.85, 0.02)) {
            # no categorical features in this dataset
            #nbins_cats_opts = seq(100,10000,100)


    list[search_criteria, hyper_params] =  hyper_params_search_crit(
            ntrees_opts = ntrees_opts,
            max_depth_opts = max_depth_opts,
            min_rows_opts = min_rows_opts,
            learn_rate_opts = learn_rate_opts, # from the previous
            sample_rate_opts = sample_rate_opts,
            col_sample_rate_opts = col_sample_rate_opts,
            col_sample_rate_per_tree_opts = col_sample_rate_per_tree_opts) 
    return(list(search_criteria, hyper_params))
}


list[search_criteria.gbm.default, hyper_params.gbm.default] =  hyper_params_search_crit()
list[search_criteria.gbm.depth, hyper_params.gbm.depth] =  hyper_params_search_crit.depth()
list[search_criteria.gbm.final, hyper_params.gbm.final] =  hyper_params_search_crit.final()

# TODO: Use h2o-utils to dedup this code
gbm.grid = function(
                    XY, YName="logerror", 
                    independentCols = setdiff(colnames(XY), YName), 
                    hyper_params=hyper_params.default, 
                    search_criteria=search_criteria.default, 
                    grid_id="depth_grid",
                    keep_cvpreds=T) {

    print(paste(hyper_params, sep=":", collapse=", "))

    list[XTrain, YTrain, XHoldout, YHoldout] = splitTrainingWrapper(
                        XY, split_percent=0.90, YName="logerror") 
    XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o")
    XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o")
    h2o.grid.helper(
        XYTrain.h2o, 
        XYTest.h2o, 
        independentCols, 
        YName=YName, 
        hyper_params=hyper_params.default, 
        search_criteria=search_criteria.default,
        grid_id=grid_id , 
        algorithm="gbm",
        keep_cvpreds=keep_cvpreds)
    #h2o.grid (
    #  ## hyper parameters
    #  hyper_params = hyper_params,

    #  ## full Cartesian hyper-parameter search
    #  search_criteria = search_criteria,

    #  ## which algorithm to run
    #  algorithm="gbm",

    #  ## identifier for the grid, to later retrieve it
    #  grid_id=grid_id,

    #  ## standard model parameters
    #  x = independentCols,
    #  y = YName,
    #  training_frame = XYTrain.h2o,
    #  validation_frame = XYTest.h2o,
    #  keep_cross_validation_predictions = keep_cvpreds,
    #  ## fix a random number generator seed for reproducibility
    #  seed = 123456,

    #  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
    #  score_tree_interval = 10
    #)
}

# Use the gbm model to do some predictions from the grid like so:
# grid =  gbm.grid(..., grid_id="test_grid", ...)
# sortedGrid = h2o.getGrid("test_grid", sort_by="RMSE", decreasing = TRUE)
# gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
# createPredictions

createPredictionsFromModel = function(gbm.model, X, rlist) {
        getDateCode = function(d, rlist) {rlist$date %>% dplyr::filter(date==as.Date(d)) %>% dplyr::pull(date_coded)}
	dates = vapply(FUN=getDateCode, X=c("2016-10-01", "2016-11-01", "2016-12-01"), rlist, FUN.VALUE=1, USE.NAMES=F)
	XPredict.1 = as.h2o(X  %>% mutate(date=dates[1]), destination_frame="XPredict.1")
	p.1 = as.data.frame(h2o.predict(gbm.model,  XPredict.1))
        h2o.rm("XPredict.1")
	print(".P")

	XPredict.2 = as.h2o(X  %>% mutate(date=dates[2]), destination_frame="XPredict.2")
	p.2 = as.data.frame(h2o.predict(gbm.model,  XPredict.2))
        h2o.rm("XPredict.2")
	print("..P")

	XPredict.3 = as.h2o(X  %>% mutate(date=dates[3]), destination_frame="XPredict.3")
	p.3 = as.data.frame(h2o.predict(gbm.model,  XPredict.3))
        h2o.rm("XPredict.3")
	print("...P")
	predictions = cbind(p.1, p.2, p.3, p.1, p.2, p.3)
	colnames(predictions) = c("201610", "201611", "201612", "201710", "201711", "201712")
	predictions$parcelid = X$id_parcel
	print("....P")
	return(predictions)
}

