library(h2o)
# Make a grid based on type of algo
h2o.grid.helper = function(
           XYTrain, XYTest, independentCols,
           YName="logerror",
           hyper_params,
           search_criteria,
           algorithm,
           grid_id=NULL,
           keep_cvpreds=T,
           seed = 123456,
           ...) { # Primarily used for specific algorithm and not for the grid itself
    h2o.grid (
        ## hyper parameters
        hyper_params = hyper_params,

        ## full Cartesian hyper-parameter search
        search_criteria = search_criteria,

        ## which algorithm to run
        algorithm=algorithm,

        ## identifier for the grid, to later retrieve it
        grid_id=grid_id,

        ## standard model parameters
        x = independentCols,
        y = YName,
        training_frame = XYTrain,
        validation_frame = XYTest,
        keep_cross_validation_predictions = keep_cvpreds,
        ## fix a random number generator seed for reproducibility
        seed = seed,

        score_tree_interval = 1, # Not applicable for GBM's
        score_each_iteration=TRUE,
        # NOTE: The stopping criteria for the individual trees are different from the stopping criteria for grid search.: https://groups.google.com/forum/#!topic/h2ostream/xgVU3scBb7w. These correspond to criteria for the individual trees to be trained.
        stopping_metric = "RMSE",
        stopping_tolerance = 1e-5, # as the learning curve flattens the error oscillates around a fixed value with a value > stopping tolerance. So make sure this isn't too small. Look at teh h2o learning graph to figure this out
        stopping_rounds = 5,
        max_runtime_secs = 5000,
        ...
    )
}

modelListFromGrids = function(grid_id_prefix, N) {
    doc = "
    This utility was useful for extracting the models from multiple grids
    with grid id as grid_id_prefix and 1:N as suffix
    "
    sapply(X=1:N,
           FUN=function(x) {
               grid.ids = h2o.getGrid(paste(grid_id_prefix, x, sep=''))
               sapply(X=grid.ids@model_ids, FUN=h2o.getModel)
           })
}

# This is a routine you can call that parallelizes the three predictions on dates 2016/10-2016/12 for zillow data set using h2o
h2o.createPredictionsFromModel = function(
	    gbm.model_id, X, host=H2O.HOST, port=H2O.PORT) {
        # It assumes that the h2o cluster is available on the remote machine
        # Do prediction for three dates
        # TODO: Consider parallel prediction for each of the 3 columns
        predictions = foreach(
                          i = 1:length(predict.dates),
                          .combine=cbind,
                          .packages=c("h2o", "magrittr")) %do% {
            # Create a small h2o instance to do predictions on each data set
            return(tryCatch({
                h2o.connect(ip=host, port=port)
                gbm.model = h2o.getModel(gbm.model_id)
                print("..")
                prediction = as.data.frame(
                               h2o.predict(gbm.model,
                                           X %>% 
                                               dplyr::mutate(date=predict.dates.numeric.codes[i]) %>%
                                               as.h2o
                                           )
                               )
                print("....")
                return(prediction)
            }, error= function(e) {
                print('Error!')
                print(e)
            }))
        }

	predictions = cbind(predictions, predictions)
	colnames(predictions) = c("201610", "201611", "201612", "201710", "201711", "201712")
	predictions$parcelid = X$id_parcel
	print("....P")
        # Cleanup
        #h2o.rm(c("XPredict.1", "XPredict.2", "XPredict.3"))
	return(predictions)
}
