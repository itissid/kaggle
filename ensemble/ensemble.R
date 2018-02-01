# After training the indivisual models we fit a stacked ensemble here
learner <- c("h2o.glm.wrapper",  "h2o.gbm.wrapper")
metalearner <- "h2o.glm.wrapper"

h2o.fit.grid.ensemble = function(
                XY, 
                model_ids, # model ids 
                YName="logerror", 
                independentCols = setdiff(colnames(XY), YName), 
                model_id="stkd.grid.ensmble") {
    set.seed(123456)
    list[XTrain, YTrain, XHoldout, YHoldout] = splitTrainingWrapper(
                        XY, split_percent=0.90, YName="logerror") 
    XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o.stacked")
    XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o.stacked")
    ensemble = h2o.stackedEnsemble(
                x = independentCols,
                y = YName,
                training_frame = XYTrain.h2o,
                model_id=model_id,
                base_model=model_ids)
    # Eval ensemble performance on a test set
    perf <- h2o.performance(ensemble, newdata = XYTest.h2o)

    # Compare to base learner performance on the test set
    baselearner_rmses <- sapply(model_ids, function(mm) h2o.rmse(h2o.performance(h2o.getModel(mm), newdata = XYTest.h2o)))
    baselearner_best_rmse_test <- min(baselearner_rmses)
    ensemble_rmse_test <- h2o.rmse(perf)
    print(sprintf("Best Base-learner Test RMSE:  %s", baselearner_best_rmse_test))
    print(sprintf("Ensemble Test RMSE:  %s", ensemble_rmse_test))
    return(ensemble)
}

h2o.fit.ensemble = function(
                XY, models, YName="logerror", 
                independentCols = setdiff(colnames(XY), YName), 
                model_id="stkd.ensmbl") {
    set.seed(123456)
    list[XTrain, YTrain, XHoldout, YHoldout] = splitTrainingWrapper(
                        XY, split_percent=0.90, YName="logerror") 
    XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o.stacked")
    XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o.stacked")
    print(paste(length(models), " models passed into ensemble"))
    e = h2o.stackedEnsemble(
                x = independentCols,
                y = YName,
                training_frame = XYTrain.h2o,
                model_id=model_id,
                base_model=lapply(FUN=function(m) m@model_id, X=models)
    )
    
    perf <- h2o.performance(e, newdata = XYTest.h2o)
    base_perfs = mapply(models, FUN = function(m) { h2o.rmse(h2o.performance(m, newdata = XYTest.h2o)) })
    baselearner_best_rmse_test <- min(as.numeric(base_perfs))
    ensemble_rmse_test <- h2o.rmse(perf)

    print(sprintf("Best Base-learner RMSE AUC:  %s", baselearner_best_rmse_test))
    print(sprintf("Ensemble Test RMSE:  %s", ensemble_rmse_test))
    return(e)
}

# I have two models a GLM and a GBM. The GBM's params can be added once the optimal one has been tuned from the h2o-xgboost and h2o-gbm
glm.model.train.fn = function(
                  XY, YName="logerror", 
                  independentCols = setdiff(colnames(XY), YName), 
                  model_id="glm.h2o", 
                  alpha=0.5,
                  splitFn=splitTrainingWrapper) {
    set.seed(123456)
    list[XTrain, YTrain, XHoldout, YHoldout] = splitFn(
                        XY, split_percent=0.90, YName="logerror") 
    XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o.glm")
    XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o.glm")
    h2o.glm(y = YName,
            x = independentCols,
            model_id=model_id,
            training_frame = XYTrain.h2o,
            validation_frame = XYTest.h2o,
            remove_collinear_columns =T,
            compute_p_values =F,
            standardize=T,
            family = "gaussian", 
            alpha = alpha, 
            fold_assignment = "Modulo",
            keep_cross_validation_fold_assignment=T,
            keep_cross_validation_predictions = T,
            nfolds=10,
            lambda_search = TRUE)
}

gbm.model.train.fn = function(
                  XY, YName="logerror", 
                  independentCols = setdiff(colnames(XY), YName), 
                  max_depth=10, 
                  learn_rate=0.02, 
                  sample_rate=0.8, 
                  col_sample_rate=0.8,
                  col_sample_rate_per_tree=0.8,
                  ntrees=10000,
                  stopping_round=10,
                  stopping_tolerance=1e-5,
                  stopping_metric="RMSE",
                  splitFn=splitTrainingWrapper) {

    set.seed(123456)
    list[XTrain, YTrain, XHoldout, YHoldout] = splitFn(
                        XY, split_percent=0.90, YName="logerror") 
    XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o.glm")
    XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o.glm")
    gbm.model = h2o.gbm(
	x = independentCols,
	y = "logerror",
	training_frame=XYTrain.h2o,
	validation_frame=XYTest.h2o,
	## more trees is better if the learning rate is small enough
	## here, use "more than enough" trees - we have early stopping
	ntrees = ntrees,

	max_depth = max_depth,

	## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
	learn_rate = learn_rate,

        ## learning rate annealing: learning_rate shrinks by 1% after every tree 
        ## (use 1.00 to disable, but then lower the learning_rate)
        learn_rate_annealing = 0.99,   
	## sample 80% of rows per tree
	sample_rate = sample_rate,

	## sample 80% of columns per split
	col_sample_rate = col_sample_rate,
        keep_cross_validation_fold_assignment=T,
        fold_assignment = "Modulo",
        keep_cross_validation_predictions = TRUE,
        nfolds=10,
	## fix a random number generator seed for reproducibility
	seed = 123456,
	## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
	score_tree_interval = 10,

	## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
        stopping_rounds = 10,
        max_runtime_secs = 5000,
        stopping_tolerance = 1e-5,
        stopping_metric = "RMSE",
    )
}
