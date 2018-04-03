library(h2o)
library(futile.logger)
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
