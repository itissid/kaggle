library(h2o)
# Make a grid based on type of algo
h2o.grid.helper = function(
           XY, X, independentCols, YName, 
           hyper_params, 
           search_criteria, 
           algorithm,
           grid_id=NULL,
           keep_cvpreds=T,
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
      training_frame = XY,
      validation_frame = X,
      keep_cross_validation_predictions = keep_cvpreds,
      ## fix a random number generator seed for reproducibility
      seed = 123456,

      ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
      score_tree_interval = 10,
      ...
    )
}
