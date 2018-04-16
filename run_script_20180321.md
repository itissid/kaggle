---
title: Tuning the GBM learning rate
author: "Sidharth Gupta"
date: \today
output:
   html_document:
    css: css/code.css
    toc: true
    toc_depth: 6
    highlight: default
    citation_package:
    keep_tex: false
    keep_md: true
    fig_caption: true
    latex_engine: pdflatex
runtime: shiny
fontsize: 11pt
geometry: margin=1ine
header-includes:
- \usepackage{indentfirst}
- \usepackage{graphicx}
- \usepackage{geometry}
- \usepackage{subfigure}
- \usepackage{amsmath}
- \usepackage{listings}
- \usepackage{tikz}
- \usetikzlibrary{matrix}
---


```r
library(dplyr)
library(magrittr)
library(gridExtra)
library(ggplot2)
library(h2o)
library(gridExtra)
library(futile.logger)
library(kableExtra)
source("h2o/source-for-rmd.R")
SEED = 123456
h2o.init()
```



## Tuning the learning rate
In this vignette I will try and get a feel for the learning rate by training many GBM models on just the learning rate. Once these models are trained I will look at the scoring history and what learning rate does well.

### Load the data
First lets use the wrapper routines and prepare some data.


```r
# The function for reading the data does a few things by default. It makes categorical  variables where ever necessar, and log transforms the large variables like for taxes etc. See documentation of that function.
list[tr.baseline, pr.baseline, vtreatIdentityFn, tplan.NULL] =  prepareDataWrapper.h2o.gbm.baseline()

independent.vars = colnames(tr.baseline %>% select(-logerror))

list[XYTrain.h2o, XYTest.h2o] = h2o.splitFrame(as.h2o(tr.baseline), ratios = 0.85, destination_frames=c("XYTrain.h2o", "XYTest.h2o"), seed=SEED)
```

## Defining the tree parameters
There are two data sets one used in the training and the other held out as a validation set. I created a grid with defaults, especially a large number of trees. But becasue we have early stopping we should be ok.


```r
hyper_params_search_crit.learnrate.tune = function(
            ntrees_opts = 10000, # early stopping will stop the earlier.
            max_depth_opts = 10,  # Start with 6 for now
            min_rows_opts = 5,   # How many leaf nodes to average over, doing the log of # rows would be ok
            learn_rate_opts = 0.01,
            learn_rate_annealing = 1,
            sample_rate_opts = 0.9,
            col_sample_rate_opts = 0.9,
            col_sample_rate_per_tree_opts = 0.9,
            strategy="RandomDiscrete") {

    list[search_criteria, hyper_params] =  hyper_params_search_crit(
            ntrees_opts = ntrees_opts,
            max_depth_opts = max_depth_opts,
            min_rows_opts = min_rows_opts,
            learn_rate_opts = learn_rate_opts, # from the previous
            learn_rate_annealing = learn_rate_annealing,
            sample_rate_opts = sample_rate_opts,
            col_sample_rate_opts = col_sample_rate_opts,
            col_sample_rate_per_tree_opts = col_sample_rate_per_tree_opts,
            strategy=strategy)
    return(list(search_criteria, hyper_params))
}
```


## Fitting the model
Here is the wrapper written to do the model fit.

```r
baseline.fit = function(hp, sc, grid_id = "learning_rate_search", seed=123456, ...) {
        h2o.grid.helper(
                        h2o.getFrame("XYTrain.h2o"),
                        h2o.getFrame("XYTest.h2o"),
                        independentCols=independent.vars,
                        hyper_params = hp,
                        search_criteria = sc,
                        algorithm="gbm",
                        grid_id=grid_id,
                        seed = seed,
                        ...
                        )
}
target_learning_rates = seq(0.01, 0.05, 0.01)
list[search_criteria.gbm.learnrate, hyper_params.gbm.learnrate] =
    hyper_params_search_crit.learnrate.tune(learn_rate_opts=target_learning_rates)
```

Now lets train a grid with different learning rates using crossvalidation and using the holdout data set as the guiding star to evaluate the best learning rate.


```r
# Randomize and search for the best learning rate.
fit = baseline.fit(
       hp=hyper_params.gbm.learnrate,
       sc=search_criteria.gbm.learnrate,
       grid_id = paste("learning_rate_search"),
       seed = SEED, 
       nfolds=5)
```
## Analysis
Lets see what are the top learning rates:

```r
kable(fit@summary_table %>% data.frame %>% select(learn_rate, residual_deviance) %>% arrange(residual_deviance))
```



learn_rate   residual_deviance    
-----------  ---------------------
0.01         0.025960275431052615 
0.02         0.02600069940495735  
0.03         0.026019651565490732 
0.04         0.02605709890513915  
0.05         0.02610080794337662  

### Plotting the individual learning rate.
Save the models, optionally for later use

```r
LOCAL.RESULTS.DIR=paste0(getwd(), "/results/learn_rate_2018_15_04_gbm")
if(!dir.exists(LOCAL.RESULTS.DIR)) 
    dir.create(LOCAL.RESULTS.DIR)
h2o.gridSaver(c("learning_rate_search"), results.dir=LOCAL.RESULTS.DIR)
```

Optionally load the grid models for evaluation

```r
LOCAL.RESULTS.DIR=paste0(getwd(), "/results/learn_rate_2018_15_04_gbm")
#models = h2o.gridLoader(Map(function(x) paste0("learning_rate_search_",x) , 1:NGrids), results.dir=LOCAL.RESULTS.DIR)
models = h2o.gridLoader(c("learning_rate_search"), results.dir=LOCAL.RESULTS.DIR)[, 1]
```

```
## [1] "Loading /Users/sidharth/Dropbox/workspace/stats-4065/kaggle/zillow/results/learn_rate_2018_15_04_gbm/learning_rate_search/learning_rate_search_model_0"
## [1] "Loading /Users/sidharth/Dropbox/workspace/stats-4065/kaggle/zillow/results/learn_rate_2018_15_04_gbm/learning_rate_search/learning_rate_search_model_1"
## [1] "Loading /Users/sidharth/Dropbox/workspace/stats-4065/kaggle/zillow/results/learn_rate_2018_15_04_gbm/learning_rate_search/learning_rate_search_model_2"
## [1] "Loading /Users/sidharth/Dropbox/workspace/stats-4065/kaggle/zillow/results/learn_rate_2018_15_04_gbm/learning_rate_search/learning_rate_search_model_3"
## [1] "Loading /Users/sidharth/Dropbox/workspace/stats-4065/kaggle/zillow/results/learn_rate_2018_15_04_gbm/learning_rate_search/learning_rate_search_model_4"
```

Or extract the individual models of each of the cross validated folds.

```r
models = Map(h2o.getModel, fit@model_ids) 
```


```r
length(models)
```

```
## [1] 5
```

Extract the score histories of each of the models into a data frame.

```r
getRMSEScoreHistory = function (models) {
    # Given a list of model ids get a data frame with its RMSE score history
    score.history = data.frame()
    # Learning rate of 0.03 seems to be ok.
    for (i in 1:length(models)) {
        model.rmse = h2o.rmse(h2o.performance(models[[i]]))
        lr = models[[i]]@parameters$learn_rate
        model.rmse.rate = h2o.scoreHistory(models[[i]]) %>%
            data.frame %>%
            pull(validation_rmse)
        score.history = rbind(score.history,
                              data.frame(
                                         ntrees = 1:length(model.rmse.rate),
                                         rmse = model.rmse.rate,
                                         id = i,
                                         lr = lr
                                         )
                              )
    }
    score.history
}

# For each model list in models extract the histories
score.history = getRMSEScoreHistory(models)
```

Plot the individual learning rate curves


```r
ggplot(score.history) +
        geom_line(aes(y=rmse, x=ntrees, color=as.factor(lr)),
                  position=position_dodge(1)) +
        theme(
          legend.position="bottom"
        )
```

![](run_script_20180321_files/figure-html/plots-1.png)<!-- -->

## Conclusion and a note on learning rate variances

The learning rates begin to increase a bit at the end i.e. overshoot the optimum. The learning rates of 0.01-0.02 tended to perform better. I also repeated this experiment many times and evaluate the variance of the learning rate. 


```r
NGrids = 10
cls = registerCluster(max.cores.per.machine = 6)

fit.bootstrapped = foreach(x = 1:NGrids, .packages= c("h2o")) %dopar% {
              h2o.connect()
              baseline.fit(
                       hp=hyper_params.gbm.learnrate,
                       sc=search_criteria.gbm.learnrate,
                       grid_id = paste("learning_rate_search_", x, sep=''),
                       seed = x)
}
```





 
Extract the combined score history and plot the grids

 ntrees        rmse   id     lr   grid_id
-------  ----------  ---  -----  --------
     40   0.1560287    1   0.02         7
     38   0.1560368    1   0.04         2
     35   0.1561204    1   0.02         9
     44   0.1561569    1   0.02         4
     18   0.1561896    1   0.05         1
     56   0.1562313    1   0.01         8
     18   0.1562642    1   0.04        10
     27   0.1562698    1   0.02         3
     55   0.1563246    1   0.01         6
     27   0.1563651    1   0.03         5

![](run_script_20180321_files/figure-html/plots.grid-1.png)<!-- -->

These experiments show that in 6/10 cases rates of 0.1-0.3 do better than others. But there is still significant variance. Next lets look at annealing to improve on this.
