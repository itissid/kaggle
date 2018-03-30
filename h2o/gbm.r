library(h2o)
source("utils.R")
source("datadefs.R")
# Strategy 1 to tune the boosting models
# 1. Start with a short number of trees like 100  and tune the learning rate: [2-10]/trees.
#     Observe the curve of the training metric to figure out the learning rate
# 2. Once you have a good hold of learning rate we can try tuning the tree depth.
# 3. And then the rest of the parameters

# March 23rd 2018: Another strategy to tune these trees is as follows:
# https://github.com/h2oai/h2o-3/blob/master/h2o-docs/src/product/tutorials/gbm/gbmTuning.Rmd
# First decide on the number of trees by selecting a low learning rate(0.02). Use a stopping
# Once the number of trees are determined(depending on what the algorithm selected) one can tune the other params
#####################################
##### NOTE: Early stopping for individual trees and the total algorithm are different: https://groups.google.com/forum/#!topic/h2ostream/xgVU3scBb7w
####################################
hyper_params_search_crit = function(
            # Defaults are meant for tuning the learning rate
            ntrees_opts = 1000,
            max_depth_opts = 6,
            min_rows_opts = 5,
            learn_rate_opts = 0.01,
            learn_rate_annealing = 0.995,
            sample_rate_opts = 0.8,
            col_sample_rate_opts = 0.8,
            col_sample_rate_per_tree_opts = 0.8,
	    col_sample_rate_change_per_level_opts = 0.9,
	    min_split_improvement_opts=1e-05,
	    nbins_opts = 20, 
	    nbins_cats_opts = 1024,
	    histogram_type_opts = c("AUTO", "UniformAdaptive", "Random", "QuantilesGlobal", "RoundRobin"),
            strategy="Cartesian") {
            # Creates Hyper parameters for GBM
            # no categorical features in this dataset
            #nbins_cats_opts = seq(100,10000,100)


    hyper_params = list( ntrees = ntrees_opts,
                         max_depth = max_depth_opts,
                         min_rows = min_rows_opts,
                         learn_rate = learn_rate_opts,
                         learn_rate_annealing=learn_rate_annealing,
                         sample_rate = sample_rate_opts,
                         col_sample_rate = col_sample_rate_opts,
                         col_sample_rate_per_tree = col_sample_rate_per_tree_opts,
			 col_sample_rate_change_per_level = col_sample_rate_change_per_level_opts,
			nbins = nbins_opts,			
			nbins_cats = nbins_cats_opts,
			min_split_improvement = min_split_improvement_opts,
			histogram_type = histogram_type_opts
			 
    )

    search_criteria = list(
                       strategy = strategy
                      )
    return(list(search_criteria, hyper_params))
}


# Once we have a handle on the learning rage lets figure out the depth
hyper_params_search_crit.depth = function(
            # Creates Hyper parameters for Just varying the depth, because that seemed to be the only important one after you adjust for the
            ntrees_opts = 10000,
            max_depth_opts = c(2, 12, 2),
            min_rows_opts = 5,
            learn_rate_opts = 0.02,
            learn_rate_annealing = 0.995,
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

prepareDataWrapper.h2o.gbm.baseline = function(
                       tr="inputs/train_2016_v2.csv",
                       pr="inputs/properties_2016.csv",
                       log.transform=T, # Log transformation of
                       remove.outliers=F,
                       outlier.range=c(-0.4, 0.4),
                       omit.nas=F,
                       do.vtreat=F,
                       keep.dates=F, # should we keep dates in the training data set
                       dates.to.numeric=F, # Convert the transaction dates to numeric
                       categorical.to.numeric=F, # h2o does this by default: https://github.com/h2oai/h2o-3/blob/master/h2o-docs/src/product/data-science/xgboost.rst
                       convert.to.categorical=T, # but convert some of these features to categorical so that h2o handles it properly
                       taxbyarea.normalize=F,
                       large.missing.features.prune=F, # h2o's docs says it treats missing data as information. So we won't delete rows
                       missing.feature.cutoff.frac = 1,
                       features.excluded=features.excluded.xg.default,
                       # Convert to factors carefully
                       features.categorical=features.categorical.xg.default,
                       features.logtransformed=features.logtransformed.xg.default,
                       features.vtreat.treated=features.treated.vtreat.xg.default,
                       vtreat.opts=list()) {
    # From utils
    list[transactions, properties, testVtreatFn, tplan] = prepareData(
         tr=tr,
         pr=pr,
         log.transform=log.transform,
         large.missing.features.prune=large.missing.features.prune,
         missing.feature.cutoff.frac = missing.feature.cutoff.frac,
         remove.outliers=remove.outliers,
         outlier.range=outlier.range,
         omit.nas=omit.nas,
         do.vtreat=do.vtreat,
         categorical.to.numeric=categorical.to.numeric,
         convert.to.categorical=convert.to.categorical,
         taxbyarea.normalize=taxbyarea.normalize,
         vtreat.opts=vtreat.opts,
         features.excluded=features.excluded,
         features.logtransformed=features.logtransformed,
         features.vtreat.treated=features.vtreat.treated,
         features.categorical=features.categorical
         )

    # The dates are to be treated as numeric features for
    t.dates = as.factor(transactions$date)
    if(dates.to.numeric)
        transactions %<>% mutate(date = as.numeric(as.factor(date)))
    if(keep.dates == F)
        transactions %<>% select(-date)
    return(list(transactions, properties, testVtreatFn, tplan, t.dates))
}


# Use the gbm model to do some predictions from the grid like so:
# grid =  gbm.grid(..., grid_id="test_grid", ...)
# sortedGrid = h2o.getGrid("test_grid", sort_by="RMSE", decreasing = TRUE)
# gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
#

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

