library(magrittr)
library(xgboost)
library(caret) # For Folds
library(ModelMetrics)
library(ggplot2)
source("utils.R")
library(foreach)
library(itertools)

###################
# 2 function for xgboost
###################

# fair objective 2 for XGBoost

amo.fairobj2 <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  con <- 2
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2

  return(list(grad = grad, hess = hess))

}

# custom MeanAbsoluteError Metric for XGBoost
# I think this would help against outliers.
maeSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
      out <- mae(data$obs, data$pred)
      names(out) <- "MAE"
      out
}
rmseSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
      out <- ModelMetrics::mse(data$obs, data$pred)
      names(out) <- "RMSE"
      out
}

####################
# Cross-validation
####################

pretrainDiagnostics = function(train, test) {
    # Place holder for plotting various diagnostics of the test and train dataset
    plotMissing = function(dataset, type="Data") {
        missing_values <- dataset %>% summarize_each(funs(sum(is.na(.))/n()))

        missing_values <- gather(missing_values, key="feature", value="missing_pct")
        missing_values %>%
          ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
          geom_bar(stat="identity",fill="red")+
          coord_flip()+
          theme(title=type)
    }

    plotMissing(train, "training")
    plotMissing(train, "testing")
}

# Run me to analyze how the tree over or under fits..
xgbGrid.overfittingCheckGrid = expand.grid(
    nrounds=c(500),
    gamma=c(0.01, 0.001), # min loss reduction to split the leaf. Larger is more conservative
    eta=c(0.01, 0.05), # Learning rate. smaller is more conservative
    max_depth=c(3, 6), # depth of the tree. Smaller is more conservative
    min_child_weight=c(1, 3), # Give up splitting leaf nodes when reaching this threshold. Larger is more conservative
    colsample_bytree=c(0.5, 0.9), # Smaller is more conservative
    subsample=c(0.5, 0.9) # of examples taken to train. Smaller prevents overfitting
)

xgbGrid.default <- expand.grid(
    nrounds = c(800), # max # iterations
    max_depth = c(4), # max depth of the tree
    eta = c(.01, 0.03), # Learning rate
    gamma = c(0.001), # 0.1), # the minimum loss reduction to make a partition
    colsample_bytree = c(1), # 0.25), # ratio of columns when constructing the tree
    min_child_weight = c(1, 2),
    subsample= c(0.8)
)

s= "
Workflow:
0. Create training and testing data set,
1. Create a xgbGrid using a custom grid from the getModelInfo('xgb')$parameters or just null.
1. Use bestFitByGridSearch to figure out the best parameters.
"

getDefaultTrControl = function(allowParallel=T, summaryFunction=rmseSummary) {
    trainControl(
          method = "repeatedcv",
          number = 5,
          repeats = 2,
          verboseIter = TRUE,
          returnData = TRUE,
          allowParallel = allowParallel,
          savePredictions="final",
          summaryFunction=summaryFunction
        )
}

getIndexTrControl = function(
               train.idxs.list, test.idxs.list, allowParallel=T, summaryFunction=rmseSummary) {
    trainControl(
        index=train.idxs.list,
        indexOut=test.idxs.list,
        verboseIter=T,
        returnData=T,
        allowParallel=allowParallel,
        summaryFunction=summaryFunction)
}

xgBoostGridSearch = function(
           train, target, ncores=6,
           tuneGrid=xgbGrid.default,
           metric='RMSE', summaryFunction=rmseSummary, allowParallel=T,
           xgbTrControl=getDefaultGrid(allowParallel, summaryFunction),
           cluster.spec=list(host="localhost"), use.snow=F) {
    require(caret)
    cl = NULL
    if(allowParallel== T) {
        library(doParallel);
        print("Starting parallel cluster for training")
        if(use.snow == T ) {
            cl <- snow::makeCluster(rep(cluster.spec, parallel::detectCores()), outfile="cluster.log");
            doSNOW::registerDoSNOW(cl)
        } else {
            cl <- parallel::makeCluster(rep(cluster.spec, parallel::detectCores()), outfile="cluster.log");
            doParallel::registerDoParallel(cl)
        }
    }

    xgbTrain <- train(
      x = as.matrix(train),
      y = target,
      metric = metric,
      objective = "reg:linear",
      trControl = xgbTrControl,
      tuneGrid = tuneGrid,
      method = "xgbTree"
    )

    if(allowParallel== T) {
        if(use.snow == T ) {
            print("Stopping parallel cluster for training")
            snow::stopCluster(cl)
            gc()
        } else {
            print("Stopping parallel cluster for training")
            parallel::stopCluster(cl)
            doParallel::stopImplicitCluster()
            gc()
        }
    }

    return(xgbTrain)
}

xgPredict = function(xgbModel, dtest) {
    preds = predict(xgbModel, dtest)
}

vtreat.default.grid = expand.grid(
    list(smFactor=c(0.1, 0.01, 0.001),
         rareCount=c(10, 50),
         rareSig=c(0.01, 0.05),
         pruneSig=c(0.01, 0.05)))

mapping_dates.default = list("2016-10-01"= "201610", "2016-11-01" ="201611", "2016-12-01"="201612")
second_round_mapping_dates.default = list("2016-10-01"= "201710", "2016-11-01" ="201711", "2016-12-01"="201712")
second_round_mapping_dates.default = list("2017-10-01"= "201710", "2017-11-01" ="201711", "2017-12-01"="201712")

xgTrainingWrapper = function(XY,
                             features.restricted,
                             features.scaled,
                             remove_outliers=F,
                             YName="logerror",
                             xgBoostTrainingGrid=xgbGrid.default,
                             splitFn=splitKWayStratifiedCrossFold,
                             holdout.metric='RMSE',
                             holdout.metric.fn=rmseSummary,
                             parallelTraining=T,
                             use.snow=T,
                             cluster.spec=list(host="localhost"),
                             holdoutdata.splitpercent=0.95,
                             convertFactorsToNumerics=F
                             ) {
    # Pre process options:
        # converts factors to numerics if requested
        # remove outliers if requested
        # limit features to features.restricted
        # scale features listed in features.scaled
    # Training options:
       # splitFn for making a hold out test set that is not used for training, default is KWay stratified Cross fold
       # holdout metric and its function, default is RMSE
       # holdoutdata.splitpercent for % split between training and testing
    # Parallelization Options:
       # parallel training if requested.
        print(".x")
        XY %<>% select_at(dplyr::vars(c(features.restricted, YName)))  %>%
            transformFeaturesForLinearRegression(txn.feature = features.scaled)
        if(convertFactorsToNumerics == T)
            XY %<>%
                mutate_if(is.factor, funs(as.numeric(as.character(.))))
        if(remove_outliers==T) {
            XY %<>%
                dplyr::filter(logerror <=0.4 & logerror >=-0.4)
        }
        print("..x")
        list[XTrain, YTrain, XTest, YTest] = splitTrainingWrapper(
                                                  XY, splitFn=splitFn, YName=YName,
                                                  split_percent=holdoutdata.splitpercent)
        print("...x")
        bestFit = xgBoostGridSearch(
            train = XTrain, target = YTrain, ncores = 6, tuneGrid = xgBoostTrainingGrid,
            metric = holdout.metric , summaryFunction = holdout.metric.fn, allowParallel=parallelTraining,
            use.snow=use.snow,
            cluster.spec=cluster.spec)
        pred = predict(bestFit, XTest)
        print("...x")
        bestFit$holdoutPred = pred
        bestFit$holdout.rmse = sqrt(mean((YTest - pred)^2))
        return(bestFit)

}

xgPredictWrapper = function(X, fitObj,
                            features.restricted,
                            features.scaled,
                            mapping=mapping_dates.default,
                            second_round_mapping = second_round_mapping_dates.default,
                            dates.are.numeric =T, # In case dates are converted to numeric values for the learning algo
                            recode_list.for.date) {

    X %<>%
        dplyr::select_at(dplyr::vars(setdiff(features.restricted, "date")))  %>%
        transformFeaturesForLinearRegression(txn.feature = features.scaled) %>%
        dplyr::mutate_if(is.factor, dplyr::funs(as.numeric(as.character(.))))
    print(".P")

    cl <- parallel::makeCluster(as.integer(parallel::detectCores()*3/4))
    doParallel::registerDoParallel(cl)
    predictionsFn = propertiesDataSetPredictorsWithDateEncoding(recode_list.for.date,
                                                              mapping, second_round_mapping)
    predictions = predictionsFn(X, fitObj, dates.are.numeric)
    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)

    print("..P")
    predictions %<>% dplyr::mutate(parcelid=X$id_parcel)
    return(predictions)
}

# Call with the cross frame the properties.
# Returns a function which given a fit object yeilds prediction that can be written to a file
xgPredictionOnPropertiesWithVtreat = function(crossFrame,
                                           properties,
                                           features.restricted,
                                           features.scaled,
                                           #features.treated,
                                           addDateMonth=T) {
    X = properties %>%
        select_at(dplyr::vars(setdiff(features.restricted, "date")))  %>%
        transformFeaturesForLinearRegression(txn.feature = features.scaled) %>%
        mutate_if(is.factor, funs(as.numeric(as.character(.))))

    prediction = function(bestFit) {
        pred.df = data.frame(row.names=1:nrow(X))
        cl <- parallel::makeCluster(as.integer(parallel::detectCores()*3/4))
        doParallel::registerDoParallel(cl)
        for(date in names(mapping)) {
           m = mapping[[date]] # The prediction column name
           Xtemp = X %>%
                dplyr::mutate(date = as.Date(date)) %>%
                applyCrossFrameToX(crossFrame, isTrain = F, keepDateTimeFeature=T)
           predictions <- foreach(d=isplitRows(Xtemp, chunks=6),
                                .combine=c, .packages=c("stats", "caret")) %dopar% {
               stats::predict(bestFit, newdata=d)
           }
           pred.df %<>% dplyr::mutate(!!m := predictions)
           x_bar = dplyr::coalesce(pred.df %>% dplyr::pull(m), mean(pred.df %>% dplyr::pull(m), na.rm=T))
           pred.df %<>% dplyr::mutate(!!m := x_bar)
        }
        return(pred.df)
    }
    return(prediction)
}

xgTrainingWrapperWithVTreat = function(transactions,
                                       features.restricted,
                                       features.treated,
                                       features.scaled,
                                       remove_outliers=T,
                                       discretizeDateMonth=T,
                                       vtreat.grid=vtreat.default.grid,
                                       YName="logerror",
                                       xgBoostTrainingGrid=xgbGrid.default,
                                       splitFn=splitKWayCrossFold,
                                       holdout.metric='RMSE',
                                       holdout.metric.fn=rmseSummary,
                                       makeLocalCluster=F,
                                       parallelTraining=F,
                                       snowCluster=NULL) {
    # xgboost with custom defaults. First it creates the cross frame using vtreat. This splits the training data
    # into 2. The treatments are apploed to both. The boosting models are trained using different vtreat options.
    # the boosting model itself can train over a grid if one is provided.
    # 1. the localhost cluster parallelizes the cross frame creation based on vtreat.grid.
    # 2. the remoteCluster is used to parallelize the computation of the boosting models themselves.
    XY = transactions %>% select_at(dplyr::vars(c(features.restricted, YName)))  %>%
        transformFeaturesForLinearRegression(txn.feature = features.scaled)
    if(remove_outliers==T) {
        XY %<>%
            dplyr::filter(logerror <=0.4 & logerror >=-0.4)
    }
    if(discretizeDateMonth == T) {
        XY %<>%
            discretizetime.month(time.feature.name.new=date)
    }
    trainingVtreatWrapper(XY,
                          features.restricted=features.restricted,
                          features.treated=features.treated,
                          YName=YName,
                          keepDateCol=discretizeDateMonth,
                          splitFn=splitFn,
                          tuneGrid = xgBoostTrainingGrid,
                          holdout.metric =holdout.metric,
                          summaryFn = holdout.metric.fn,
                          gridSearchFn = xgBoostGridSearch,
                          vtreat.grid,
                          makeLocalCluster=makeLocalCluster,
                          crossFrameCluster=snowCluster,
                          parallelTraining=parallelTraining)
    #results = snow::parLapply(cl=snowCluster,
    #                          apply(vtreat.grid, 1, as.list), parallelTraining,
    #                          XY,
    #                          features.restricted=features.restricted,
    #                          splitFn=splitFn,
    #                          features.treated=features.treated,
    #                          YName=YName,
    #                          tuneGrid = xgBoostTrainingGrid,
    #                          holdout.metric =holdout.metric,
    #                          summaryFn = holdout.metric.fn,
    #                          gridSearchFn = xgBoostGridSearch,
    #                          makeLocalCluster=F)
    #datetime  = format(Sys.time(), "%Y%m%d_%H_%M_%S")
    #fn = paste("results/xgboost_with_vtreat_", datetime, sep="")
    #print(paste("saving xgboost model to ", fn), sep="")
    #saveRDS(results, fn)
    #return(results)
}
