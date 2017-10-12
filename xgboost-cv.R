library(magrittr)
library(xgboost)
library(caret) # For Folds
library(ModelMetrics)
library(ggplot2)
library(foreach)
library(itertools)
source("datadefs.R")

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
      out <- ModelMetrics::mae(data$obs, data$pred)
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

xgbTree.grid.default <- expand.grid(
    nrounds = 800, # max # iterations
    max_depth = 4, # max depth of the tree
    eta = .3, # learning rate
    gamma = 0.001, # 0.1), # the minimum loss reduction to make a partition
    colsample_bytree = 0.75, # 0.25), # ratio of columns when constructing the tree
    min_child_weight = 2,
    subsample= 0.75
)
xgbLinear.grid.default <- expand.grid(
    nrounds=c(100, 150, 200), # max # iterations
    #max_depth = c(4,5), # max depth of the tree
    eta = 0.037, # Learning rate
    alpha=0.4,
    lambda=0.8
)


xgbTree.grid.cv <- expand.grid(
    nrounds = 800, # max # iterations
    max_depth = c(4,5), # max depth of the tree
    eta = .3, # Learning rate
    gamma = c(0, 0.01, 0.001), # 0.1), # the minimum loss reduction to make a partition
    colsample_bytree = 0.75, # 0.25), # ratio of columns when constructing the tree
    min_child_weight = c(1, 2),
    subsample= c(0.75,1)
)

xgbLinear.grid.cv <- expand.grid(
    nrounds = c(100, 200, 300), # max # iterations
    eta = c(0.01, 0.05, 0.1, 0.2), # Learning rate
    alpha=c(0.1, 0.4),
    lambda=c(0.1, 0.5, 0.8)
)
s= "
Workflow:
0. Create training and testing data set,
1. Create a xgbGrid using a custom grid from the getModelInfo('xgb')$parameters or just null.
1. Use bestFitByGridSearch to figure out the best parameters.
"
# TODO: try adaptive CV
getDefaultTrControl.xg = function(
              allowParallel=T,
              summaryFunction=rmseSummary,
              method="repeatedcv",
              folds=folds
              ) {
    trainControl(
          method = method,
          number = folds,
          repeats = 2,
          verboseIter = TRUE,
          returnData = TRUE,
          allowParallel = allowParallel,
          savePredictions="final",
          summaryFunction=summaryFunction
        )
}

getIndexTrControl.xg = function(
               train.idxs.list,
               test.idxs.list,
               allowParallel=T,
               method="repeatedcv",
               summaryFunction=rmseSummary) {
    trainControl(
        method = method,
        index=train.idxs.list,
        indexOut=test.idxs.list,
        verboseIter=T,
        returnData=T,
        allowParallel=allowParallel,
        savePredictions="final",
        summaryFunction=summaryFunction)
}
# Function borrowed from regression.R, supports additional parallelization
# using the cluster.spec and use.snow options
bestFit.xg = function(
           train, target, ncores=6,
           tuneGrid=xgbTree.grid.default,
           metric='RMSE',
           allowParallel=T,
           trControl= getDefaultTrControl.xg(allowParallel=allowParallel),
           cluster.spec=list(host="localhost"),
           use.snow=T) {
    require(caret)
    cl = NULL
    ncores_p =  parallel::detectCores()
    ncores_p = ifelse(is.na(ncores_p), 4, ncores_p)
    if(allowParallel== T) {
        print("Starting parallel cluster for training")
        if(use.snow == T ) {
            print("Using snow for parallel cluster")
            cl <- snow::makeCluster(rep(cluster.spec, ncores_p), outfile="cluster.log");
            doSNOW::registerDoSNOW(cl)
        } else {
            print("Using parallel for local cluster")
            cl <- parallel::makeCluster(rep(cluster.spec, ncores_p), outfile="cluster.log");
            doParallel::registerDoParallel(cl)
        }
    }
    print("Using Grid: ")
    print(tuneGrid)
    print(paste("Using metric", metric))
    print(trControl)
    xgbTrain <- caret::train(
      x = as.matrix(train),
      y = target,
      metric = metric,
      #objective = "reg:linear",
      trControl = trControl,
      tuneGrid = tuneGrid,
      method = "xgbLinear"
    )

    if(allowParallel== T) {
        print(trControl)
        if(use.snow == T ) {
            print("Stopping snow cluster for training")
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

vtreat.default.grid = expand.grid(
    list(smFactor=c(0.1, 0.01, 0.001),
         rareCount=c(10, 50),
         rareSig=c(0.01, 0.05),
         pruneSig=c(0.01, 0.05)))
# TODO: Maybe we can use certain permutations of this to see what improves prediction
# Data preparation utility copied from regression.R.
prepareDataWrapper.xg = function(
                       recode_chars=T, # Char cols are converted to ints by default
                       log.transform=T,
                       remove.outliers=T,
                       outlier.range=c(-0.4, 0.4),
                       omit.nas=T,
                       do.vtreat=F,
                       engineer.features=F,
                       large.missing.features.prune=T,
                       missing.feature.cutoff.frac = 0.11,
                       features.excluded=features.excluded.xg.default,
                       do.factor.conversion=F, # For LM we exclude this becasue it makes it unscalable
                       # Convert to factors carefully
                       features.categorical=features.categorical.xg.default,
                       features.logtransformed=features.logtransformed.xg.default,
                       features.vtreat.treated=features.treated.vtreat.xg.default,
                       vtreat.opts=list(scale.features=T, usecached.plan=F, pruneSig=NULL)) {
    list[transactions, properties, recode_list, testVtreatFn, tplan] = prepareData(
         recode_chars=recode_chars,
         log.transform=log.transform,
         large.missing.features.prune=large.missing.features.prune,
         missing.feature.cutoff.frac = missing.feature.cutoff.frac,
         remove.outliers=remove.outliers,
         outlier.range=outlier.range,
         omit.nas=omit.nas,
         do.vtreat=do.vtreat, 
         engineer.features=engineer.features,
         vtreat.opts=vtreat.opts,
         features.excluded=features.excluded,
         features.logtransformed=features.logtransformed,
         features.vtreat.treated=features.vtreat.treated,
         do.factor.conversion=do.factor.conversion,
         features.categorical=features.categorical
         )


    return(list(transactions, properties, recode_list, testVtreatFn, tplan))
}

trainingAndPredictionWrapper.xg = function(
                XY,
                XTest,
                recode_list,
                params=xgbTree.grid.default,
                evalMetric="RMSE",
                summaryFunction=rmseSummary,
                preProcessFn = function(x) {x},
                dates.are.numeric=T,
                dates.matter=T,
                write.predictions=F,
                stratified.cv=F,
                folds=10,
                cvMethod="repeatedcv",
                use.snow=T,
                cluster.spec=list(host="localhost"),
                allowParallel=T,
                lastkdatecv=F,
                date.break="2016-10-01") {
    if(lastkdatecv == T) {
         dates.select = recode_list$date %>% dplyr::mutate(date=as.Date(date)) %>%
             dplyr::arrange(date) %>% dplyr::filter(date >= as.Date(date.break)) %>% pull(date_coded)
    } else {
        dates.select=NULL
    }

    print(".")
    bestFit = trainingWrapper.xg(
                 XY,
                 dates.select,
                 params=params,
                 evalMetric=evalMetric,
                 summaryFunction=summaryFunction,
                 stratified.cv=stratified.cv,
                 lastkdatecv=lastkdatecv,
                 folds=folds,
                 cvMethod=cvMethod,
                 allowParallel=allowParallel,
                 use.snow=use.snow,
                 cluster.spec=cluster.spec)

    print(summary(bestFit))
    print("..")
    ncores_p =   parallel::detectCores()
    ncores_p = ifelse(is.na(ncores_p), 4, ncores_p)
    cl <- parallel::makeCluster(rep(cluster.spec, ncores_p), outfile="xgtesting.log");
    doParallel::registerDoParallel(cl)
    predictions = propertiesDataSetPredictorsWithDateEncoding(
                                    XTest, bestFit, recode_list$date,
                                    preProcessFn=preProcessFn,
                                    dates.are.numeric = dates.are.numeric,
                                    dates.matter=dates.matter)
    parallel::stopCluster(cl)
    doParallel::stopImplicitCluster()
    print("...")
    predictions %<>% dplyr::mutate(parcelid=XTest$id_parcel)
    if(write.predictions==T) {
        print("Writing prediction")
        writePredictions(predictions, filename.suffix="regression_submission")
    }
   return(list(bestFit, predictions))
}

trainingWrapper.xg = function(XY,
            dates.select,
            params=xgbTree.grid.default,
            stratified.cv=F,
            lastkdatecv=F,
            folds=10,
            cvMethod="repeatedcv",
            evalMetric="RMSE",
            summaryFunction=rmseSummary,
            ncores=6,
            allowParallel=T,
            use.snow=T,
            cluster.spec=list(host="localhost")
            ) {
        if(stratified.cv == T) {
            print("*** Using Stratified CV")
            crossValPlan = splitKWayStratifiedCrossFoldHelper(XY %>% pull("logerror"), folds)
            train_idxs = lapply(crossValPlan,function(ci){ci$train})
            test_idxs = lapply(crossValPlan, function(ci){ci$app})
            trControl = getIndexTrControl.xg(method=cvMethod,train_idxs, test_idxs, T)
        } else if(lastkdatecv == T) {
            print("*** generating test folds in CV for last 3 months")
            crossValPlan = splitLast3MonthsCrossFold(XY, folds, dates.select)
            train_idxs = lapply(crossValPlan,function(ci){ci$train})
            test_idxs = lapply(crossValPlan, function(ci){ci$app})
            trControl = getIndexTrControl.xg(method=cvMethod,train_idxs, test_idxs, T)
        } else {
            print("*** Using randomized test folds")
            trControl = getDefaultTrControl.xg(allowParallel=allowParallel, summaryFunction=summaryFunction, folds=folds, method=cvMethod)
        }
        print("..x")

    bestFit = bestFit.xg(
        train = XY %>% dplyr::select(-logerror),
        target = XY %>% dplyr::pull(logerror),
        ncores = 6,
        tuneGrid = params,
        metric = evalMetric ,
        trControl=trControl,
        allowParallel=allowParallel,
        use.snow=use.snow,
        cluster.spec=cluster.spec)
        print("...x")
    return(bestFit)
}
