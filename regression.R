library(caret)


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

# Simplest model.
bestFitByGridSearch = function(
           train, target, ncores=6, 
           tuneGrid,
           allowParallel=T,
           metric='RMSE', summaryFunction=rmseSummary,
           trControl= getDefaultGrid(allowParallel, summaryFunction)) {
    #require(doMC)
    #registerDoMC(cores=ncores)


    model <- caret::train(
      x = as.matrix(train),
      y = target,
      metric = metric,
      trControl = trControl,
      tuneGrid = tuneGrid,
      method = "lm"
    )
    return(model)
}

rmseSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
      data.t = data %>% na.omit
      out <- ModelMetrics::mse(data.t$obs, data.t$pred)
      names(out) <- "RMSE"
      out
}

features.excluded.regression.default = c(
    "id_parcel",
    "area_total_calc", # causes full rank issues in LM
    "num_bathroom_calc", # So closely correlated to the num_bathroom_calc
    "num_bath", # So closely correlated to the num_bathroom_calc
    "pooltypeid2", # Redundant to pooltype10
    "pooltypeid7", # redundant to pooltype10
    "censustractandblock", # redundant
    "rawcensustractandblock", # broken into block and tract
    "fips", # redundant
    "census", # redundant
    "tax_year",
    "region_city", # Excluded because large arity categorical vars are not useful for lm model
    "region_zip",
    "region_neighbor",
    "tract_number",
    "tract_block",
    "region_county",
    "zoning_landuse" #
)


# Quite a few features here are dropped because of the large # of categories
# Ideally we should do multilevel regression for some of them
features.categorical.regression.default = c(
    "zoning_property",
    "quality",
    "framing",
    "architectural_style",
    "num_unit"
)

# list[trans, props, rlist] =  prepareDataWrapper.regression()
# list[fit.lm, predictions] = trainingAndPredictionWrapper.regression(trans, props, rlist)
prepareDataWrapper.regression = function(
                       recode_chars=T, # Char cols are converted to ints by default
                       log.transform=T,
                       remove.outliers=T,
                       omit.nas=T,
                       features.excluded=features.excluded.regression.default,
                       features.categorical=features.categorical.regression.default) {

   prepareData(
         recode_chars=recode_chars,
         log.transform=log.transform,
         omit.nas=omit.nas,
         features.excluded=features.excluded,
         features.categorical=features.categorical)
}

trainingAndPredictionWrapper.regression = function(
                XY, XTest, recode_list, dates.are.numeric=T, dates.matter=T, write.predictions=T, 
                stratified.cv=F) {
   bestFit = trainingWrapper.regression(XY, stratified.cv=stratified.cv)
   print(summary(bestFit))

   print(".")
   predictions = propertiesDataSetPredictorsWithDateEncoding(
                                    XTest, bestFit, recode_list$date,
                                    dates.are.numeric = dates.are.numeric,
                                    dates.matter=dates.matter)
    print("..")
    predictions %<>% dplyr::mutate(parcelid=XTest$id_parcel)
    if(write.predictions==T) {
        print("Writing prediction")
        writePredictions(predictions, filename.suffix="regression_submission")
    }
   return(list(bestFit, predictions))
}

trainingWrapper.regression = function(XY,
                           params=data.frame(intercept=F),
                           ncores=6, 
                           stratified.cv=F) {
    if(stratified.cv == T) {
        crossValPlan = splitKWayStratifiedCrossFoldHelper(XY %>% pull("logerror"), 10)
        train_idxs = mapply(function(ci){ci$train}, crossValPlan)
        test_idxs = mapply(function(ci){ci$app}, crossValPlan)
        trControl = getIndexTrControl(train_idxs, test_idxs, T)
    } else {
        trControl = getDefaultTrControl()
    }
    bestFitByGridSearch(
        train = XY %>% dplyr::select(-logerror),
        target = XY %>% dplyr::pull(logerror),
        ncores = 6,
        tuneGrid = params,
        metric ="RMSE" ,
        summaryFunction = rmseSummary,
        trControl=trControl)
}
