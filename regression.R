library(caret)

# Simplest model.

bestFitByGridSearch = function(
           train, target, ncores=6, tuneGrid,
           metric='RMSE', summaryFunction=rmseSummary) {
    #require(doMC)
    #registerDoMC(cores=ncores)

    trControl <- trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 2,
      verboseIter = FALSE,
      returnData = TRUE,
      savePredictions="final",
      summaryFunction=summaryFunction
    )

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
                XY, XTest, recode_list, dates.are.numeric=T, dates.matter=T, write.predictions=T) {
   bestFit = trainingWrapper.regression(XY)
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
                           ncores=6) {
    bestFitByGridSearch(
        train = XY %>% dplyr::select(-logerror),
        target = XY %>% dplyr::pull(logerror),
        ncores = 6,
        tuneGrid = params,
        metric ="RMSE" ,
        summaryFunction = rmseSummary)
}


splitTrainingWrapper = function(XY, split_percent=0.85) {
    trainingIndices = splitTrainingData(Y = 1:nrow(XY), split_percent = split_percent)
    print(dim(XY))

    XTrain = XY[trainingIndices, ] %>% dplyr::select(-logerror) %>% na.omit

    print(dim(XTrain))
    XTest = XY[-trainingIndices, ] %>% dplyr::select(-logerror) %>% na.omit

    YTrain = XY[rownames(XTrain), ] %>% .[["logerror"]]
    YTest = XY[rownames(XTest), ] %>% .[["logerror"]]
    assertthat::assert_that(length(YTrain) == nrow(XTrain))
    assertthat::assert_that(length(YTest) == nrow(XTest))
    assertthat::assert_that(base::setequal(colnames(XTest),  colnames(XTrain)) == T)
    assertthat::assert_that(nrow(XTrain) > 0)
    assertthat::assert_that(length(YTrain) > 0)
    assertthat::assert_that(nrow(XTest) > 0)
    assertthat::assert_that(length(YTest) > 0)
    return(list(XTrain, YTrain, XTest, YTest))
}
