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
    "area_total_calc", # causes full rank issues in LM
    "num_bathroom_calc", # caused rank issues in Lm
    "id_parcel",
    "pooltypeid2", # Redundant to pooltype10
    "pooltypeid7", # redundant to pooltype10
    "censustractandblock", # redundant
    "rawcensustractandblock", # broken into block and tract
    "fips", # redundant
    "census" # redundant
)

features.logtransformed.regression.default = c("area_lot", "area_total_calc", "tax_total", "tax_land", "tax_property", "tax_building",
                        "area_total_finished", "area_live_finished", "area_firstfloor_finished", "area_garage", 
                        "area_shed", "area_patio", "area_basement", "area_base", "area_unknown", "area_pool",
                        "area_liveperi_finished")
# Quite a few features here are dropped because of the large # of categories
# Ideally we should do multilevel regression for some of them
features.categorical.regression.default = c(
    "region_city",
    "region_county",
    "region_neighbor",
    "region_zip",
    "zoning_landuse",
    "zoning_property",
    "zoning_landuse_county",
    "quality",
    "framing",
    "architectural_style",
    "num_unit",
    "build_year",
    "date",
    "tax_delinquency_year",
    "tract_number",
    "tract_block"
)
# TODO: Maybe we can use certain permutations of this to see what improves prediction
features.treated.vtreat.regression.default = c(
    "region_city",
    "region_county",
    "region_neighbor",
    "region_zip",
    "zoning_landuse",
    "zoning_property",
    "zoning_landuse_county",
    "build_year",
    "tax_delinquency_year",
    "tract_number",
    "tract_block"
)

# list[trans, props, rlist] =  prepareDataWrapper.regression()
# list[fit.lm, predictions] = trainingAndPredictionWrapper.regression(trans, props, rlist)
# For vtreat the call is
# list[trans, props, rlist, testVTreatFn, tplan] =  prepareDataWrapper.regression( do.vtreat=T, features.categorical=c())
# To debug faster we can cache the cross frame:
# list[trans, props, rlist, testVTreatFn, tplan] =  prepareDataWrapper.regression(do.vtreat=T, features.categorical=c(), vtreat.opts=list(scale.features=T, usecached.plan=T))
# list[fit.lm, predictions]  = trainingAndPredictionWrapper.regression(trans, props, rlist, testVTreatFn)
# If the # of categorical variables become too large then lm has issues, so take care during automation.
prepareDataWrapper.regression = function(
                       recode_chars=T, # Char cols are converted to ints by default
                       log.transform=T,
                       remove.outliers=T,
                       omit.nas=T,
                       do.vtreat=F,
                       large.missing.features.prune=T,
                       features.excluded=features.excluded.regression.default,
                       do.factor.conversion=F, # For LM we exclude this becasue it makes it unscalable
                       features.categorical=features.categorical.regression.default,
                       features.logtransformed=features.logtransformed.regression.default,
                       features.vtreat.treated=features.treated.vtreat.regression.default,
                       vtreat.opts=list(scale.features=T, usecached.plan=F, pruneSig=NULL)) {

    list[transactions, properties, recode_list, testVtreatFn, tplan] = prepareData(
         recode_chars=recode_chars,
         log.transform=log.transform,
         large.missing.features.prune=large.missing.features.prune,
         remove.outliers=remove.outliers,
         omit.nas=omit.nas,
         do.vtreat=do.vtreat,
         vtreat.opts=vtreat.opts,
         features.excluded=features.excluded,
         features.logtransformed=features.logtransformed,
         features.vtreat.treated=features.vtreat.treated,
         do.factor.conversion=do.factor.conversion,
         features.categorical=features.categorical
         )


    return(list(transactions, properties, recode_list, testVtreatFn, tplan))
}

trainingAndPredictionWrapper.regression = function(
                XY, XTest, recode_list, preProcessFn = function(x) {x}, 
                dates.are.numeric=T, dates.matter=T, write.predictions=F,
                stratified.cv=F, folds=10, lastkdatecv=F, date.break="2016-10-01") {
   if(lastkdatecv == T) {
        dates.select = recode_list$date %>% dplyr::mutate(date=as.Date(date)) %>%
            dplyr::arrange(date) %>% dplyr::filter(date >= as.Date(date.break)) %>% pull(date_coded)
   } else {
       dates.select=NULL
   }

   print(".")
   bestFit = trainingWrapper.regression(
                XY,
                dates.select,
                stratified.cv=stratified.cv,
                lastkdatecv=lastkdatecv,
                folds=folds)

   print(summary(bestFit))
   print("..")
   predictions = propertiesDataSetPredictorsWithDateEncoding(
                                    XTest, bestFit, recode_list$date,
                                    preProcessFn=preProcessFn,
                                    dates.are.numeric = dates.are.numeric,
                                    dates.matter=dates.matter)
    print("...")
    predictions %<>% dplyr::mutate(parcelid=XTest$id_parcel)
    if(write.predictions==T) {
        print("Writing prediction")
        writePredictions(predictions, filename.suffix="regression_submission")
    }
   return(list(bestFit, predictions))
}

trainingWrapper.regression = function(
          XY, dates.select, params=data.frame(intercept=F), ncores=6, stratified.cv=F, lastkdatecv=F,
          folds=10) {
    if(stratified.cv == T) {
        print("*** Using Stratified CV")
        crossValPlan = splitKWayStratifiedCrossFoldHelper(XY %>% pull("logerror"), folds)
        train_idxs = lapply(crossValPlan,function(ci){ci$train})
        test_idxs = lapply(crossValPlan, function(ci){ci$app})
        trControl = getIndexTrControl(train_idxs, test_idxs, T)
    } else if(lastkdatecv == T) {
        print("*** generating test folds in CV for last 3 months")
        crossValPlan = splitLast3MonthsCrossFold(XY, folds, dates.select)
        train_idxs = lapply(crossValPlan,function(ci){ci$train})
        test_idxs = lapply(crossValPlan, function(ci){ci$app})
        trControl = getIndexTrControl(train_idxs, test_idxs, T)
    } else {
        print("*** Using randomized test folds")
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
