source("utils.R")
source("features.R")
source("xgboost-cv.R")
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

features.categorical = c("region_city",
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

features.excluded = c(
"id_parcel",
"pooltypeid2",
"pooltypeid7",
"censustractandblock",
"rawcensustractandblock",
"fips",
"census",
"tax_year"
)

xgBoostVtreatPredictionWrapper = function(XTest, fitObj, recode_list.for.date, tplan, features.vtreat.restricted) {

    print(".")
    testVtreat <- vtreat::prepare(
        tplan[[1]]$treatments, XTest,
        varRestriction = features.vtreat.restricted, ############# VARIABLES PRUNED BY SIG. IN THE CROSS FRAME ########
        pruneSig=0.01)
    cl <- parallel::makeCluster(as.integer(parallel::detectCores()*3/4), outfile="xgpredict.log")
    doParallel::registerDoParallel(cl)
    print("..")
    predictions = propertiesDataSetPredictorsWithDateEncoding(
                                    testVtreat, fitObj, recode_list.for.date, dates.are.numeric = T,
                                    dates.matter=F)
    print(head(predictions))
    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)
    print("...")
    predictions %<>% dplyr::mutate(parcelid=XTest$id_parcel)
    return(predictions)

}

xgBoostVtreatTrainWrapper = function(XY, cluster.spec.massive, scale.features=T) {
    # Allows for unrolling return args https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value

    ########### Create the cross frame ##########

    tplan <- createCrossFrameTreatment(
        XY,
        features=setdiff(colnames(XY), "logerror"),
        vtreat.grid=data.frame(smFactor = 0.01, rareCount = 50, rareSig = 0.01),
        makeLocalCluster=T,
        crossFrameCluster=snow::makeCluster(1))
    print("...")
    ########### Create  cross validation folds using startified k fold #######

    crossValPlan = splitKWayStratifiedCrossFoldHelper(XY %>% pull("logerror"),10)
    train_idxs = mapply(function(ci){ci$train}, crossValPlan)
    test_idxs = mapply(function(ci){ci$app}, crossValPlan)
    trControl = getIndexTrControl(train_idxs, test_idxs, T)

    ############## THE TUNING GRID FOR FINDING THE BEST PARAMS ################
    xg.params = expand.grid (
          nrounds = 800, # max # iterations
          max_depth = c(3, 4), # max depth of the tree
          eta = 0.3, # Learning rate
          gamma = 0,  # the minimum loss reduction to make a partition
          colsample_bytree = c(0.5, 0.75, 1), # ratio of columns when constructing the tree
          min_child_weight = c(1, 2),
          subsample= c(0.7, 1)
    )

    assertthat::assert_that(nrow(tplan[[1]]$crossFrame) > 0)
    vScoreFrame <- tplan[[1]]$treatments$scoreFrame
    scoreFrame.filtered <- vScoreFrame %>% dplyr::filter(sig<=0.01)
    features.vtreat.restricted = scoreFrame.filtered %>% dplyr::pull(varName) %>% sort

    trainVtreat <- vtreat::prepare(
        tplan[[1]]$treatments,
        XY, ############# TAKE CARE OF THE DATA SET NAME ###########
        varRestriction = features.vtreat.restricted, ############# VARIABLES PRUNED BY SIG. IN THE CROSS FRAME ########
        pruneSig=0.01,
        scale=scale.features)

    print("....")
    ############### Strat the training #########
    bestFit = xgBoostGridSearch(train = trainVtreat %>% select(-logerror), target = trainVtreat[["logerror"]],
                  tuneGrid = xg.params, xgbTrControl = trControl, use.snow=T, cluster.spec=cluster.spec.massive)
    print(".....")
    return(list(bestFit, tplan))
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

