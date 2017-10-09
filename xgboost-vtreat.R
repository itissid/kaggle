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

xgBoostVtreatPredictionWrapper = function(X, fitObj, recode_list.for.date, tplan, features.vtreat.restricted) {

    print(".")
    testVtreat <- vtreat::prepare(
        tplan[[1]]$treatments, X,
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
    predictions %<>% dplyr::mutate(parcelid=X$id_parcel)
    return(predictions)

}

xgBoostVtreatTrainWrapper = function(X, cluster.spec.massive, scale.features=T) {
    # Allows for unrolling return args https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value

    ########### Create the cross frame ##########

    tplan <- createCrossFrameTreatment(
        X,
        features=setdiff(colnames(X), "logerror"),
        vtreat.grid=data.frame(smFactor = 0.01, rareCount = 50, rareSig = 0.01),
        makeLocalCluster=T,
        crossFrameCluster=snow::makeCluster(1))
    print("...")
    ########### Create  cross validation folds using startified k fold #######

    crossValPlan = splitKWayStratifiedCrossFoldHelper(X %>% pull("logerror"),10)
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
        X, ############# TAKE CARE OF THE DATA SET NAME ###########
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
