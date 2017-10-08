source("utils.R")
source("features.R")
source("xgboost-cv.R")
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

xgBoostVtreatTrainWrapper = function(cluster.spec.massive) {
    # Allows for unrolling return args https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
    list[transactions, properties] = readAndTransformData() 
    transactions %<>%transformCoords()
    properties %<>% transformCoords()
    print(".")

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

    # Recode all char data into numerics converting their missing vals(blanks, empty strings into )
    list[transactions, properties, recode_list] = recodeCharacterColumns(transactions, properties)
    rm(properties)
    # Impute tax delinquency vars only
    transactions_cleaned = transactions %>% round1Impute()

    transactions_cleaned %<>% dplyr::select(-dplyr::one_of(features.excluded))

    transactions_cleaned %<>% dplyr::mutate_at(dplyr::vars(features.categorical), as.factor)

    #Scale the variables that are orders of magnitude larger.
    transactions_cleaned_log_transformed = transactions_cleaned  %>% transformFeaturesForLinearRegression()
    assertthat::assert_that(nrow(transactions_cleaned_log_transformed) == nrow(transactions))
    print("..")

    ########### Create the cross frame ##########

    tplan <- createCrossFrameTreatment(
        transactions_cleaned_log_transformed, 
        features=setdiff(colnames(transactions_cleaned_log_transformed), "logerror"),
        vtreat.grid=data.frame(smFactor = 0.01, rareCount = 50, rareSig = 0.01),
        makeLocalCluster=T,
        crossFrameCluster=snow::makeCluster(1))
    print("...")
    ########### Create  cross validation folds using startified k fold #######

    crossValPlan = splitKWayStratifiedCrossFoldHelper(transactions_cleaned_log_transformed %>% pull("logerror"),10)
    train_idxs = mapply(function(ci){ci$train}, crossValPlan)
    test_idxs = mapply(function(ci){ci$app}, crossValPlan)
    trControl = getIndexTrControl(train_idxs, test_idxs, T)

    ############## THE TUNING GRID FOR FINDING THE BEST PARAMS ################
    xg.params = expand.grid(
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
        transactions_cleaned_log_transformed, ############# TAKE CARE OF THE DATA SET NAME ###########
        varRestriction = features.vtreat.restricted, ############# VARIABLES PRUNED BY SIG. IN THE CROSS FRAME ########
        pruneSig=0.01)

    print("....")
    ############### Thrusters on for training #########
    bestFit = xgBoostGridSearch(train = trainVtreat %>% select(-logerror), target = trainVtreat[["logerror"]], 
                  tuneGrid = xg.params, xgbTrControl = trControl, use.snow=T, cluster.spec=cluster.spec.massive)
    print(".....")
    return(bestFit)
}
