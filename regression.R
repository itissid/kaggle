library(caret)
building_properties = c('tax_total', 'tax_property',  'tax_land', 'build_year',  'area_total_calc', 'area_lot',  'area_total_finished', 'date')
geo_features = c('latitude',  'longitude')
neighbour_features = c('region_zip' , 'region_city', 'zoning_landuse_county',  'region_neighbor')

# Simplest model.
model.1 = logerror ~ tax_total + tax_land + tax_property + build_year + area_total_calc + area_lot + area_total_finished + date + latitude + longitude + region_city + region_zip + region_neighbor + zoning_landuse_county


lmByGridSearch = function(
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
    factors = names(Filter(is.factor, train))
    if(length(factors) > 0) {
        print(
          paste("***** WARN: some factor columns passed ", 
                paste(factors, collapse=", " ), " converting them to numeric"))
    } else{
        for(fctr in factors) {
            train[[fctr]] = as.numeric(as.character(train[[fctr]]))
        }
    }
    print(str(as.matrix(train)))
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

lmBoostrap = function()
{
    source("utils.R")
    require(magrittr)
    params = data.frame(
          intercept=F
    )
    X = transactions %>%
        dplyr::filter(logerror<=0.4 & logerror >=-0.4) %>%
        dplyr::select_(.dots = chosen_features)  %>% na.omit

    list[X, Xdict] = charColumnEncode(X)


    sort(colnames(X))
    dim(X)
    dim(transactions)

    trainingIndices = splitTrainingData(Y = transactions$logerror, split_percent = 0.95)

    dim(X[trainingIndices, ])
    str(X)
    bestFit = bestFitByGridSearch(
        train = X[trainingIndices, ], target = transactions$logerror[trainingIndices], ncores = 6, tuneGrid = params,
        metric ="RMSE" , summaryFunction = rmseSummary)
}


transformFeaturesForLinearRegression = function(
        data, txn.features =c("area_lot", "area_total_calc", "tax_total", "tax_land", "tax_property")) {
    # Must be in among the chosen ones.
    # and must be in the data as well.
    assertthat::assert_that(all(mapply((function(i) {i %in% colnames(data)}), txn.features)))

    for(f in txn.features) {
        data %<>%
            dplyr::mutate(!!f := log(!!quo(!!as.name(f))))
    }
    return(data)
}
