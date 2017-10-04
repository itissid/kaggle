library(caret)
building_properties = c('tax_total', 'tax_property',  'tax_land', 'build_year',  'area_total_calc', 'area_lot',  'area_total_finished', 'date')
geo_features = c('latitude',  'longitude')
neighbour_features = c('region_zip' , 'region_city', 'zoning_landuse_county',  'region_neighbor')

# Simplest model.
model.1 = logerror ~ tax_total + tax_land + tax_property + build_year + area_total_calc + area_lot + area_total_finished + date + latitude + longitude + region_city + region_zip + region_neighbor + zoning_landuse_county

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
