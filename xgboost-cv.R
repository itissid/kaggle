library(magrittr)
library(data.table)
library(xgboost)
library(caret) # For Folds
library(ModelMetrics)
library(ggplot2)
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
      out <- mse(data$obs, data$pred)  
      names(out) <- "RMSE"
      out
}

###################
# xgboost set up
###################
# Select features that form the transactions and the properties and return
# test, train and the target(Y) data in a list
createTestAndTrain = function(transactions, properties, tran_features, prop_features) {
    xtransactions = transactions %>%
	dplyr::select_(.dots = tran_features) 

    xproperties = properties %>%
	dplyr::select_(.dots = prop_features) 
    target <- transactions$logerror 

    assertthat::assert_that(nrow(transactions) == nrow(xtransactions))
    assertthat::assert_that(length(target) == nrow(xtransactions))
    assertthat::assert_that(nrow(properties) == nrow(xproperties))

    return(list(train=xtransactions, target=target, test=xproperties))

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

charColumnDecode = function(dataset)  {
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

xgBoostGridSearch = function(
           train, target, ncores=6, tuneGrid=xgbGrid.default,
           metric='MAE', summaryFunction=maeSummary) {
    require(caret)
     
    xgbTrControl <- trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 2,
      verboseIter = TRUE,
      returnData = TRUE,
      allowParallel = TRUE,
      savePredictions="final",
      summaryFunction=summaryFunction
    )
     
    xgbTrain <- train(
      x = as.matrix(train), 
      y = target,
      metric = metric,
      objective = "reg:linear",
      trControl = xgbTrControl,
      tuneGrid = tuneGrid,
      method = "xgbTree"
    )
    return(xgbTrain)
}

xgPredict = function(xgbModel, dtest) {
    preds = predict(xgbModel, dtest) 
}

xgTrainingWrapperWithVTreat = function() {
}
    
