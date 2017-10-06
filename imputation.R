# TODO: We should look at each imputed feature and see if it introduces any benefit. Look at the RMSE for each combination?
imputation_provider.grid = expand.grid(
                               k = c(5, 15), 
                               algo=c("euclidean", "mahalanbobis", "ica"),
                               features=c("area_liveperi_finished", "region_neighbor", "area_lot", "num_story", "num_unit"))

getImputedCleanedDf = function(k = 5, algo="euclidean") {
    # use a certain data set for imputation
    transactions.imputed = readRDS("imputations/transactions_20171002_07_12_06")
    pattern = paste("_", k, "_", algo, sep="")
    transactions.imputed %<>%
        dplyr::select(dplyr::contains(pattern))
    colnameMappings = t(mapply(function(x) { a = strsplit(x, pattern); return(c(a[[1]][[1]], x)) }, colnames(transactions.imputed), USE.NAMES=F))
    colnames(colnameMappings) = c("new", "old")  
    print("..I")
    transactions.imputed %>% dplyr::rename_(
        .dots=with(data.frame(colnameMappings), setNames(as.list(as.character(old)), new)))

}

addImputationsToTransactions = function(transactions, features.imputed = c("area_liveperi_finished", "region_neighbor", "area_lot", "num_story", "num_unit")) {

    imputations = getImputedCleanedDf()
    # Probably call the xgTrainingWrapper
    for(f in features.imputed) {
        imputedvals = imputations[[f]]
        transactions %<>% dplyr::mutate(!!f := imputedvals)
    }
    return(transactions)
}

xgBoostWithImputationWrapper = function(transactions, features.restricted, features.scaled, xgBoostTraining.grid, 
                                        features.imputed = c("area_liveperi_finished", "region_neighbor", "area_lot", "num_story", "num_unit")) {
    # TODO: try other options later for fine tuning
    features.restricted = c(features.restricted, features.imputed)
    features.scaled = c(features.scaled, "area_lot")
    xgTrainingWrapper(transactions, features.restricted = features.restricted, features.scaled = features.scaled, 
                                       parallelTraining=T, xgBoostTrainingGrid = xgBoostTraining.grid)
}
