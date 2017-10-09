# TODO: We should look at each imputed feature and see if it introduces any benefit. Look at the RMSE for each combination?
imputation_provider.grid = expand.grid(
                               k = c(5, 15),
                               algo=c("euclidean", "mahalanbobis", "ica"),
                               features=c("area_liveperi_finished", "region_neighbor", "area_lot", "num_story", "num_unit"))

getImputedCleanedDf = function(k = 5, algo="euclidean", type="transactions") {
    # use a certain data set for imputation
    if(type=="transactions") {
        X.imputed = readRDS("imputations/transactions_20171002_07_12_06")
    } else if(type == "properties") {
        X.imputed = readRDS("imputations/properties_20171002_09_17_48")
    }
    pattern = paste("_", k, "_", algo, sep="")
    X.imputed %<>%
        dplyr::select(dplyr::contains(pattern))
    colnameMappings = t(mapply(function(x) { a = strsplit(x, pattern); return(c(a[[1]][[1]], x)) }, colnames(X.imputed), USE.NAMES=F))
    colnames(colnameMappings) = c("new", "old")
    print("..I")
    X.imputed %>% dplyr::rename_(
        .dots=with(data.frame(colnameMappings), setNames(as.list(as.character(old)), new)))

}

addImputationsToTransactions = function(X, type="transactions",
                                        features.imputed = c("area_liveperi_finished", "region_neighbor", "area_lot", "num_story", "num_unit")) {

    imputations = getImputedCleanedDf(type=type)
    # Probably call the xgTrainingWrapper
    for(f in features.imputed) {
        imputedvals = imputations[[f]]
        X %<>% dplyr::mutate(!!f := imputedvals)
    }
    return(X)
}

