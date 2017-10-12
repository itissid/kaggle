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

knnPerformanceComparison = function(XY, predictors, response) {
        # fast knn handily beats the knn I use .
        t3 = system.time({XY = XY %>%
            select_at(vars(predictors, response)) %>%
            filter_at(vars(predictors, response), all_vars(!is.na(.))) %>%
            dplyr::mutate_if(is.factor, funs(as.numeric(as.character(.))))
        X = XY %>% select(predictors)
        Y = XY %>% pull(response)
        # This step is slow, I could do better by just sampling across all non na
        # columns and save those splits
        splt <- vtreat::kWayStratifiedY(nrow(X), 10, NULL, Y)
        tr.idx  = splt[[1]]$train
        rm(splt)
        })
        print(t3)
        x.tr <- X[tr.idx,] %>% as.matrix
        x.te <- X[-tr.idx,] %>% as.matrix
        y.tr <- Y[tr.idx]
        y.te <- Y[-tr.idx]
        print(dim(x.tr))
        print(dim(x.te))
        t2 <- system.time({
              ncores <- as.integer(parallel::detectCores()*0.8)
              cl <- snow::makeCluster(rep("localhost", ncores), type = "SOCK")
              estm = smoothz(cbind(x.tr, y=y.tr), knnreg, k=10 , nchunks=20, cls=cl)
              print("**")
              idx = RANN::nn2(data=x.tr, query=x.te, k=1, treetype="kd", searchtype="standard")$nn.index
              yhat2 = estm[idx]
              snow::stopCluster(cl)
        })
        print(t2)
        t1 <- system.time({
              yhat1 <- fastknn::fastknn(xtr = x.tr, ytr = as.factor(y.tr), xte = x.te, k = 10, method = "dist")
        })
        print(t1)
}

knnPerformance = function(f.impute, X, predictors, k=50, nchunks=7) {
    # Measure the performance of KNN by splitting the data and then mes
    mkDataFrame = function(df, response) {
        print(response)
        X.fpresent = df %>%
            select_at(vars(predictors, response)) %>%
            filter_at(vars(predictors, response), all_vars(!is.na(.))) %>%
            dplyr::mutate_if(is.factor, funs(as.numeric(as.character(.))))
        # Introduce some NA's into X
        X.model = X.fpresent %>%
            dplyr::mutate(!!response :=ifelse(rbinom(nrow(X.fpresent), 1, 0.9) == 1, !!quo(!!as.name(response)), NA))
        assertthat::assert_that(nrow(X.model) > 0)
        # get the results from the imputation routine
        result = knnImputeClassify(X.model, predictors, response, k=k, nchunks=nchunks)
        # Measure RMSE against the true data set
        rmse = sqrt(mean((result$prediction - X.fpresent[result$id, response])^2))
        return(rmse)
    }

    lapply(X=f.impute, FUN=mkDataFrame, df=X)
}
