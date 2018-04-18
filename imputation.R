# TODO: We should look at each imputed feature and see if it introduces any benefit. Look at the RMSE for each combination?

round1ImputeHelper = function(df, var_name, mutate_call) {
    # Mak a mutate_call like: interp(~ifelse(region_county==county & is.na(var), 0, var), county=2061, var=as.name("area_garage"))
    df %<>% dplyr::mutate_(.dots=setNames(mutate_call, var_name))
    # Given a number of fields and their values we want to impute values for them
}


# TODO: impute for heating and aircon by kNN. Use the zerodist to impute as much as I can.
# TODO: drop the "story" property and use the "area_basement" instead. Impute 0 where this property is missing. See the captains log for why basements are sparse in SoCAL
# TODO: drop architectural_style if I haven't already.
# TODO: drop material for now as I am not sure imputing is of any use.
# TODO: for tax properties like tax_total, tax_land and tax_property. I can try and use the kNN for not just distance but other features in the space: longitude, latitude, area_firstfloor_finished, area_total_calc, num_bathroom, num_bedroom, year_built, tax_property, tax_land, tax_total, unitcnt
# TODO: Impute same value for region_county and region_neighbor by just knn on long lat.
# TODO: Impute same value for num_garage, area_garage features using zerodist thingy.
# TODO: Impute num_stories to use same value using zerodist.
# TODO: We drop pooltypeid2 and pooltypeid7 and poolcnt is retained as num_pool. We impute value using zerodist and then replace NA with 0.
# TODO: poolidtype10 and hashottuborspa seem to reflect the same property but not sure which one is better. Compare these

# Order of imputation: First impute the region_county and region_neighbour proprerties.

# NOTED: Issue with imputation using lat long in properties: 11437 rows in properties are not having longitude and latitude and region_county property.
imputeNA0 = function(df, features) {
    for(f in features) {

        if(is.factor(df[, f])) {
            facna = addNA(df[, f])
            stopifnot(!("0" %in% levels(facna)))
            levels(facna) = c(levels(df[, f]), 0)
            df[, f] = facna
        } else {
            stopifnot(!(0 %in% df[, f]))
            df[is.na(df[, f]), f] = 0
        }
    }
    df
}

round1Impute = function(df) {
    # Simply impute 0 for na for certain (county subset, feature) pair
    createRule = function (counties, var_name) {
            interp(~ifelse(region_county %in% counties & is.na(x), 0, x), x=as.name(var_name))

    }
    # TODO: consider using a "selector" to test various imputations.
    imputeRules = list(
            #area_garage=createRule(c(1286, 2061), "area_garage"),
            #area_basement=createRule(c(2061), "area_basement"),
            #deck=createRule(c(2061), "deck"),
            #flag_fireplace = createRule(c(1286), "flag_fireplace"),
            #area_patio = createRule(c(2061), "area_patio"),
            #area_pool = createRule(c(2061), "area_pool"),
            #area_shed = createRule(c(2061), "area_shed"),
            #framing = createRule(c(3101), "framing"),
            #num_pool = createRule(c(1286, 2061, 3101), "num_pool"),
            #num_fireplace = createRule(c(1286, 2061), "num_fireplace"),
            #num_garage = createRule(c(1286, 2061), "num_garage"),
            tax_delinquency_year = createRule(c(1286, 2061, 3101), "tax_delinquency_year"),
            tax_delinquency = createRule(c(1286, 2061, 3101), "tax_delinquency")
    )
    for(n in names(imputeRules)) {
        df = round1ImputeHelper(df, n, imputeRules[n])
    }
    return(df)
    # area_garage       Impute 0 for CT 1286, 2061
    # area_basement     Impute 0 for 2061 county in place of NA.
    # deck		Impute 0 for 2061 only.
    # flag_fireplace    Impute 0 for NA county 1286 only.
    # area_patio        Impute 0 for NA county 2061 only
    # area_pool		Impute 0 for NA county 2061 only
    # area_shed		Impute 0 for NA county 2061 only
    # framing		Impute 0 for NA for 3101 only.
    # num_pool		Impute 0 for NA in all counties
    # num_fireplace     Impute 0 for NA in county 1286, 2061 only.
    # num_garage        Impute 0 for NA in county 1286, 2061 only.
    # tax_delinquency_year		Impute 0 for NA.
}

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
