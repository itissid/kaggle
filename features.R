source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
library(magrittr)
library(lazyeval)
library(dplyr)
source("smoother.R")

getBuildingFeatures = function() {
    # TODO: modularize feature
}
getNeighborhoodFeatures = function() {
    # TODO: modularize feature
}

discretizetime.month = function(data, time.feature.name=date, time.feature.name.new=date) {
    o = enquo(time.feature.name)
    n = enquo(time.feature.name.new)
    data %>% dplyr::mutate(!!quo_name(n) := lubridate::make_date(year=lubridate::year(!!o), month=lubridate::month(!!o), day=1))
}

transformFeaturesForLinearRegression = function(
        data,
        txn.features =c("area_lot", "area_total_calc", "tax_total", "tax_land", "tax_property", "tax_building",
                        "area_total_finished", "area_live_finished", "area_firstfloor_finished", "area_garage",
                        "area_shed", "area_patio", "area_basement", "area_base", "area_unknown", "area_pool",
                        "area_liveperi_finished")) {
    # TODO: parameterize the features we want to transform as well
    # If the transformed features are in the chosen_features then they must be in the data, else error
    # I use the chosen set to create the training data and the test data set. Transformation of features
    # must also happen for a proper subset of the chosen ones only.
    # Must be in among the chosen ones.
    # and must be in the data as well.


    for(f in txn.features) {
        if(!f %in% colnames(data)) {
            print(paste("** WARN: feature ",f, " requested for log transform but not in data. Skipping."))
            next
        }
        data %<>%
            dplyr::mutate(!!f := log(!!quo(!!as.name(f)))) %>%
            dplyr::mutate(!!f := ifelse(is.infinite(!!quo(!!as.name(f))), 0, !!quo(!!as.name(f))))
    }
    return(data)
}

impactCoding = function(data, xcol.name, xcol.name.new, depvar.name=logerror) {
    xcol.name.new.enquo = enquo(xcol.name.new)
    depvar.name.enquo = enquo(depvar.name)
    xcol.name.enquo = enquo(xcol.name)
    condProbModel = impactModel(data %>% pull(!!xcol.name.enquo), data %>% pull(!!depvar.name.enquo))
    m = applyImpactModel(condProbModel, data %>% pull(!!xcol.name.enquo))
    data %<>% dplyr::mutate(!!quo_name(xcol.name.new.enquo) := m)
}
# Impact coding routines: WARNING may introduce bias in the model.
impactModel = function(xcol, depvar) {
    # xcol is your categorical variable and depvar is the variable that will be
    # whose value will be
    n = length(depvar)
    p = sum(depvar)/n
    # duplicate output for NA (average NA towards grand uniform average)
    x = c(xcol,xcol)
    y = c(depvar, depvar)
    x[(1+n):(2*n)] = NA
    levelcounts = table(x, y, useNA="always")
    condprobmodel = (levelcounts[,2] + p)/(levelcounts[,1] + levelcounts[,2] + 1.0)
    # apply model example: applyImpactModel(condprobmodel,data[,varname])
    condprobmodel
}

# apply model to column to essentially return condprobmodel[rawx]
# both NA's and new levels are smoothed to original grand average
applyImpactModel = function(condprobmodel, xcol) {
  naval = condprobmodel[is.na(names(condprobmodel))]
  dim = length(xcol)
  condprobvec = numeric(dim) + naval
  for(nm in names(condprobmodel)) {
    if(!is.na(nm)) {
      condprobvec[xcol==nm] = condprobmodel[nm]
    }
  }
  condprobvec
}

# Wrappers around mkCrossFrameNExperiment and prepare that adjust rareCount, rareSig and smFactor
createCrossFrameTreatment = function(
        XY,
        features, # Only these variables will be used for creating treatment
        #smFactor=0.01, # vary these for tuning
        #rareSig=0.01,
        #rareCount=10,
        vtreat.grid,
        YName = "logerror",
        makeLocalCluster=F,
        crossFrameCluster=NULL) {
    assertthat::assert_that(all(features %in% colnames(XY)))
    snow::clusterEvalQ(crossFrameCluster, ({
                    print("*")
                     library(vtreat)
                     library(snow)
                     library(doParallel)
                    print("**")
    }))
    print("***")
    res = snow::parLapply(crossFrameCluster, apply(vtreat.grid, 1, as.list),
            function(opts.vtreat, XY, features, YName, makeLocalCluster) {
                rareCount = opts.vtreat$rareCount
                rareSig = opts.vtreat$rareSig
                smFactor = opts.vtreat$smFactor
                fn = paste("cache/crossFrame",
                           "_smFactor_", smFactor,
                           "_rareSig_", rareSig,
                           "_rareCount_", rareCount, sep="")
                cluster = NULL
                if(makeLocalCluster == T) {
                    cluster = snow::makeCluster(as.integer(parallel::detectCores()), outfile="cluster.log")
                }
                print("***")
                #print(class(XY))
                #print(head(XY))
                #print(features)
                #print(YName)
                #print(fn)
                #print(cluster)

                #if(!exists(fn)) {
                prep = vtreat::mkCrossFrameNExperiment(
                    XY,
                    features, # Only treat these variables
                    YName,
                    rareCount=rareCount,
                    rareSig=rareSig,
                    smFactor=smFactor,
                    parallelCluster=cluster,
                    codeRestriction = c('lev', 'catN', 'clean', 'isBAD')
                    )
                    #print(prep)
                    #if(!dir.exists("cache")) dir.create("cache")
                    #saveRDS(prep, fn)
                #} else {
                #    prep = readRDS(fn)
                #}
                if(makeLocalCluster == T)
                    snow::stopCluster(cluster)
                print("*****")
                prep$opts.vtreat=opts.vtreat
                return(prep)
            },
            XY,
            features, # Only treat these variables
            YName,
            makeLocalCluster)
    print("**** ****")
    return(res)
}

applyCrossFrameToX = function(
       X, prep, pruneSig=0.01, yName="logerror", isTrain=T,
       keepDateTimeFeature=T, dateTimeColname="date") {
    # yName: only used for some assertions.

    # X can only be either the training
    scoreFrame = prep$treatments$scoreFrame
    newVars <- scoreFrame$varName[scoreFrame$sig<1/nrow(scoreFrame)]
    assertthat::assert_that(!yName %in% colnames(X))
    if(keepDateTimeFeature == T) {
        dateVars = scoreFrame %>% dplyr::filter(origName==dateTimeColname) %>% dplyr::pull(varName)
        newVars = base::union(dateVars, newVars)
    }
    print(paste(length(newVars), " variables created by preparation"))
    treated.features = scoreFrame %>% dplyr::distinct(origName) %>% dplyr::pull(origName)
    # Add to X the prepared features and remove the features from which prepared features were created
    if(isTrain==T) {
        # Training data is combined with the cross frame
        XTreated = cbind(X %>% dplyr::select(-dplyr::one_of(treated.features)), prep$crossFrame %>% dplyr::select(newVars))
        print(dim(XTreated))
        XTreated = XTreated %>% dplyr::select(-dplyr::matches("(.*catP)|(.*catD)"))
        print(dim(XTreated))
    } else {
        # Testing data is combined with the preparation applied to the test data set
        XTestTreated <- vtreat::prepare(prep$treatments, X,
                                      pruneSig=pruneSig,varRestriction=newVars)
        XTreated = cbind(X %>% dplyr::select(-dplyr::one_of(treated.features)), XTestTreated)
        XTreated = XTreated %>% dplyr::select(-dplyr::matches("(.*catP)|(.*catD)"))
    }
    print(paste(length(colnames(XTreated)), " variables in treated data"))
    return(XTreated)
}

engineerFeatures = function(X, bys = c("region_city", "region_county", "region_zip"),
                           remove_marginal=F) {
    # Average features for tax assesed and property taxes
    # There are two types of tax assessments:
    # - One for the land area of the parcel: tax_land
    # - And one for the  parcel itself: tax_total
    # Property taxes
    # variations may be using the mean or median function
    # I might want to remove these by groups to check effectiveness
    # remove the per sq feet average features by zip/county/city.
    # remote mean tax features per city/county/zip
    bys = c("region_city", "region_county", "region_zip")
    before = colnames(X)
    #X %<>% dplyr::group_by(region_city) %>% dplyr::mutate(tax_total_city = mean(tax_total, na.rm=T)) %>% ungroup()
    #X %<>% dplyr::group_by(region_zip) %>%  dplyr::mutate(tax_total_zip = mean(tax_total, na.rm=T)) %>% ungroup()
    #X %<>% dplyr::group_by(region_county) %>%  dplyr::mutate(tax_total_cty = mean(tax_total, na.rm=T)) %>% ungroup()

    meanBy = function(XTemp, fs, ftarget) {
        for(f in fs) {
            ftarget_by_f = paste(ftarget, "_", f, sep="")
            ftarget.quo = quo(!!as.name(ftarget))
            XTemp %<>%
                dplyr::group_by_(.dots=c(f)) %>%
                dplyr::mutate(!!ftarget_by_f := mean(!!ftarget.quo, na.rm=T)) %>%
                ungroup()
       }
       return(XTemp)
    }

    #################################################################
    ############# The tax total assessd value  #####################
    #################################################################
    # Per/sq feet Per room features for tax assessed values.
    # One could average these by the city, zip and county..
    #!
    X %<>% dplyr::mutate(tax_assd_persqfeet_living=tax_total/area_live_finished)
    #X %<>% dplyr::mutate(tax_assd_persqfeet_lot=tax_total/area_lot)
    #!
    X %<>% dplyr::mutate(tax_assd_perroom = tax_total/(num_room+1))


    # And there county city and zip meand
    #X %<>% meanBy(bys, "tax_assd_persqfeet_living")
    #X %<>% meanBy(bys, "tax_assd_persqfeet_lot")
    #X %<>% meanBy(bys, "tax_assd_perroom")
    #################################################################
    ############# The property taxes ###############################
    #################################################################
    X %<>% dplyr::mutate(tax_prop_persqfeet_living=tax_property/area_live_finished)
    #X %<>% dplyr::mutate(tax_prop_persqfeet_lot=tax_property/area_lot)
    X %<>% dplyr::mutate(tax_prop_perroom = tax_property/(num_room+1))
    # And their county, city and zip means
    #X %<>% meanBy(bys, "tax_prop_persqfeet_living")
    #X %<>% meanBy(bys, "tax_prop_persqfeet_lot")
    #X %<>% meanBy(bys, "tax_prop_perroom")
    proposed_removal = c("tax_property", "tax_total", "area_live_finished")
    return(list(X, setdiff(colnames(X), before), proposed_removal))
}

# Call like so:
# list[preds.location, preds.all, var.names, methods.knn] = knn.opts()
knn.opts = function() {
    preds.location=c('longitude','latitude')
    preds.all = c(preds.location,'tax_building','area_total_calc','build_year','tax_total','num_bathroom')
    var.names = c("area_liveperi_finished", "region_neighbor", "area_lot", "num_story", "num_unit")
    methods.knn =c("mahalanobis", "euclidean", "ica")
    return(list(preds.location, preds.all, var.names, methods.knn))

}
# Call knnImpute.gen.wrapper instead of knnImpute.gen directly
knnImpute.gen.wrapper = function(
                     data,
                     dataset.type,
                     k=c(5,15),
                     preds,
                     var.names,
                     instances,
                     methods.knn) {
    cluster = snow::makeCluster(instances, type="SOCK")
    data = data %>%
        dplyr::mutate(area_lot = log10(area_lot)) %>%
        dplyr::mutate(area_liveperi_finished=log10(area_liveperi_finished)) %>%
        dplyr::mutate_if(is.factor, funs(as.numeric(as.character(.)))) %>%
        select_at(dplyr::vars(preds, var.names))
    knnImpute.gen(data, dataset.type, k, preds, var.names, cl=cluster, methods.knn=methods.knn)
}

knnImpute.gen = function (
                     data,
                     dataset.type,
                     k=c(5,15),
                     preds=c('longitude','latitude','tax_building','area_total_calc','build_year','tax_total','num_bathroom'),
                    var.names,
                    cl = NULL,
                    methods.knn =c("mahalanobis", "euclidean", "ica")) {
    the.grid = expand.grid(methods.knn=methods.knn, k=k, var.names=var.names)

    knnFn = function(i.grid, data, preds) {
        print(i.grid)
        k = as.integer(i.grid$k)
        method = i.grid$methods.knn
        var.name = i.grid$var.names
        print(preds)
        x = as.formula(paste("~", paste(preds, sep='', collapse="+")))
        print(paste("Using predictors", paste(x, collapse="")))
        y = as.formula(paste("~", var.name))
        print(paste(y, collapse=""))
        print(paste("Using ", method, "method, imputing for a dataset type:", dataset.type, ", of size: ", nrow(data), " using ", k, " nearest neighbors"))
        imp = yaImpute::yai(x, y, data=data, method=method, k=k)
        #fn = paste(dataset.type, k, method, var.name, datetime, sep="_")
        # Add imputations to the data
        # return(list(imp, fn))
        return(imp)
    }

    datetime  = format(Sys.time(), "%Y%m%d_%H_%M_%S")
    if(is.null(cl)) {
        cl = snow::makeSOCKcluster(c("localhost","localhost"))
    }
    data %<>% dplyr::mutate(row_id = rownames(data))
    imputations = snow::parLapply(cl=cl, apply(the.grid, 1, as.list), knnFn, data, preds)
    for(imputation.obj in imputations) {
        predictions = yaImpute::impute(imputation.obj)
        var.name =  yaImpute::yvars(imputation.obj)
        k = imputation.obj$k
        method = imputation.obj$method
        var.name.new = paste(var.name,"_", k, "_", method, ".o", sep="")
        # The col name in predictions df is var.name . We join that to the
        # original data frame
        predictions %<>% dplyr::mutate(row_id = rownames(predictions)) %>%
            dplyr::select_at(dplyr::vars(var.name, "row_id")) %>%
            dplyr::rename(!!var.name.new := !!var.name)

        data %<>% dplyr::left_join(predictions, by=c("row_id"))
        rm(imputation.obj)
        gc()
    }
    fn = paste(dataset.type, datetime, sep="_")
    print(paste("saving Imputed data in ", fn))
    saveRDS(data, fn)
    return(imputations)
}

knnImpute.do = function(imputation_file_pattern) {
}
knnImpute.perf = function(imputation_file_pattern){
    # From the saved files gather all the satistics
}

knnImputeClassify = function(XY, predictors, response, k=15) {
    # predictors is a character vector of predictors used for training the KNN
    # The response variable is string representing the variable to be imputed.
    # The method uses complete examples from XY to learn the imputation
    # The test data is created from the examples that have missing response
    # Get the subset of indices for train and test
        # Only get the rows that are non na.
    # NOTES: convert all the factors to numeric before using
    # mutate_if(is.factor, funs(as.numeric(as.character(.))))
    if(any(mapply(function(x) {is.factor(XY %>% pull(x))}, colnames(XY)))) {
        print("*** ERROR: some features are factors, make em numeric first. Aborting")
        stopifnot(TRUE)
    }
    if(any(mapply(function(x) {is.character(XY %>% pull(x))}, colnames(XY)))) {
        print("*** ERROR: some features are character, make em numeric first. Aborting")
        stopifnot(TRUE)
    }
    XY %<>% dplyr::mutate(row_id = 1:nrow(XY))
    train.idxs = XY %>%
            dplyr::filter_at(vars(c(predictors, response)), all_vars(!is.na(.))) %>%
            dplyr::pull(row_id)
    if(length(train.idxs) == 0) {
        print(paste("Cannot impute for",  quo_name(response), " because predictor value are missing aborting"))
        return(NULL) # We must have some train data to impute
    }

    test.idxs = XY %>%
        dplyr::filter_at(vars(response), all_vars(is.na(.))) %>%
        dplyr::filter_at(vars(predictors), all_vars(!is.na(.))) %>%
        dplyr::pull(row_id)

    if (length(test.idxs) == 0) {
        print(paste("Nothing to impute for variable:",  response, " aborting."))
        return(NULL)
    }

    print(paste("Imputing", length(test.idxs), " values out of", length(test.idxs)))

    # Train: Create a Df X with preditor and response columns
    # Call estm = smoothz(X, knnreg, 5, nchunks=7)
    # Get the data to be imputed using just the predictors. Call this XTest
    # Call pred = smoothzpred(XTest, X[,predictors],estm)
    # Impute the data into the data frame at the indexes
    ncores <- as.integer(parallel::detectCores()*0.8)
    parallelCluster <- parallel::makeCluster(ncores)
    X = XY %>%
        dplyr::filter(row_id %in% train.idxs) %>%
        dplyr::select_at(vars(predictors, response))

    print(".")
    t.1 = system.time({
        estm = RANN::nn2(
                 data=as.matrix(X), k=k, treetype="kd", searchtype="standard")
    })
    #XTest = properties %>%
    #    dplyr::filter(row_id %in% test.idxs) %>%
    #    dplyr::select_at(vars(predictors))
    print(t.1)

    print("..")
    idx = RANN::nn2(data=as.matrix(X), query=x.te, k=1, treetype="kd", searchtype="standard")$nn.index
    yhat2 = estm[idx]
    print("...")

    data.frame(id=test.idxs, prediction=yhat2)
}

naImpute = function(properties, feature, impute_val) {
    properties %>% dplyr::mutate_at(vars(feature), funs(ifelse(is.na(.), impute_val, .)))
}

round1ImputeHelper = function(df, var_name, mutate_call) {
    # Mak a mutate_call like: interp(~ifelse(region_county==county & is.na(var), 0, var), county=2061, var=as.name("area_garage"))
    df %<>% dplyr::mutate_(.dots=setNames(mutate_call, var_name))
    # Given a number of fields and their values we want to impute values for them
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
