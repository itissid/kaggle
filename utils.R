library(magrittr)
library(lazyeval)
library(futile.logger)
source("datadefs.R")
source("utils/io.R")
source("utils/spatial.R")
source("features.R")
source("imputation.R")
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

#properties with high 75th percentile of the abs_error.

readAndRenameFeatures = function(pr='inputs/properties_2016.csv', tr = 'inputs/train_2016_v2.csv') {
    # Minimal processing of features to make them more human readable

    properties <- readPropertiesFile(pr)
    transactions <- readPropertiesFile(tr)
    #sample_submission <- data.table::fread('inputs/sample_submission.csv')
    properties %<>% makePropertiesFeaturesReadable()
    transactions %<>% makeTransactionFeaturesReadable()
    # Split the census raw data into meaningful categories
    properties %<>%
        dplyr::mutate(census = as.character(rawcensustractandblock),
               tract_number = as.numeric(stringr::str_sub(census,5,11)),
               tract_block = as.numeric(stringr::str_sub(census,12)))
    return(list(transactions,  properties))
}

missing_fn = function(pr, variable) {
	 variable_enquo = enquo(variable)
	 pr %>%
             group_by(region_county) %>%
             summarize(not_missing = sum(!is.na(!!variable_enquo)),
                       missing_pct=sum(is.na(!!variable_enquo))/n())
}

# Convenience function to copy text to OSX clipboard
copy_to_clip = function(obj) {
    clip <- pipe("pbcopy", "w")
    write.table(obj, file=clip, sep='\t\t', quote = F, row.name=F)
    close(clip)
}
# A Wrapper function that combines several steps into one:
# 1. read the data,
# 2. rename the columns to human readable form
# 3. Clean up the data: Remove outliers, remove rows with certain amounts of missing data. etc
# 3. Do basic feature engineering: Log Transformation, Using vtreat, convert features to factors
# Things to take care of:
    # Generally speaking  addition of things to the data set, specifically new columns and imputed values should come BEFORE
    # deletion of data, example removing rows/columns with too many NAs, outlier removal etc
    # Removing of columns(e.g. with too many NA's) should be done after imputations.

# This routine is called by other algorithms and those are the ones that set the default.
prepareData = function(
                       pr='inputs/properties_2016.csv',
                       tr = 'inputs/train_2016_v2.csv',
                       ### Feature engineering ###
                       log.transform=T,
                       large.missing.features.prune=T,
                       do.vtreat=F,
                       taxbyarea.normalize=F,
                       vtreat.opts=list(),
                       features.logtransformed=c(),
                       features.vtreat.treated=c(), # WITH vtreat
                       categorical.to.numeric=T,
                       convert.to.categorical=F,
                       features.categorical, # which features to convert to factors
                       impute.NA.0 = F,
                       impute.NA.0.features = c(),
                       #### Data cleanup ####
                       missing.feature.cutoff.frac = 0.10,
                       remove.outliers=T,
                       outlier.range=c(-0.4, 0.4),
                       omit.nas=F,
                       features.excluded=c()
                       ) {

    # Renames columns to be more reasonable,
    # puts lats and logs in correct format
    # converts all chars in props and trans to integers. And converts the feature.categorical to factors
    # Optionally log transforms the large area_ and tax_ features
    list[transactions, properties] = readAndRenameFeatures(pr, tr)
    transactions.tmp = transactions
    properties.tmp = properties
    properties.tmp %<>% transformCoords()
    flog.debug(str(properties.tmp[, 1:5]))
    flog.debug(".")

    # Recode all char data into numerics converting their missing vals(blanks, empty strings into )
    if(convert.to.categorical==T) {
        properties.tmp %<>%
            mutate_at(vars(features.categorical), funs(as.factor(.)))
    }
    if(categorical.to.numeric == T) {
        flog.debug(paste("*** Converting following categorical variables to numeric: ", paste(features.categorical, collapse=', ')))
        properties.tmp %<>%
            mutate_at(vars(features.categorical), funs(as.numeric(as.factor(.))))
    }
    flog.debug(str(properties.tmp[, 1:5]))
    #if(recode_chars == T) {
    #    list[properties.enc, props.recode_list] = recodeCharacterColumns(properties)
    #    list[transactions.enc, tran.recode_list] = recodeCharacterColumns(transactions, features=c("date"))
    #} else {
    #    recode_list = NULL
    #}
    # Impute some common sense variables
    # transactions.tmp = transactions.enc %>% round1Impute()
    # properties.tmp = properties.enc %>% round1Impute()
    if(impute.NA.0) {
        if(length(impute.NA.0.features) > 0) {
            flog.info(paste("Imputing the following features with 0",
                            paste0(impute.NA.0.features, collapse=', ')))
            properties.tmp %<>% imputeNA0(features=impute.NA.0.features)
        } else {
            flog.warn("Imputation flag was on but no features provided")
        }
    }

    #########################################################################
    ############### GET THE TREATMENTS BEFORE YOU DO ANYTHING ELSE ##########
    #########################################################################
    if(do.vtreat==T) {
        if(log.transform == T) {
            shouldnotbehappening = intersect(features.logtransformed, features.vtreat.treated)
            if(length(shouldnotbehappening) > 0) {
                flog.warn(paste("features that are treated cannot be log transformed",
                            paste(shouldnotbehappening, sep=", ")))
            }
        }

        list[tplan, trainVtreat, testVtreatFn] = prepareDataFeaturesWithVtreat(transactions.tmp, features.vtreat.treated, vtreat.opts)
        flog.debug("..")

        features.excluded = features.excluded
        # add all the features that are treated already to the exclusion thingy
        flog.info(paste("***", length(features.vtreat.treated), "features are additionally excluded from preprocessing due to vtreat treatment: ",
                    paste(features.vtreat.treated, collapse=", ")))
    } else{
        testVtreatFn = function(x){x} # Dummy function
        tplan=NULL
    }
    #######################################################################
    ######### Feature engineering before log xfrm, but after vtreat    ####
    #######################################################################
    if(taxbyarea.normalize == T) {
        list[properties.tmp, taxbyarea.normalize.added, rm.prop] = taxByAreaTransform(
                              properties.tmp)
        flog.debug(paste("Added", length(taxbyarea.normalize.added), " engineered features:"))
        flog.debug(paste(paste(taxbyarea.normalize.added, collapse="\n\t"), "\n"))
        features.excluded = c(features.excluded, rm.prop)
    }
    flog.debug(str(properties.tmp[, 1:5]))

    #######################################################################
    ######### Scale the variables that are orders of magnitude larger. ####
    #######################################################################
    if(log.transform == T) {
        properties.tmp = properties.tmp %>% transformFeaturesForLinearRegression(features.logtransformed)
    } else {
        flog.debug("*** NO LOG TRANSFORMATIONS SELECTED..")
    }

    flog.debug(str(properties.tmp[, 1:5]))
    #######################################################################
    #################### NOW WE DO THE FEATURE PRUNING ###################
    #######################################################################

    pruneFeatures = function(X, missing.feature.cutoff.frac) {
        # Choose features by excluding ones that have NA > cutoff
        features = X  %>%
            dplyr::summarize_all(dplyr::funs(sum(is.na(.))/n())) %>%
            tidyr::gather(key="feature", value="missing_pct") %>%
            dplyr::arrange(missing_pct) %>%
            dplyr::filter(missing_pct<missing.feature.cutoff.frac) %>%
            dplyr::pull(feature)

        assertthat::assert_that(length(features) > 0)
        return(features)
    }

    if(length(features.excluded) > 0) {
        # Don't remove the vtreat features. the treatment is not applied to the data set until the end
        properties.tmp %<>% dplyr::select(-dplyr::one_of(setdiff(features.excluded, "id_parcel")))
    }

    if(large.missing.features.prune==T) {
        features.pruned = properties.tmp %<>%
            dplyr::select(c(pruneFeatures(properties.tmp, missing.feature.cutoff.frac), "logerror"))
        features.net.pruned = set.diff(features.pruned, features.excluded)
        flog.info(paste("*** Pruned additional ", length(features.net.pruned), " features: "))
        flog.info(paste(sort(features.net.pruned), collapse="\n"))
    }

    ##################################################################################
    ######## JUST BEFORE REMOVING ROWS JOIN THE TREATMENT AND THE ORIGINAL DATA FRAME
    ##################################################################################
    if(do.vtreat == T) {
        assertthat::assert_that(nrow(trainVtreat) == nrow(transactions.tmp))
        transactions.tmp = cbind(trainVtreat %>% dplyr::select(-logerror), transactions.tmp)
    }

    #######################################################################
    #################### REMOVE OUTLIERS AND NAs ###########################
    #######################################################################
    transactions.tmp %<>% dplyr::inner_join(properties.tmp, by="id_parcel")

    if(remove.outliers==T) {
        transactions.tmp %<>%
            dplyr::filter(logerror <=outlier.range[2] & logerror >=outlier.range[1])
    }

    if(omit.nas == T) { # only for transactions
         # The properties are needed for prediction even if its NA
        transactions.tmp %<>% na.omit
    }
    assertthat::assert_that(nrow(properties.tmp) <= nrow(properties))
    flog.info(paste("Using ", length(colnames(transactions.tmp)) , " features in transactions:"))
    flog.info(paste(sort(colnames(transactions.tmp)), collapse="\n"))
    flog.debug("..")
    return(list(transactions.tmp, properties.tmp, testVtreatFn, tplan))
}



# returns a property data set with the date column set to whats needed in the final predictions.

propertiesDataSetPredictorsWithDateEncoding = function(
                                       X,
                                       fitObj, # Used to predict the
                                       recode_list.for.date, # If the dates are to be used in prediction this list contains the numerical value to date mappings used in X
                                       preProcessFn=function(x){x}, # Teh function to call after preparing X to pre process it before prediction but after adding the date column. Useful when working with vtreat
                                       mapping_dates = mapping_dates.default, # The dates in the prediction file column
                                       second_round_mapping_dates = second_round_mapping_dates.default, # Dates for the private LB released at the end of the first round
                                       dates.are.numeric=F,
                                       dates.matter=T # Switch to control if predictions are done
                                       ) {
    # This is a helper function that creates a date column in the properties data set for
    # prediction. The value of this date column are can be anything depending on the date_coded column of the recode_list.for.date. A function is retured to
    # that does the actual prediction. This method just sets up the mapping. The recode_list.for.date contains
    # mapping of date to whatever value needs to be substituted for it.
    # three data sets are created for the three prediction columns
    mapping_dates.df = utils::stack(mapping_dates) # reshape all of the list's name-value pair to a 2 column data frame
    colnames(mapping_dates.df) = c("prediction.col.2016", "date")
    second_round_mapping.df = utils::stack(second_round_mapping_dates)
    colnames(second_round_mapping.df) = c("prediction.col.2017", "date")
    mapping_full = dplyr::inner_join(mapping_dates.df, second_round_mapping.df, by="date") %>%
                    dplyr::inner_join(recode_list.for.date, by="date")
    print(mapping_full)
    print(mapping_dates.df)
    assertthat::assert_that(nrow(mapping_full) == nrow(mapping_dates.df))
    pred.df = data.frame(row.names=1:nrow(X))

    apply(mapping_full, 1,
          function(mapping_i, fitObj) {
               date.code = mapping_i[["date_coded"]]
               date.char = mapping_i[["date"]]
               prediction.col.2016 = mapping_i[["prediction.col.2016"]]
               prediction.col.2017 = mapping_i[["prediction.col.2017"]]
               if(dates.matter == T) {
                   if(dates.are.numeric== T) {
                       X = X %>% dplyr::mutate(date = as.integer(date.code))
                   } else {
                       X = X %>% dplyr::mutate(date = as.Date(date.char))
                   }
               }
               X = cbind(X, preProcessFn(X))
               print(class(X))
               print(paste("Predicting for date: ", date.char))
               print(".P")
               if(dates.matter == T) {
                   predictions = foreach::foreach(
                              d=itertools::isplitRows(X, chunks=6),
                              .combine=c,
                              .packages=c("stats", "caret")) %dopar% {
                       stats::predict(fitObj, newdata=d)
                   }
               } else {
                   predictions = pred.df[, ncol(pred.df)] # previous column
               }
               print("..P")
               rm(X)
               pred.df %<>% dplyr::mutate(!!prediction.col.2016 := predictions)
               x_bar = dplyr::coalesce(
                           predictions, mean(predictions, na.rm=T))
               pred.df %<>% dplyr::mutate(!!prediction.col.2016 := x_bar)
               pred.df %<>% dplyr::mutate(!!prediction.col.2017 := x_bar)
               assign("pred.df", pred.df, envir=parent.frame(2))
               rm(predictions)
               gc()
      }, fitObj)
    return(pred.df)
}

recodeHelper = function(X, colname) {
    X %<>% dplyr::distinct_(.dots=colname)
    X %<>% dplyr::filter_at(dplyr::vars(colname), dplyr::all_vars(trimws(.) !="")) # Don't include any missing values
    right.coded.col.name = paste(colname, "_coded", sep="")
    colnames(X) = c(colname)
    X[[right.coded.col.name]] = 1:nrow(X)
    return(X)
}

recodeCharacterColumns =function(
        properties, features=c(
                   "zoning_landuse_county", "zoning_property",
                   "flag_tub", "flag_fireplace", "tax_delinquency")) {
    # TODO: Consider not rolling up blank strings into their own value.
    # Tested this with some dummy examples:
    ### a.test = data.frame(b = c(23, NA, NA, " ", 14, 16, 23))
    ### a =  data.frame(b = c(NA, "", "  ", "    ", 12, 23, 12))
    ### check recodeCharacterColumns(a, a.test, features=c("b"))  gives the right
    ### answer
    # Deals with character values in relavent columns of the data set.
    # Assigns NA to the missing values or empty strings.
    recode_list = list()
    for(feature in features) {
       stopifnot(feature %in% colnames(properties))
       if(is.numeric(properties %>% dplyr::pull(feature) %>% na.omit)) {
           print(paste(feature, " is already numeric, skipping"));
           next
       }
       print(paste("Recoding feature to numeric:", feature))
       # Create a data frame with numeric values per factor
       f1 = properties %>%
               dplyr::select(dplyr::matches(feature)) %>%
               dplyr::distinct_(.dots=feature) %>%
               recodeHelper(feature)
       # Now join and get rid of the old column
       # NA's will be left in place of empty strings
       recode_list[[feature]] = f1
       right.coded.col.name = paste(feature, "_coded", sep="")
       properties.t  = properties %>% dplyr::left_join(f1, by=feature) %>%
            dplyr::select(-dplyr::one_of(feature)) %>%
            dplyr::rename_(.dots=setNames(right.coded.col.name, feature))

       assertthat::assert_that(all(dim(properties) == dim(properties.t)))
       assertthat::assert_that(base::setequal(colnames(properties), colnames(properties.t)))
       properties = properties.t
       rm(properties.t)
    }
    return(list("properties"=properties, "recode_list" = recode_list))
}

convertToFactors = function(transactionPropertyData, features=c(
                                "region_city", "region_neighbor", "region_zip", "region_county",
                                "zoning_landuse", "zoning_landuse_county", "zoning_property",
                                "tract_number", "tract_block", "build_year")) {
    transactionPropertyData %<>% dplyr::mutate_at(dplyr::vars(features), funs(factor(as.numeric(.))))
}


#### Routines specific to Caret, and vtreat for data prep ##############

splitTrainingData = function(Y, split_percent=0.75) {
    set.seed(998)
    inTraining <- caret::createDataPartition(Y, p = split_percent, list = FALSE)
    return(inTraining)
}

splitLast3MonthsCrossFold = function(XY, k, dates.select) {
    rowsAfterDatebreak = XY %>%
        dplyr::mutate(row.name = as.integer(1:nrow(XY))) %>% dplyr::filter(date %in% dates.select) %>% dplyr::pull(row.name)

    rowsBeforeDatebreak = XY %>%
        dplyr::mutate(row.name = as.integer(1:nrow(XY))) %>% dplyr::filter(!date %in% dates.select) %>% dplyr::pull(row.name)
    lapply(1:k, function(k_) {
               testRows = sample(rowsAfterDatebreak, as.integer(length(rowsAfterDatebreak)/k))
               testRows.2 = sample(rowsBeforeDatebreak, as.integer(length(rowsBeforeDatebreak)/k))
               list(train=c(setdiff(rowsBeforeDatebreak, testRows.2), setdiff(rowsAfterDatebreak, testRows)),
                    app=c(testRows, testRows.2))
           })
}

splitKWayStratifiedCrossFoldHelper = function(Y, k)  {
    pStrat <- vtreat::kWayStratifiedY(length(Y), k, NULL, Y)
    # check if the split is a good partition
    check = vtreat::problemAppPlan(length(Y),k, pStrat,TRUE)
    if(is.null(check)) {
      print("Plan is good")
    } else {
      print(paste0("Problem with plan: ", check))
      stopifnot(TRUE)
    }
    return(pStrat)
}

splitKWayStratifiedCrossFold = function(Y, split_percent) {
    assertthat::assert_that(split_percent < 1 & split_percent > 0)
    k = as.integer(1/(1-split_percent))
    pStat = splitKWayStratifiedCrossFoldHelper(Y, k)
    print(paste("Creating a data set with ", k , " way cross validation folds"))
    return(pStrat[[1]]$train)
}

splitTrainingWrapper = function(XY,
                                split_percent=0.85,
                                splitFn=splitTrainingData,
                                YName="logerror") {

    trainingIndices <- caret::createDataPartition(XY %>% dplyr::pull(YName), p = split_percent, list = FALSE)

    XYTrain = XY[trainingIndices, ]
    XYTest = XY[-trainingIndices, ]

    XTrain = XYTrain %>% dplyr::select(-dplyr::contains(YName))
    XTest = XYTest %>% dplyr::select(-dplyr::contains(YName))
    YTrain = XYTrain %>% dplyr::pull(YName)
    YTest = XYTest %>% dplyr::pull(YName)
    flog.debug(nrow(XTrain))
    flog.debug(nrow(XTest))
    flog.debug(length(YTest))
    assertthat::assert_that(abs(nrow(XTrain)/nrow(XY) - split_percent) <= 1e-3)
    assertthat::assert_that(length(YTrain) == nrow(XTrain))
    assertthat::assert_that(length(YTest) == nrow(XTest))
    assertthat::assert_that(base::setequal(colnames(XTest), colnames(XTrain)) == T)
    return(list(XTrain, YTrain, XTest, YTest))
}
# TODO: Create a splitting routine that gets splits in different ways,
# like city, zip and see if there is some improvements
# May have some redundant stuff to the regression-vtreat branch code.
# TODO: Refactor this to reuse some of that code.
trainingVtreatWrapper = function(
           XY,
           features.restricted,
           features.treated,
           YName,
           keepDateCol=T,
           splitFn=splitKWayCrossFold,
           # Params for training to caret
           tuneGrid=data.frame(intercept=T),
           holdout.metric="RMSE",
           summaryFn=rmseSummary,
           gridSearchFn=lmByGridSearch,
           # Params for vtreat
           vtreat.grid=data.frame(smFactor=0.01, rareCount=10, rareSig=0.01, pruneSig=0.01),
           makeLocalCluster=F,
           crossFrameCluster=NULL,
           parallelTraining=parallelTraining) {
    # Create Training and hold out data sets.
    # Treat the hold out data sets from treatments on the training only
    # Run LM, return the list of best fit model on the holdout data set.
    # Return the best model fit and predictions on the holdout data
    # NOTE: All transformation and scaling is done outside of this method
    assertthat::assert_that(all(!is.null(tuneGrid) & !is.na(tuneGrid)))
    assertthat::assert_that(!is.null(holdout.metric) & !is.na(holdout.metric))
    assertthat::assert_that(is.function(summaryFn))
    assertthat::assert_that(is.function(gridSearchFn))
    assertthat::assert_that(all(!is.null(vtreat.grid) & !is.na(vtreat.grid)))
    assertthat::assert_that(all(features.restricted %in% colnames(XY)))
    assertthat::assert_that(all(features.treated %in% colnames(XY)))
    #print(paste("Vtreat options:", paste(names(options.vtreat), options.vtreat, collapse=",", sep=":")))
    XY = XY  %>%
        dplyr::select_at(dplyr::vars(c(features.restricted, YName)))
        # transform the features for LR select only the subset that remain and remove ones that were impact coded.

    assertthat::assert_that(nrow(XY) > 0 )
    set.seed(123456)
    # Trying a more balanced cross folding startegy to learn from
    list[XTrain, YTrain, XTest, YTest] = splitTrainingWrapper(XY, splitFn=splitFn, YName=YName)
    set.seed(123456)
    XYTrain = cbind(XTrain, YTrain)
    names(XYTrain)[ncol(XYTrain)] = YName
    prepFrames = createCrossFrameTreatment(
                 XYTrain,
                 features=features.treated,
                 vtreat.grid,
                 YName=YName,
                 makeLocalCluster=makeLocalCluster,
                 crossFrameCluster=crossFrameCluster) # damn this for now?
    print(".")
    snow::parLapply(crossFrameCluster, prepFrames, function(prep, applyCrossFrameToX, gridSearchFn, parallelTraining) {
        #set.seed(123456)

        options.vtreat = prep$opts.vtreat
        XTrainTreated = applyCrossFrameToX(XTrain, prep, pruneSig=options.vtreat$pruneSig, isTrain=T, yName=YName)
        #set.seed(123456)
        XTestTreated = applyCrossFrameToX(XTest, prep, pruneSig=NULL, isTrain=F, keepDateTimeFeature=keepDateCol, yName=YName)
        print(nrow(XTrainTreated))
        if(nrow(XTrainTreated) == 0 | ncol(XTrainTreated) == 0) {
            print(paste("*** WARN: there are no significant cols in training data, returning"))
            print(prep$treatments$scoreFrame)
            return(NULL)
        }
        if(!base::setequal(colnames(XTestTreated), colnames(XTrainTreated))) {
            print("XTest and XTrain cols are not the same:")
            print(colnames(XTestTreated))
            print(colnames(XTrainTreated))
            print("Aborting")
            stopifnot(TRUE)
        }
        print("...")
        assertthat::assert_that(setequal(colnames(XTestTreated), colnames(XTrainTreated)))
        bestFit = gridSearchFn(
            train = XTrainTreated, target = YTrain, ncores = 6, tuneGrid = tuneGrid,
            metric = holdout.metric , summaryFunction = summaryFn, allowParallel=parallelTraining)
        pred = predict(bestFit, XTestTreated)
        print("....")
        bestFit$holdoutPred = pred
        bestFit$holdout.rmse = sqrt(mean((YTest - pred)^2))
        bestFit$crossFrame = prep
        return(bestFit)
        print(".....")
    }, applyCrossFrameToX, gridSearchFn, parallelTraining)
}

charColumnEncode = function(dataset) {
    # Convert all the catagorical columns to integer with lookup
    char_cols_logical = sapply(dataset, FUN=function(x) {is.character(x)})
    char_cols = names(char_cols_logical[char_cols_logical == T])
    dictionary = data.frame()
    for( col_name in char_cols) {
        u = unique(dataset[, col_name])
        id = 1:length(u)
        look = data.frame(id=id, val=u, var_name=col_name)
        dataset[[col_name]] = look$id[match(dataset[[col_name]], look$val)]
        dictionary = rbind(dictionary, look)
    }
    return(list(dataset, dictionary))
}

############# Spatial EDA routines *****************
# The residuals are plotted per city in orange county to compare
countyOverUnderOC = function(sample_properties, reg_residuals, map.colors, shp.oc.city) {
    doc = "
    Arguments
        sample_properties: data frame for property data with columns: city, longitude and latitude columns
        reg_residuals: a vector of the same #rows as sample_properties containing residuals
        map.colors: a named vector with the names same as city
    "
    assert_that(length(errors) == nrow(x)) # the errors correspond to the # data points
}

propertiesToSpatial =  function(sample_properties, proj4String) {
    doc = "
        sample_properties: data frame for property data with columns: city, longitude and latitude columns
    "
    sp::coordinates(sample_properties) <- ~ longitude + latitude
    sp::proj4string(sample_properties) = proj4string

    return(sample_properties)
}

######### GIS data munging related, like getting shape files, transforming them etc #############

# For shape files that contain zip codes but not city names
joinZipToName = function(fortifiedShapeDf, zipcode_key_name) {
    fortifiedShapeDf %<>%
        dplyr::left_join(zipcode %>%
                            dplyr::rename(longitude.city=longitude, latitude.city=latitude) %>%
                            dplyr::select(zip, city, state, longitude.city, latitude.city),
                         by = c("ZIPCODE" = "zip"))
    return(fortifiedShapeDf)
}

getShapeFiles = function(name, layer) {
    t = rgdal::readOGR(dsn =  name, layer = layer, verbose=FALSE)
    print(sp::proj4string(t))
    return(t)
}

# Given raw shape files read by readOGR, transform them using the transform_crs
# We may want to transform these coordinates so that they can all be the same
transformShapeFiles = function (shp, transform_crs="+proj=longlat +datum=NAD83") {
    return(sp::spTransform(shp, sp::CRS(transform_crs)))
}

computeVariogramFromTransactions = function(transactions) {
    print(transactions)
    transactions %<>%
        dplyr::select(longitude, latitude, abs_logerror) %>% na.omit
    print(dim(transactions))
    sp::coordinates(transactions) <- ~ longitude + latitude
    sp::proj4string(transactions) = sp::proj4string(shp.oc.city)

    return(gstat::variogram(abs_logerror ~ 1, transactions))
}

getShapeDfCentroids = function(shpDf) {
    sp::coordinates(shpDf)
}

getShapeDf = function(name, layer, transform_latlon=FALSE, transform_crs="+proj=longlat +datum=NAD83"){
    shp = getShapeFiles(name, layer)

    if(transform_latlon == TRUE ) { shp = transformShapeFiles(shp, transform_crs) }

    shp@data$id = rownames(shp@data)
    shp.points = ggplot2::fortify(shp, region="id")
    #print(paste(dim(shp.points)))
    shp.df = dplyr::inner_join(shp.points, shp@data, by="id")
    return(shp.df)
}

getDfFromShape = function(shp) {
    shp@data$id = rownames(shp@data)
    shp.points = ggplot2::fortify(shp, region="id")
    #print(paste(dim(shp.points)))
    shp.df = dplyr::inner_join(shp.points, shp@data, by="id")
    return(shp.df)
}

