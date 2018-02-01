library(magrittr)
library(lazyeval)
source("datadefs.R")
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

#properties with high 75th percentile of the abs_error.

readAndTransformData = function(pr='inputs/properties_2016.csv', tr = 'inputs/train_2016_v2.csv') {

    properties <- readPropertiesFile(pr)
    transactions <- readPropertiesFile(tr)
    #sample_submission <- data.table::fread('inputs/sample_submission.csv')
    properties %<>% makePropertiesFeaturesReadable()
    transations %<>% makeTransactionFeaturesReadable()
    # Split the census raw data into meaningful categories
    properties %<>%
        dplyr::mutate(census = as.character(rawcensustractandblock),
               tract_number = as.numeric(stringr::str_sub(census,5,11)),
               tract_block = as.numeric(stringr::str_sub(census,12)))
    return(list(transactions %>% dplyr::left_join(properties, by="id_parcel"), properties))
}


prepareData = function(
                       recode_chars=T,
                       log.transform=T,
                       large.missing.features.prune=T,
                       missing.feature.cutoff.frac = 0.10,
                       remove.outliers=T,
                       outlier.range=c(-0.4, 0.4),
                       omit.nas=F,
                       do.vtreat=F,
                       engineer.features=F,
                       avg.bys=c("region_city","region_county", "region_zip"),
                       vtreat.opts,
                       features.excluded,
                       features.logtransformed,
                       features.vtreat.treated, # WITH vtreat
                       do.factor.conversion,
                       features.categorical
                       ) {

    # Renames columns to be more reasonable,
    # puts lats and logs in correct format
    # converts all chars in props and trans to integers. And converts the feature.categorical to factors
    # Optionally log transforms the large area_ and tax_ features
    list[transactions, properties] = readAndTransformData()
    transactions %<>%transformCoords()
    properties %<>% transformCoords()
    print(".")

    # Recode all char data into numerics converting their missing vals(blanks, empty strings into )
    if(recode_chars == T) {
        list[transactions.enc, properties.enc, recode_list] = recodeCharacterColumns(transactions, properties)
    } else {
        recode_list = NULL
    }
    # Impute some common sense variables
    transactions_cleaned = transactions.enc %>% round1Impute()
    properties_cleaned = properties.enc %>% round1Impute()

    if(do.factor.conversion == T && length(features.categorical) > 0) { # May often be excluded from lm due to data set size.
        transactions_cleaned %<>% dplyr::mutate_at(dplyr::vars(features.categorical), as.factor)
        properties_cleaned %<>% dplyr::mutate_at(dplyr::vars(setdiff(features.categorical, "date")), as.factor)
    } else {
        print("** Not converting any features to factors. do.factor.conversion is set to false or no
              categorical features specified")
    }
    #########################################################################
    ############### GET THE TREATMENTS BEFORE YOU DO ANYTHING ELSE ##########
    #########################################################################
    if(do.vtreat==T) {
        if(log.transform == T) {
            shouldnotbehappening = intersect(features.logtransformed, features.vtreat.treated)
            if(length(shouldnotbehappening) > 0) {
                print(paste("features that are treated cannot be log transformed",
                            paste(shouldnotbehappening, sep=", ")))
            }
        }

        list[tplan, trainVtreat, testVtreatFn] = prepareDataFeaturesWithVtreat(transactions_cleaned, features.vtreat.treated, vtreat.opts)
        print("..")

        train.features.excluded = union(features.excluded, features.vtreat.treated)
        test.features.excluded = features.excluded
        # add all the features that are treated already to the exclusion thingy
        print(paste("***", length(features.vtreat.treated), "features are additionally excluded from preprocessing due to vtreat treatment: ",
                    paste(features.vtreat.treated, collapse=", ")))
    } else{
        testVtreatFn = function(x){x}
        train.features.excluded = features.excluded
        test.features.excluded = features.excluded
        tplan=NULL
    }
    #######################################################################
    ######### Feature engineering before log xfrm, but after vtreat    ####
    #######################################################################
    if(engineer.features == T) {
        list[transactions_cleaned, features.engineered, rm.prop] = engineerFeatures(
                                transactions_cleaned, avg.bys=avg.bys)
        list[properties_cleaned, XXX__, YYY__] = engineerFeatures(
                              properties_cleaned, transactions_cleaned, avg.bys=avg.bys)
        print(paste("Added", length(features.engineered), " engineered features:"))
        print(paste(features.engineered, collapse=", "))
        test.features.excluded = c(test.features.excluded, rm.prop)
        train.features.excluded = c(train.features.excluded, rm.prop)
    }

    #######################################################################
    ######### Scale the variables that are orders of magnitude larger. ####
    #######################################################################
    if(log.transform == T) {
        transactions_cleaned = transactions_cleaned  %>% transformFeaturesForLinearRegression(features.logtransformed)
        print("")
        properties_cleaned = properties_cleaned %>% transformFeaturesForLinearRegression(features.logtransformed)
    } else {
        print(" NO LOG TRANSFORMATIONS SELECTED..")
    }

    #######################################################################
    #################### NOW WE DO ALL THE FEATURE PRUNING ###################
    #######################################################################
    if(length(train.features.excluded) >0) {
        transactions_dates = transactions_cleaned %>% dplyr::select(date)
        transactions_cleaned %<>% dplyr::select(-dplyr::one_of(train.features.excluded))
    }

    if(length(test.features.excluded) > 0) {
        # Don't remove the vtreat features. the treatment is not applied to the data set until the end
        properties_cleaned %<>% dplyr::select(-dplyr::one_of(setdiff(test.features.excluded, "id_parcel")))
    }


    pruneFeatures = function(X, missing.feature.cutoff.frac=0.12) {
        # Choose features by excluding ones that have NA > cutoff
        features = X  %>%
            dplyr::summarize_all(dplyr::funs(sum(is.na(.))/n())) %>%
            tidyr::gather(key="feature", value="missing_pct") %>% dplyr::arrange(missing_pct) %>%
            dplyr::filter(missing_pct<missing.feature.cutoff.frac) %>% dplyr::pull(feature)

        assertthat::assert_that(length(features) > 0)
        return(features)
    }

    if(large.missing.features.prune==T) {
        transactions_cleaned %<>%
            dplyr::select(c(pruneFeatures(transactions_cleaned, missing.feature.cutoff.frac), "logerror"))
        # features should not be excluded from properties #
    }

    assertthat::assert_that(nrow(transactions_cleaned) == nrow(transactions))
    ##################################################################################
    ######## JUST BEFORE REMOVING ROWS JOIN THE TREATMENT AND THE ORIGINAL DATA FRAME
    ##################################################################################
    if(do.vtreat == T) {
        assertthat::assert_that(nrow(trainVtreat) == nrow(transactions_cleaned))
        transactions_cleaned = cbind(trainVtreat %>% dplyr::select(-logerror), transactions_cleaned)
    }

    #######################################################################
    #################### REMOVE OUTLIERS AND NAs ###########################
    #######################################################################
    if(remove.outliers==T) {
        transactions_dates %<>% dplyr::filter(transactions_cleaned$logerror <=outlier.range[2] & 
                                                  transactions_cleaned$logerror >= outlier.range[1])
        transactions_cleaned = transactions_cleaned  %>%
            dplyr::filter(logerror <=outlier.range[2] & logerror >=outlier.range[1])
        assertthat::assert_that(nrow(transactions_dates) == nrow(transactions_cleaned))
    }

    if(omit.nas == T) {
        transactions_dates %<>% dplyr::filter(transactions_cleaned %>% complete.cases)
        transactions_cleaned %<>% na.omit
        # properties_cleaned %<>% na.omit # The properties are needed for prediction even if its NA
        assertthat::assert_that(nrow(transactions_dates) == nrow(transactions_cleaned))
    }
    assertthat::assert_that(nrow(properties_cleaned) == nrow(properties))
    assertthat::assert_that(nrow(transactions_dates) == nrow(transactions_cleaned))
    print(paste("Using ", length(colnames(transactions_cleaned)) , " features in transactions:"))
    print(colnames(transactions_cleaned))
    print("..")
    return(list(transactions_cleaned, transactions_dates, properties_cleaned, recode_list, testVtreatFn, tplan))
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
        transactions, properties, features=c(
                   "zoning_landuse_county", "zoning_property",
                   "flag_tub", "flag_fireplace", "tax_delinquency", "date")) {
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
        if((feature == "date" ||
                is.numeric(
                    properties %>% dplyr::pull(feature) %>% na.omit)
                ) &&
                is.numeric(transactions %>% dplyr::pull(feature) %>% na.omit)) {
            print(paste(feature, " is already numeric, skipping"));
            next
        }
        print(paste("Recoding feature to numeric:", feature))
        # Create a data frame with numeric values per factor
        f1 = data.frame(rbind(
                properties %>% dplyr::select(dplyr::matches(feature)) %>% dplyr::distinct_(.dots=feature),
                transactions %>% dplyr::select(dplyr::matches(feature)) %>% dplyr::distinct_(.dots=feature))) %>%
                recodeHelper(feature)
        # Now join and get rid of the old column
        # NA's will be left in place of empty strings
       recode_list[[feature]] = f1
       right.coded.col.name = paste(feature, "_coded", sep="")
       transactions.t = transactions %>% dplyr::left_join(f1, by=feature) %>%
            dplyr::select(-dplyr::one_of(feature)) %>%
            dplyr::rename_(.dots=setNames(right.coded.col.name, feature))
        if(feature != "date")
            properties.t  = properties %>% dplyr::left_join(f1, by=feature) %>%
                dplyr::select(-dplyr::one_of(feature)) %>%
                dplyr::rename_(.dots=setNames(right.coded.col.name, feature))

        assertthat::assert_that(all(dim(transactions) == dim(transactions.t)))
        if(feature != "date")
            assertthat::assert_that(all(dim(properties) == dim(properties.t)))
        assertthat::assert_that(base::setequal(colnames(transactions),colnames(transactions.t)))
        if(feature != "date")
            assertthat::assert_that(base::setequal(colnames(properties), colnames(properties.t)))
        if(feature != "date")
            properties = properties.t
        transactions = transactions.t
    }
    return(list("transactions"=transactions, "properties"=properties, "recode_list" = recode_list))
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
    inTraining <- createDataPartition(Y, p = split_percent, list = FALSE)
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

splitTrainingWrapper = function(XY, split_percent=0.85, splitFn=splitTrainingData,
                                YName="logerror") {
    trainingIndices = splitFn(Y = XY %>% dplyr::pull(YName), split_percent = split_percent)

    XYTrain = XY[trainingIndices, ] %>% na.omit
    XYTest = XY[-trainingIndices, ] %>% na.omit

    XTrain = XYTrain %>% dplyr::select(-dplyr::contains(YName))
    XTest = XYTest %>% dplyr::select(-dplyr::contains(YName))
    YTrain = XYTrain %>% dplyr::pull(YName)
    YTest = XYTest %>% dplyr::pull(YName)
    assertthat::assert_that(length(YTrain) == nrow(XTrain))
    assertthat::assert_that(length(YTest) == nrow(XTest))
    assertthat::assert_that(base::setequal(colnames(XTest), colnames(XTrain)) == T)
    assertthat::assert_that(nrow(XTrain) > 0)
    assertthat::assert_that(length(YTrain) > 0)
    assertthat::assert_that(nrow(XTest) > 0)
    assertthat::assert_that(length(YTest) > 0)
    return(list(XTrain, YTrain, XTest, YTest))
}

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

