library(magrittr)
library(lazyeval)
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")


#properties with high 75th percentile of the abs_error.
oc_high_75 = c( 'LAGUNA BEACH' , 'NEWPORT BEACH', 'LAGUNA WOODS','SAN CLEMENTE', 'DANA POINT', "SAN JUAN CAPISTRANO","SANTA ANA",  "SEAL BEACH")

readAndTransformData = function(tr='inputs/properties_2016.csv', pr = 'inputs/train_2016_v2.csv') {

    properties <- data.table::fread(tr, strip.white=T, na.strings=c(NA, ''), stringsAsFactors=F)
    transactions <- data.table::fread(pr, strip.white=T, na.strings=c(NA, ''), stringsAsFactors=F)
    #sample_submission <- data.table::fread('inputs/sample_submission.csv')
    properties <- properties %>% dplyr::rename(
      id_parcel = parcelid,
      build_year = yearbuilt,
      area_basement = basementsqft,
      area_patio = yardbuildingsqft17,
      area_shed = yardbuildingsqft26,
      area_pool = poolsizesum,
      area_lot = lotsizesquarefeet,
      area_garage = garagetotalsqft,
      area_firstfloor_finished = finishedfloor1squarefeet,
      area_total_calc = calculatedfinishedsquarefeet,
      area_base = finishedsquarefeet6,
      area_live_finished = finishedsquarefeet12,
      area_liveperi_finished = finishedsquarefeet13,
      area_total_finished = finishedsquarefeet15,
      area_unknown = finishedsquarefeet50,
      num_unit = unitcnt,
      num_story = numberofstories,
      num_room = roomcnt,
      num_bathroom = bathroomcnt,
      num_bedroom = bedroomcnt,
      num_bathroom_calc = calculatedbathnbr,
      num_bath = fullbathcnt,
      num_75_bath = threequarterbathnbr,
      num_fireplace = fireplacecnt,
      num_pool = poolcnt,
      num_garage = garagecarcnt,
      region_county = regionidcounty,
      region_city = regionidcity,
      region_zip = regionidzip,
      region_neighbor = regionidneighborhood,
      tax_total = taxvaluedollarcnt,
      tax_building = structuretaxvaluedollarcnt,
      tax_land = landtaxvaluedollarcnt,
      tax_property = taxamount,
      tax_year = assessmentyear,
      tax_delinquency = taxdelinquencyflag,
      tax_delinquency_year = taxdelinquencyyear,
      zoning_property = propertyzoningdesc,
      zoning_landuse = propertylandusetypeid,
      zoning_landuse_county = propertycountylandusecode,
      flag_fireplace = fireplaceflag,
      flag_tub = hashottuborspa,
      flag_tub_extra = pooltypeid10,
      quality = buildingqualitytypeid,
      framing = buildingclasstypeid,
      material = typeconstructiontypeid,
      deck = decktypeid,
      story = storytypeid,
      heating = heatingorsystemtypeid,
      aircon = airconditioningtypeid,
      architectural_style= architecturalstyletypeid
    )

    transactions <- transactions %>% dplyr::rename(
      id_parcel = parcelid,
      date = transactiondate
    )
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
                       remove.outliers=T,
                       omit.nas=F,
                       features.excluded,
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

    if(length(features.excluded) >0) {
        transactions_cleaned %<>% dplyr::select(-dplyr::one_of(features.excluded))
        # properties_cleaned %<>% dplyr::select(-dplyr::one_of(features.excluded))
    }

    if(length(features.categorical) >0) {
        transactions_cleaned %<>% dplyr::mutate_at(dplyr::vars(features.categorical), as.factor)
        properties_cleaned %<>% dplyr::mutate_at(dplyr::vars(setdiff(features.categorical, "date")), as.factor)
    }

    pruneFeatures = function(X, cutoff=0.12) {
        # Choose features by excluding ones that have NA > cutoff
        features = X  %>%
            summarize_all(funs(sum(is.na(.))/n())) %>%
            tidyr::gather(key="feature", value="missing_pct") %>% arrange(missing_pct) %>%
            filter(missing_pct<cutoff) %>% pull(feature)

        assertthat::assert_that(length(features) > 0)
        print(paste("Using ", length(features), " features"))
        return(features)
    }

    if(large.missing.features.prune==T) {
        transactions_cleaned %<>%
            select(c(pruneFeatures(transactions_cleaned), "logerror"))
        properties_cleaned %<>%
            select(setdiff(c(colnames(transactions_cleaned), "id_parcel"), c("logerror", "date")))
    }

    print(colnames(properties_cleaned))


    #Scale the variables that are orders of magnitude larger.
    if(log.transform == T) {
        transactions_cleaned = transactions_cleaned  %>% transformFeaturesForLinearRegression()
        print("")
        properties_cleaned = properties_cleaned %>% transformFeaturesForLinearRegression()
    }

    assertthat::assert_that(nrow(transactions_cleaned) == nrow(transactions))

    if(remove.outliers==T) {
        transactions_cleaned = transactions_cleaned  %>%
            dplyr::filter(logerror <=0.4 & logerror >=-0.4)
    }
    assertthat::assert_that(nrow(properties_cleaned) == nrow(properties))
    if(omit.nas == T) {
        transactions_cleaned %<>% na.omit
        # properties_cleaned %<>% na.omit # The properties are needed
    }
    print("..")
    return(list(transactions_cleaned, properties_cleaned, recode_list))
}

recodeHelper = function(X, colname) {
    X %<>% dplyr::distinct_(.dots=colname)
    X %<>% dplyr::filter_at(dplyr::vars(colname), dplyr::all_vars(trimws(.) !="")) # Don't include any missing values
    right.coded.col.name = paste(colname, "_coded", sep="")
    colnames(X) = c(colname)
    X[[right.coded.col.name]] = 1:nrow(X)
    return(X)
}

# Creates a function that returns a property data set with the date column set to whats needed in the predictions
mapping_dates.default = list("2016-10-01"= "201610", "2016-11-01" ="201611", "2016-12-01"="201612")
second_round_mapping_dates.default = list("2016-10-01"= "201710", "2016-11-01" ="201711", "2016-12-01"="201712")

propertiesDataSetPredictorsWithDateEncoding = function(
                                       X, fitObj,
                                       recode_list.for.date,
                                       mapping_dates = mapping_dates.default,
                                       second_round_mapping_dates = second_round_mapping_dates.default,
                                       dates.are.numeric=F,
                                       dates.matter=T) {
    # This is a helper function that creates a date column in the properties data set for
    # prediction. The value of this date column are can be anything depending on the date_coded column of the recode_list.for.date. A function is retured to
    # that does the actual prediction. This method just sets up the mapping. The recode_list.for.date contains
    # mapping of date to whatever value needs to be substituted for it.
    # three data sets are created for the three prediction columns
    mapping_dates.df = stack(mapping_dates)
    colnames(mapping_dates.df) = c("prediction.col.2016", "date")
    second_round_mapping.df = stack(second_round_mapping_dates)
    colnames(second_round_mapping.df) = c("prediction.col.2017", "date")
    mapping_full = dplyr::inner_join(mapping_dates.df, second_round_mapping.df, by="date") %>%
                    dplyr::inner_join(recode_list.for.date, by="date")
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
               print(paste("Predicting for date: ", date.char))
               predictions = foreach::foreach(d=itertools::isplitRows(X, chunks=6),
                                    .combine=c, .packages=c("stats", "caret")) %dopar% {
                   stats::predict(fitObj, newdata=d)
               }

               pred.df %<>% dplyr::mutate(!!prediction.col.2016 := predictions)
               x_bar = dplyr::coalesce(
                           pred.df %>% dplyr::pull(prediction.col.2016),
                           mean(pred.df %>% dplyr::pull(prediction.col.2016), na.rm=T))
               pred.df %<>% dplyr::mutate(!!prediction.col.2016 := x_bar)
               pred.df %<>% dplyr::mutate(!!prediction.col.2017 := x_bar)
               assign("pred.df", pred.df, envir=parent.frame(2))
      }, fitObj)
    return(pred.df)
}

recodeCharacterColumns =function(
        transactions, properties, features=c(
                   "zoning_landuse_county", "zoning_property",
                   "flag_tub", "flag_fireplace", "tax_delinquency", "date")) {
    # Deals with character values in relavent columns of the data set.
    # Assigns NA to the missing values or empty strings.
    recode_list = list()
    for(feature in features) {
        if((feature == "date" || is.numeric(properties %>% dplyr::pull(feature) %>% na.omit)) &&
           is.numeric(transactions %>% dplyr::pull(feature) %>% na.omit) ){
            print(paste(feature, " is already numeric, skipping"));
            f1 = data.frame(rbind(
                    properties %>% dplyr::select(dplyr::matches(feature)) %>% dplyr::distinct_(.dots=feature),
                    transactions %>% dplyr::select(dplyr::matches(feature)) %>% dplyr::distinct_(.dots=feature))) %>%
                    recodeHelper(feature) #distinct_(.dots=feature)
            recode_list[[feature]] = f1
            next
        }
        print(paste("Recoding feature to numeric:", feature))
        # Create a data frame with numeric values per factor
        f1 = data.frame(rbind(
                properties %>% dplyr::select(dplyr::matches(feature)) %>% dplyr::distinct_(.dots=feature),
                transactions %>% dplyr::select(dplyr::matches(feature)) %>% dplyr::distinct_(.dots=feature))) %>%
                recodeHelper(feature) #distinct_(.dots=feature)
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

transformCoords = function(transactionPropertyData) {
    transactionPropertyData %>%
	dplyr::mutate(latitude = latitude/1e6, longitude=longitude/1e6)
}

transformErrors = function(transactionPropertyData) {
    transactionPropertyData %>%
	dplyr::mutate(abs_logerror = abs(logerror)) %>%
	dplyr::mutate(overunder = ifelse(logerror<0,"under","over"))
}

transformCoordErrors = function (transactionPropertyData) {
    sample_props = transactionPropertyData %>%
        transformCoords() %>%
        transformErrors()
    return(sample_props)
}

cutTransactionsPerCounty = function(transactions) {
    sample_props.1286 = transactions %>% dplyr::filter(region_county==1286) %>% dplyr::mutate(region_county_name = "orange county")
    sample_props.2061 = transactions %>% dplyr::filter(region_county== 2061) %>% dplyr::mutate(region_county_name = "ventura county")
    sample_props.3101 = transactions %>% dplyr::filter(region_county==3101) %>% dplyr::mutate(region_county_name = "la county")

    return(list(sample_props.1286, sample_props.3101, sample_props.2061))
}

groupBySpatialFeature = function(df, f.spatial, var_name){
    #mutate_call = interp(~!is.na(a), a=as.name(var_name));
    var_name.enquo = enquo(var_name)
    f.spatial.enquo = enquo(f.spatial)
    var_name_missing = paste(quo_name(var_name.enquo), "_missing", sep="")
    var_name_present = paste(quo_name(var_name.enquo), "_present", sep="")
    # You can follow one of two patterns with regards to using dplyr in functions:
    # 1. USE NSE for function args in calls and use enquo and !! patterns for use with dplyr verbs.
    # 2. USE non-NSE semantics, i.e. use strings as function arguments and using !!as.name(.) pattern in dplyr verbs.
    # Note, that if you need to make new variable names from strings
    # then u just needs to make sure to use !! for LHS of expression and
    # !!as.name(<string>) on the RHS eg: summarize(ct = sum(!!as.name(var_name_present), na.rm=TRUE)) .
    # (1) seems an easier choice, but sometimes new names are needed for which strings need to be created.
    # dplyr vars() used  in the *_all, *_at, *_each functions can mix strings and names making programming easier.
    b = df %>%
        # Below call is more understandable instead of: mutate_(.dots=setNames(list(mutate_call), var_name_present)) %>%
        dplyr::mutate(!!var_name_present := is.na(!!var_name.enquo)) %>%
        dplyr::select_at(dplyr::vars(!!f.spatial.enquo, var_name_present)) %>%
        dplyr::group_by_at(dplyr::vars(!!f.spatial.enquo)) %>%
        # One can also do: summarize(ct = sum(!!as.name(var_name_present), na.rm=TRUE)) %>% head(50)
        summarize_at(var_name_present , sum, na.rm=TRUE)

    a = df %>%
        dplyr::mutate(!!var_name_missing := !is.na(!!var_name.enquo)) %>%
        dplyr::select_at(dplyr::vars(!!f.spatial.enquo, var_name_missing)) %>%
        dplyr::group_by_at(dplyr::vars(!!f.spatial.enquo))  %>%
        dplyr::summarize_at(var_name_missing, sum, na.rm=TRUE)
    return(list(missing=a, present=b))
}

countyGrouping = function(df, var_name){

    groupBySpatialFeature(df, region_county, var_name)
}

#### Routines specific to Caret, and vtreat for data prep ##############

splitTrainingData = function(Y, split_percent=0.75) {
    set.seed(998)
    inTraining <- createDataPartition(Y, p = split_percent, list = FALSE)
    return(inTraining)
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
        library(magrittr)

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
library(zipcode)
data(zipcode)

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

