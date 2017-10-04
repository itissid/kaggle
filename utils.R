library(magrittr)
library(lazyeval)
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
source("features.R") 


#properties with high 75th percentile of the abs_error.
oc_high_75 = c( 'LAGUNA BEACH' , 'NEWPORT BEACH', 'LAGUNA WOODS','SAN CLEMENTE', 'DANA POINT', "SAN JUAN CAPISTRANO","SANTA ANA",  "SEAL BEACH")

readAndTransformData = function() {

    properties <- data.table::fread('inputs/properties_2016.csv', strip.white=T, na.strings=c(NA, ''), stringsAsFactors=F)
    transactions <- data.table::fread('inputs/train_2016_v2.csv', strip.white=T, na.strings=c(NA, ''), stringsAsFactors=F)
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

recodeCharacterColumns =function(
        transactions, properties, features=c(
                   "zoning_landuse_county", "zoning_property",
                   "flag_tub", "flag_fireplace", "tax_delinquency")) {
    # Deals with character values in relavent columns of the data set.
    # Assigns NA to the missing values or empty strings.
    for(feature in features) {
        if(is.numeric(properties %>% dplyr::pull(feature) %>% na.omit) &
           is.numeric(properties %>% dplyr::pull(feature) %>% na.omit) ){
            print(paste(feature, " is already numeric, skipping"));
            next
        }
        print(paste("Recoding feature to numeric:", feature))
        f1 = data.frame(rbind(properties %>% dplyr::distinct_(.dots=feature), 
                transactions %>% dplyr::distinct_(.dots=feature))) %>% distinct_(.dots=feature)
        f1 %<>% dplyr::filter_at(vars(feature), all_vars(trimws(.) !="")) # Don't include any missing values
        right.col.name = paste(feature, "_tmp", sep="")
        right.coded.col.name = paste(feature, "_coded", sep="")
        colnames(f1) = c(right.col.name)
        f1[[right.coded.col.name]] = 1:nrow(f1)
        # Now join and get rid of the old column
        # NA's will be left in place of empty strings
        transactions.t  = transactions %>% dplyr::left_join(f1, by=setNames(right.col.name, feature))
        properties.t  = properties %>% dplyr::left_join(f1, by=setNames(right.col.name, feature)) 
      
        # Remove the character columns and rename the new coded columns 
        transactions.t = transactions.t[, -which(names(transactions) %in% c(feature, right.col.name))] %>%
            dplyr::rename_(.dots=setNames(right.coded.col.name, feature))
        properties.t = properties.t[, -which(names(properties) %in% c(feature, right.col.name))] %>%
            dplyr::rename_(.dots=setNames(right.coded.col.name, feature))
         
        assertthat::assert_that(all(dim(transactions) == dim(transactions.t)))
        assertthat::assert_that(all(dim(properties) == dim(properties.t)))
        assertthat::assert_that(base::setequal(colnames(transactions),colnames(transactions.t)))
        assertthat::assert_that(base::setequal(colnames(properties), colnames(properties.t)))
        properties = properties.t
        transactions = transactions.t
    }
    return(list("transactions"=transactions, "properties"=properties))
}

convertToFactors = function(transactionPropertyData, features=c(
                                "region_city", "region_neighbor", "region_zip", "region_county",
                                "zoning_landuse", "zoning_landuse_county", "zoning_property",
                                "tract_number", "tract_block", "build_year")) {
    transactionPropertyData %<>% mutate_at(vars(features), funs(factor(as.numeric(.)))) 
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
        dplyr::mutate(!!var_name_present := !is.na(!!var_name.enquo)) %>% 
        select_at(dplyr::vars(!!f.spatial.enquo, var_name_present)) %>%
        group_by_at(dplyr::vars(!!f.spatial.enquo)) %>% 
        # One can also do: summarize(ct = sum(!!as.name(var_name_present), na.rm=TRUE)) %>% head(50)
        summarize_at(var_name_present , sum, na.rm=TRUE)

    a = df %>% 
        dplyr::mutate(!!var_name_missing := !is.na(!!var_name.enquo)) %>% 
        select_at(dplyr::vars(!!f.spatial.enquo, var_name_missing)) %>%
        group_by_at(dplyr::vars(!!f.spatial.enquo))  %>%
        summarize_at(var_name_missing, sum, na.rm=TRUE)
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
splitKWayStratifiedCrossFold = function(Y, split_percent) {
    assertthat::assert_that(split_percent < 1 & split_percent > 0)
    k = as.integer(1/(1-split_percent))
    print(paste("Creating a data set with ", k , " way cross validation folds"))
    pStrat <- vtreat::kWayStratifiedY(nrow(Y),k, NULL, Y)
    # check if the split is a good partition
    check = vtreat::problemAppPlan(nrow(Y),k,pStrat,TRUE)
    if(is.null(check)) {
      print("Plan is good")
    } else {
      print(paste0("Problem with plan: ", check))
      stopifnot(TRUE)
    }
    return(pStrat[[1]]$train)
}

splitTrainingWrapper = function(XY, split_percent=0.85, splitFn=splitTrainingData, 
                                YName="logerror") {
    trainingIndices = splitFn(Y = XY %>% dplyr::select_at(dplyr::vars(YName)), split_percent = split_percent)

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
    t = rgdal::readOGR(dsn = paste("inputs/", name, sep=""), layer = layer, verbose=FALSE)
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

