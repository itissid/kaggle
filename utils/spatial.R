library(rlang)
transformCoords = function(X) {
    # Internal function
    X %>%
	dplyr::mutate(latitude = latitude/1e6, longitude=longitude/1e6)
}

oc_high_75 = c( 'LAGUNA BEACH' , 'NEWPORT BEACH', 'LAGUNA WOODS','SAN CLEMENTE', 'DANA POINT', "SAN JUAN CAPISTRANO","SANTA ANA",  "SEAL BEACH")

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

addTransactionsPerCounty = function(X) {
    # Add human readable names of countys to the data
    sample_props.1286 = X %>% dplyr::filter(region_county==1286) %>% dplyr::mutate(region_county_name = "orange county")
    sample_props.2061 = X %>% dplyr::filter(region_county== 2061) %>% dplyr::mutate(region_county_name = "ventura county")
    sample_props.3101 = X %>% dplyr::filter(region_county==3101) %>% dplyr::mutate(region_county_name = "la county")

    return(list(sample_props.1286, sample_props.3101, sample_props.2061))
}

groupBySpatialFeature = function(df, f.spatial, var_name){
    # A convenience  function to group var_name by a spatial variable  and then return 

    var_name.enquo = enquo(var_name)
    f.spatial.enquo = enquo(f.spatial)
    var_name_missing = paste(quo_name(var_name.enquo), "_missing", sep="")
    var_name_present = paste(quo_name(var_name.enquo), "_present", sep="")
    # This is a good chance to explain one of two patterns with regards to using dplyr in functions:
    # 1. USE NSE for function args in calls and use enquo and !! patterns for use with dplyr verbs.
    # 2. USE non-NSE semantics, i.e. use strings as function arguments and using !!as.name(.) pattern in dplyr verbs.
    # Note, that if you need to make new variable names from strings
    # then u just needs to make sure to use !! for LHS of expression and
    # !!as.name(<string>) on the RHS eg: summarize(ct = sum(!!as.name(var_name_present), na.rm=TRUE)) .
    # (1) seems an easier choice, but sometimes new names are needed for which strings need to be created.
    # dplyr vars() used  in the *_all, *_at, *_each functions can mix strings and names making programming easier.
    # This is more readable as compared to using mutate_ or group_by_ which are used as:
    # mutate_(.dots=setNames(list(mutate_call), var_name_present)) #ugly
    b = df %>%
        dplyr::mutate(!!var_name_present := is.na(!!var_name.enquo)) %>%
        dplyr::select_at(dplyr::vars(!!f.spatial.enquo, var_name_present)) %>%
        dplyr::group_by_at(dplyr::vars(!!f.spatial.enquo)) %>%
        # One can also do: summarize(ct = sum(!!as.name(var_name_present), na.rm=TRUE))
        dplyr::summarize_at(var_name_present , sum, na.rm=TRUE)

    a = df %>%
        dplyr::mutate(!!var_name_missing := !is.na(!!var_name.enquo)) %>%
        dplyr::select_at(dplyr::vars(!!f.spatial.enquo, var_name_missing)) %>%
        dplyr::group_by_at(dplyr::vars(!!f.spatial.enquo))  %>%
        dplyr::summarize_at(var_name_missing, sum, na.rm=TRUE)
    return(list(missing=a, present=b))
}

# This is how we would use groupBySpatialFeature
countySpatialGrouping = function(df, var_name){
    groupBySpatialFeature(df, region_county, var_name)
}
