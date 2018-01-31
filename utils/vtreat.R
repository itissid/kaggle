
prepareDataFeaturesWithVtreat = function(XY, treated.features, vtreat.opts) {
    # vtreat has a package to encode high cardinality categorical variables either by techniques such as 
    # selective one hot encoding, selecting only those variables that have a signal with y, 
    # or as in case of category columns with lots of NA, subsititue a dummy value that actually provide some signal instead of dropping it. 
    # This is used with the prepareData() routine that does feature engineering.
    # USE CAREFULLY: Especially when placing it among other preprocessing routines. A good example is
    # using it in prepareData above, you want to place it BEFORE you call pruneFeatures.
    # This routines returns a treatment plan for treated.fetaures. A good use case is high cardinality
    # variables like city/zipcodes.
    # For regression use like
    # 0. Recode all variables that are chars
    # 1. Prepare Treatments on a subset of features
    # 2. logtransform features. make sure no treated vars are log transformed
    # 3. prune features that have a lot of missing data. Again no pruned features should have been treated already
    # 4. Apply the treatment frame to a copy of X and join then with X.
    # ***** The set of features retained in X is colnames(X) - scoreFrame$origName  ****
    # RETURN: a treatmentplan, train data set and function for giving test data sets
    ###### PREPARE THE DATA SET USING VTREAT #####
    scale.features = vtreat.opts$scale.features
    usecached = vtreat.opts$usecached.plan
    pruneSig = vtreat.opts$pruneSig
    print("..P")
    d = format(Sys.time(), "%Y%m%d")
    fn = paste("cache/tplan", d, sep="_")
    if(usecached==F) {  # SET ME CARE FULLY. ONLY IF YOU WANT TO DEBUG/SAVE TIME
        tplan <- createCrossFrameTreatment(
            XY,
            features=treated.features, # Try paying attention to the high cardinality vars first
            vtreat.grid=data.frame(smFactor = 0.01, rareCount = 50, rareSig = 0.02),
            makeLocalCluster=T,
            crossFrameCluster=snow::makeCluster(1))
        if(!dir.exists("cache")) {
            dir.create("cache")
        }
        saveRDS(tplan, fn)
    } else if(file.exists(fn)) {

         print(paste("
             ##########################################
             ######## option set for USING A CACHED treatment plan!!
             ##########################################
         "))
         tplan = readRDS(fn)
    } else {
        print(paste("ERROR: Asked to use cache but cache file ", fn , "does not exist!. "))
        stopifnot(TRUE)
    }

    assertthat::assert_that(nrow(tplan[[1]]$crossFrame) > 0)
    vScoreFrame <- tplan[[1]]$treatments$scoreFrame
    scoreFrame.filtered <- vScoreFrame %>% dplyr::filter(sig<=0.02)
    features.vtreat.restricted = scoreFrame.filtered %>% dplyr::pull(varName) %>% sort
    features.vtreat.treated = scoreFrame.filtered %>% dplyr::distinct(origName) %>% dplyr::pull(origName) %>% sort

   ######## PREPARE THE TEST AND TRAIN DATA SETS WITH THE TREATMENT PLAN ######
    trainVtreat <- vtreat::prepare (
        tplan[[1]]$treatments,
        XY, ############# TAKE CARE OF THE DATA SET NAME ###########
        varRestriction = features.vtreat.restricted,
        scale=scale.features,
        ############# VARIABLES PRUNED BY SIG. IN THE CROSS FRAME ########
        pruneSig=NULL)

    # We return a function instead as this is what we need to use to generate the properties data sets with dates
    testDataGenerator = function(tplan, scale.features, pruneSig) {
        pruneSignal.test = pruneSig
        scale.test.features = scale.features
        function(XTest) {
            vtreat::prepare(
                tplan[[1]]$treatments, XTest,
                varRestriction = features.vtreat.restricted, ############# VARIABLES PRUNED BY SIG. IN THE CROSS FRAME ########
                scale=scale.test.features,
                pruneSig=pruneSignal.test)
        }
    }
    ############################################
    return(list(tplan, trainVtreat, testDataGenerator(tplan, scale.features, pruneSig)))
}
