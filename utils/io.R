### For reading and writing the various models and submissions to disk ###

readPropertiesFile = function(fname) {
    # Helper utils to Read the raw data from the properties files
    data.table::fread(fname, strip.white=T, na.strings=c(NA, ''), stringsAsFactors=F)
}

saveModel = function(model, description="Generic model") {
    d = format(Sys.time(), "%Y%m%d_%H-%M-%S")
    saveRDS(model, paste(d, description, sep="_"))
    # Return the file name you wrote
    return(paste(d, description, sep="_"))
}

write_submission_file <- function(model, te, var_names, target_months, results_path){
  sub_name <- paste(results_path, "submission_",
                    model$best_score,"_", Sys.time(),".csv", sep = "")
  sub_name <- gsub(":", "-", sub_name)
  sub_name <- gsub(" ", "_", sub_name)

  sub_data <- predict_monthly_errors(model, te, var_names, target_months)

  write_csv(sub_data, sub_name)
  return(sub_name)
}

writePredictions = function(predictions,
                            filename.suffix= 'generic_model_submission',
                            expect.colnames=c("parcelid", "201610", "201611" ,"201612", "201710", "201711", "201712")) {
    # Predict on a test data set, returns the name of the file written
    assertthat::assert_that(base::setequal(colnames(predictions) , expect.colnames))
    d = format(Sys.time(), "%Y%m%d_%H-%M-%S")

    dated.filename = paste(d, filename.suffix, sep="_")
    file.path = paste('results/', dated.filename, '.csv.gz', sep="")
    z = gzfile(file.path)
    write.csv(predictions %>% dplyr::select_at(dplyr::vars(expect.colnames)), file=z, row.names=FALSE)
    return(file.path)
}


# This method joins properties from 2017 data that was released later in the competion
# to the 2016 data. For features present in both the data, it also returns summaries of 
# 1. how much data is NA in 2016 and is not NA in 2017
# 2. how much data is different
# After calling this  call processJoint1617Data function to rename the features removing the ".x" and ".y".
merge2017Data = function(tr.2016, pr.2016, pr.2017, join_features=join_features.default) {
    # The only feature that actually comes up is quality. but its only ~ 1.2% of the cases
    # tr.X transaction data set for year X
    # pr.x properties data set for year X

    tr.join = tr.2016 %>% dplyr::inner_join(pr.2017 %>% dplyr::select(c(join_features, "id_parcel")),
                                            by="id_parcel")
    pr.join = pr.2016 %>% dplyr::inner_join(pr.2017 %>% dplyr::select(c(join_features, "id_parcel")),
                                            by="id_parcel")
    print(".")
    assertthat::assert_that(nrow(tr.join) == nrow(tr.2016)) # the rows are the same
    assertthat::assert_that(nrow(pr.join) == nrow(pr.2016)) # the rows are the same
    join_features.left = sapply(join_features, function(f){paste(f, ".x", sep="")})
    join_features.right = sapply(join_features, function(f){paste(f, ".y", sep="")})
    # For the features that were found in both '16 and '17 data, do a check if they changed. This is
   # to make sure that the data are sane. 
    for(i in 1:length(join_features.left)) {
        feature.x = join_features.left[[i]]
        feature.y = join_features.right[[i]]
        feature.newdata = paste(join_features[[i]], "_new", sep="")
        feature.diffdata = paste(join_features[[i]], "_diff", sep="")
        feature.x.name = quo(!!as.name(feature.x))
        feature.y.name = quo(!!as.name(feature.y))
        tr.join %<>% dplyr::mutate(
                         !!feature.newdata := is.na(!!as.name(feature.x)) & !is.na(!!as.name(feature.y))) %>%
                       dplyr::mutate(
                         !!feature.diffdata := !is.na(!!feature.x.name) & !is.na(!!feature.y.name) & as.character(!!feature.x.name) != as.character(!!feature.y.name))

        pr.join %<>% dplyr::mutate(
                         !!feature.newdata := is.na(!!as.name(feature.x)) & !is.na(!!as.name(feature.y))) %>%
                       dplyr::mutate(
                         !!feature.diffdata := !is.na(!!feature.x.name) & !is.na(!!feature.y.name) & as.character(!!feature.x.name) != as.character(!!feature.y.name))
    }
    print("..")
    # The missing values
    missing.2016 = tr.join %>% dplyr::select_at(vars(ends_with("_new"))) %>% dplyr::summarize_all(funs(sum(.))) %>% tidyr::gather() %>% arrange(value)
    diff.vals.2016 = tr.join %>% dplyr::select_at(vars(ends_with("_diff"))) %>% dplyr::summarize_all(funs(sum(.))) %>% tidyr::gather() %>% arrange(value)
    pr.missing.2016 = pr.join %>% dplyr::select_at(vars(ends_with("_new"))) %>% dplyr::summarize_all(funs(sum(.))) %>% tidyr::gather() %>% arrange(value)
    pr.diff.vals.2016 = pr.join %>% dplyr::select_at(vars(ends_with("_diff"))) %>% dplyr::summarize_all(funs(sum(.))) %>% tidyr::gather() %>% arrange(value)
    print("...")
    for(i in 1:length(join_features.left)) {
        left = join_features.left[[i]]
        right = join_features.right[[i]]
        tr.join %<>%
            dplyr::mutate(!!left := ifelse(is.na(!!as.name(left)), !!as.name(right), !!as.name(left)))
        pr.join %<>%
            dplyr::mutate(!!left := ifelse(is.na(!!as.name(left)), !!as.name(right), !!as.name(left)))
    }
    return(list(tr.join, pr.join, missing.2016, diff.vals.2016, pr.missing.2016, pr.diff.vals.2016))
}

processJoint1617Data = function(X,
                                join_features=join_features.default,
                                non_join_features=non_join_features.default) {
    join_features.left = sapply(join_features, function(f) {paste(f, ".x", sep="")})
    for(i in 1:length(join_features.left)) {
        old = join_features[[i]]
        left = join_features.left[[i]]
        print(old)
        print(left)
        X %<>% dplyr::rename_(.dots=setNames(left, old))
    }
    X %<>% select_at(vars(c(join_features, non_join_features)))
}
