### For reading and writing the various models and submissions to disk ###

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
