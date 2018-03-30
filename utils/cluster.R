registerDoParallelWrapper = function(max.cores = 4, log.file.prefix="generic_cluster", cluster.spec=list(host="localhost")) {
    # Extracts a few things I used to do repeatedly
    ncores_p =  parallel::detectCores()
    ncores_p = min(ifelse(is.na(ncores_p), 4, ncores_p), max.cores)

    d = format(Sys.time(), "%Y%m%d-%H-%M-%S")
    dated.filename = paste(log.file.prefix, d, sep="_")
    if(!dir.exists("log/")) {
       dir.create("log/") 
    }
    dated.file.path = paste('log/', dated.filename, '.log', sep="")
    cl <- parallel::makeCluster(rep(cluster.spec, ncores_p), outfile=dated.file.path);
    doParallel::registerDoParallel(cl)
    flog.info(paste("Logging cluster output to", dated.file.path)) 
    return(cl)
}

stopParallelCluster = function(clus) {
    # Useful in notebooks to stop the cluster
    parallel::stopCluster(clus)
    doParallel::stopImplicitCluster()
}
