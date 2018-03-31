.autoMaxCores = function(default.max=2) {
    ncores_p =  parallel::detectCores()
    min(ifelse(is.na(ncores_p), default.max, ncores_p), default.max)
}

# use registerCluster with locaOnly instead of using this
.registerDoParallelWrapper = function(
	max.cores, cluster.spec, log.file.prefix="generic_doparallel_cluster") {
    # Extracts a few things I used to do repeatedly
    ncores_p =  parallel::detectCores()
    ncores_p = min(ifelse(is.na(ncores_p), 4, ncores_p), max.cores)

    d = format(Sys.time(), "%Y%m%d-%H-%M-%S")
    dated.filename = paste0(paste(log.file.prefix, d, sep="_"), ".log")
    cl <- parallel::makeCluster(rep(cluster.spec, ncores_p), outfile=dated.filename);
    doParallel::registerDoParallel(cl)
    flog.info(paste("Logging cluster output to", dated.filename)) 
    return(cl)
}


.registerDoSNOWCluster = function(spec, ncores.per.machine, log.file.prefix="generic_snow_cluster", port) {
    d = format(Sys.time(), "%Y%m%d-%H-%M-%S")
    dated.filename = paste0(paste(log.file.prefix, d, sep="_"), ".log")
    snow::setDefaultClusterOptions(port=port)
    clus = snow::makeCluster(spec, type="SOCK",outfile=dated.file)
    doSNOW::registerDoSNOW(clus)
    return(clus)
}

#########################################
########### READ ME FIRST ##############
# Main routine that chooses a local vs multimachine cluster
########################################
# If localOnly=TRUE then you need to just set max.cores.per.machine. Set it to Inf if you want to use all the cores or some lower value if you prefer. The instances.opts is ignored in this case
# if localOnly=FALSE then send the instances.opts as a list of spec(see example) or reachable IP's. In this case you MUST specify a finite max.cores.per.machine and that is the # of R processes on each machine that will be spawned.
# set max.cores.per.machine = Inf
# the port
# Limitations: Cluster must be "balanced". Precisely if # processors on some machines is different than others, this routine cannot scale up the # processors (atleast not yet)
# Examples:
# clus = registerCluster(max.cores.per.machine=4) # Use 4 cores of local
# clus = registerCluster(max.cores.per.machine=Inf) # Use all cores
# clus = registerCluster(localOnly=FALSE, 
#			 instances.spec = list(
#			    list(host="ec2.1.2.3.4.compute1.amazonaws.com"),
#			    list(host="ec2.1.2.3.5.compute1.amazonaws.com"),
#			    list(host="ec2.1.2.3.6.compute1.amazonaws.com")))
registerCluster = function(
	    localOnly=TRUE, 
	    instances.opts=list(host="localhost"), 
	    max.cores.per.machine=2,
	    port=12345) {
	ncores = .autoMaxCores(max.cores.per.machine)
	if(localOnly) {
		cls = .registerDoParallelWrapper(ncores, list(host="localhost"))
	} else {
		spec = c(sapply(instances.opts, function(x) rep(list(x), max.cores.per.machine)))
		cls = .registerDoSNOWCluster(spec, max.cores.per.machine)
	}
	return(cls)
}

stopParallelCluster = function(clus) {
    # Useful in notebooks to stop the cluster
    parallel::stopCluster(clus)
    doParallel::stopImplicitCluster()
}
