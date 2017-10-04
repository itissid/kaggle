library(doSNOW)
library(foreach)


# need to re-run these functions from AWS.tools so that they are added to the namespace 
sleep.while.pending <- function(instances, sleep.time=2,verbose=TRUE) {
    while(pending.instance(instances)) {
        if(verbose) { cat(".") }
        Sys.sleep(sleep.time)
    }
    if(verbose) { cat("\n") }
}


pending.instance <- function(instances ) {

    ## test both state and public dns status
    instanceStatus = lapply(instances, function(x) aws.ec2::instance_status(x))
    if(any(sapply(instanceStatus, function(x) length(x)) == 0)) {
        ans <- TRUE
    } else {
        state = sapply(instanceStatus, function(x) x[[1]]$instanceState$name[[1]])
        status = sapply(instanceStatus, function(x) x[[1]]$instanceStatus$status[[1]])
        if(all(state=="running") & all(status=="ok")) {
            ans <- FALSE
        } else {
            ans <- any(status=="initializing" | status == "pending")
        }
    }
    ans
}

createSgNameFromTime = function() {
    t = lubridate::now()
    yymmdd_hhmmss =  paste(
      c(lubridate::year(t), lubridate::month(t), lubridate::day(t), 
        lubridate::hour(t), lubridate::day(t), lubridate::minute(t)),
      sep="", collapse="_")
    return(paste("sg_", yymmdd_hhmmss, sep="", collapse=""))
}


# I edited the startCluster function so that it has a "security groups" option.
startCluster <- function (ami,
                          instance.count,
                          instance.type,
                          keypair="sid-aws-key", # This is needed just for access
                          verbose = TRUE) 
{
    # Assume the keys are in the default ~/.aws/credentials file for this instance.
    #cmd <- paste("ec2-run-instances", ami, "--show-empty-fields", 
    #    "--key", key, "--instance-count", instance.count, "--instance-type", 
    #    instance.type, "--group",security.groups)
    subnet = aws.ec2::describe_subnets()
    security.group <- "sg-ec2b7c9f" # SEE AWS for what this group is.
    # Need to add a key pair and the private file to the started instances.
    # Also I want to add it to the .ssh/config files to manage ssh without password.
    res = aws.ec2::run_instances(
           ami, 
           type=instance.type, 
           min=instance.count, 
           max=instance.count, 
           keypair=keypair,
           subnet=subnet[[1]], 
           sgroup=security.group,
           )
    instances = as.character(sapply(res, function(x) as.character(x$instanceId[[1]])))

    #if (verbose) {
    #    cat("using this cmd:\n")
    #    print(cmd)
    #}
    sleep.while.pending(instances, verbose=verbose, sleep.time=10)
    dnsNames = getPrivateDNSFromReservations(res)
    return(clusterSpecFromInstances(dnsNames))
}

clusterSpecFromInstances = function(instances) {
    instances = as.character(instances)
    names(instances) = rep("host", length(instances))
    return(instances)
}

getExistingInstancesPublicDNS = function() {
    sapply(aws.ec2::describe_instances(), function(x) {
        sapply(x$instancesSet, function(instance) instance$dnsName) 
    })
}

getExistingInstancesPrivateDNS = function() {
    reservations = aws.ec2::describe_instances()
    instancesSets = sapply(reservations, function(x) {x$instancesSet})
    instanceSets.flat = unlist(instancesSets, recursive=T)
    instanceSets.flat[names(instanceSets.flat) == "privateDnsName"]
}

getPrivateDNSFromReservations = function(reservations) {
    instancesSets = sapply(reservations, function(x) {x$privateDnsName})
}

getPublicDNSFromInstances = function(instances) {
   sapply(instances, function(x) aws.ec2::describe_instances(x)[[1]]$instancesSet[[1]]$dnsName)
}

# start cluster here, ami is based on a "worker node" machine image that I created
# We can only have 20 instances running at once without prior permission from amazon.
#cl <- startCluster(ami="ami-xxxx",key="Keyname",instance.count=19,instance.type="t2.micro",security.groups="sg-xxxx",verbose=TRUE)
#
#
#machines <- cl$instances$dnsName
#setDefaultClusterOptions(port=10187)
#clust <- makeCluster(machines,type="SOCK")
#registerDoSNOW(clust)
#
#
###### Do parallel work here
#
#stopCluster(clust)
## stops the SNOW cluster
#terminateCluster(cl)
## terminates the EC2 worker instances, so billing stops.  
