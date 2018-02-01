library(doSNOW)
library(foreach)
# *********************************************************************************************************************
# NOTE: install itissid/aws.ec2 to use this instead of cloudyr/aws.ec2. The original one contains bugs that I fixed.
# *********************************************************************************************************************
# Embarrasingly simple parallelism using AWS. Often I will start the cluster using startCluster. however incase I want to 
# do spotBidding for large instances I can chose to 
# start cluster here, ami is based on a "worker node" machine image that I created
# We can only have 20 instances running at once without prior permission from amazon.

#cl <- startCluster(ami="ami-xxxx",key="Keyname",instance.count=19,instance.type="t2.micro",security.groups="sg-xxxx",verbose=TRUE)
#
#
#machines <- cl$instances$dnsName
# setDefaultClusterOptions(port=10187)
# clust <- makeCluster(machines,type="SOCK")
# registerDoSNOW(clust)
#
#
###### Do parallel work here
#  stopCluster(clust)
## stops the SNOW cluster
#  terminateCluster(cl)
## terminates the EC2 worker instances, so billing stops.  

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
                          security.group <- "sg-ec2b7c9f",
                          verbose = TRUE) 
{
    # Assume the keys are in the default ~/.aws/credentials file for this instance.
    #cmd <- paste("ec2-run-instances", ami, "--show-empty-fields", 
    #    "--key", key, "--instance-count", instance.count, "--instance-type", 
    #    instance.type, "--group",security.groups)
    subnet = aws.ec2::describe_subnets()
     # SEE AWS for what this group is.
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
    dnsNames = .getPrivateDNSFromReservations(res)
    return(clusterSpecFromInstances(dnsNames))
}

# use aws.ec2::allocate_ip to allocate IP addresses. The routine assumes you have done that already
assocWithEIP = function(instances_id) {
    elastic_ips = aws.ec2::describe_ips()
    # must be the same number
    assertthat::are_equal(length(instances_id), length(elastic_ips))
    res = apply(cbind(instances_id, elastic_ips), 1 , function(x) {
              instance = x[[1]]
              ip= x[[2]]
              associate_ip(instance, ip)
    })
    return(res)
}

disassocWithEIP = function() {
    elastic_ips = aws.ec2::describe_ips()
    # must be the same number
    res = mapply(function(eip) {
                     print(paste("disassociating: ", eip$allocationId)); aws.ec2::disassociate_ip(eip)}, 
                     elastic_ips)
    return(res)
}

# If you started the cluster from aws web console, use these routines to get DNS names from these two routines.
# you would need to make them into a cluster spec by calling clusterSpecFromInstances() on the DNS name.

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

# Private and helper routines. Don't use these directly.
getExistingInstancesAttr = function(attribute) {
    sapply(aws.ec2::describe_instances(), function(x) {
        sapply(x$instancesSet, function(instance) instance[[attribute]]) 
    })
}
clusterSpecFromInstances = function(instances) {
    instances = as.character(instances)
    names(instances) = rep("host", length(instances))
    return(instances)
}

.getPrivateDNSFromReservations = function(reservations) {
    instancesSets = sapply(reservations, function(x) {x$privateDnsName})
}

.getPublicDNSFromInstances = function(instances) {
   sapply(instances, function(x) aws.ec2::describe_instances(x)[[1]]$instancesSet[[1]]$dnsName)
}

