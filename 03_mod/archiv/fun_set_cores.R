################################################################################
# Sets up for parallel computation
################################################################################

parallel::detectCores()
n.cores <- parallel::detectCores() - 1
# create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

# check cluster definition (optional)
print(my.cluster)
## socket cluster with 7 nodes on host 'localhost'
# register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

# check if it is registered (optional)
foreach::getDoParRegistered()
#how many workers are available? (optional)
foreach::getDoParWorkers()

################################################################################
# End
################################################################################