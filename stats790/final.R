
install_packs <- function(){
  packs <- c("MASS","mclust","ggplot2","GGally","dplyr",
             "mvtnorm","dirichletprocess","parallel" )
  for(pack in packs){
    install.packages(pack)
  }
}
# install_packs() # uncomment this to install everything

library(MASS) # load PIMA dataset 
library(mclust) # mclust for clustering. 
library(ggplot2) # plotting stuff 
library(GGally) # more plotting stuff 
library(dplyr) # sorting 
library(mvtnorm) # density needed to calculate e-step. 
library(dirichletprocess) # infinite gaussian mixtures. 
library(parallel) # parallel for server processes. 


pima <- rbind(Pima.tr,Pima.te)
## Pairs plot for the two groups
pairs <- ggpairs(pima[,-8], aes(colour = pima$type ))
pairs
# Scale 
pima[,-8] <- scale(pima[,-8])
# classes are not even so I am going to do bootstrap
# sampling to create my own training set
set.seed(15)
len_pima <- dim(pima)[1]
ind_train <- sample(1:len_pima, as.integer(len_pima*0.80))
train.n <- pima[ind_train,]
test.n  <- pima[-ind_train,]
# scaling some stuff
train.n[,-8] <- scale(train.n[,-8])
test.n[,-8] <- scale(test.n[,-8])

## boot strapped training sample
# get indexs of all yes values in training 
ind.yes <- rownames(train.n[train.n$type == "Yes",])
train.bs <- rbind(train.n,train.n[sample(ind.yes, 
                                         size = as.integer(as.numeric(table(train.n$type)[1] - table(train.n$type)[2])),
                                         replace = TRUE),])
train.n$type <- as.numeric(train.n$type == "Yes")
test.n$type <- as.numeric(test.n$type == "Yes")

# checking to see if classes are weighted equally. 
table(train.bs$type) # done. 
train.bs$type <- as.numeric(train.bs$type == "Yes")

## Parsimonious family of gaussian models. 
mm <- Mclust(train.bs[,-8])
## fit all 14 models and see the clusters.

pairs_clust <- ggpairs(train.bs[,-8],   aes(color = as.factor(mm$classification)),
        lower = list( continuous = wrap("points",size = 0.5))
          )
pairs_clust
## it looks like there is a lot of overlap between classes. 
# the green points and red points belong to to the diabetic class while the other ones 
# belong to the non diabetic 

## Parsimonious family of gaussian models. 
mm <- Mclust(train.bs[,-8],G = 2)
## fit all 14 models and see the clusters.

pairs_clust <- ggpairs(train.bs[,-8],   aes(color = as.factor(mm$classification)),
                       lower = list( continuous = wrap("points",size = 0.5))
)
pairs_clust

table(train.bs$type,mm$classification)
adjustedRandIndex(train.bs$type,mm$classification)



#
dp <- DirichletProcessMvnormal(as.matrix(train.bs[,-8]),alphaPriors = )
dp <- Fit(dp, 1000)
clus_labels <- dp$clusterLabels
pairs_clust <- ggpairs(train.bs[,-8],   aes(color = as.factor(clus_labels)),
                       lower = list( continuous = wrap("points",size = 0.5)))
pairs_clust

table(clus_labels,train.bs$type)
adjustedRandIndex(clus_labels,train.bs$type)

# custom function for prediction on the test set 
predict_dp <- function(mod, new_data ){
  # mixing proportions 
  pigs <- mod$weights
  G <- length(pigs)
  mus <- mod$clusterParameters$mu
  sigs <- mod$clusterParameters$sig
  n = dim(new_data)[1]
  zigs = matrix(0,n,G)
  
  for(i in 1:n){
    for(g in 1:G){
      zigs[i,g] = pigs[1]*dmvnorm(new_data[i,-8],mus[,,g],sigs[,,g])
    }
  }
  
  for(i in 1:n){
    zigs[i,] = zigs[i,]/sum(zigs[i,])
    zigs[i,G] = 1 - sum(zigs[i,1:(G-1)])
  }
  
  # map them 
  znaks = rep(0,n)
  for(i in 1:n){
    znaks[i] = match(max(zigs[i,]),zigs[i,])
  }
  
  znaks 
}


vlabs <- predict_dp(dp,test.n[,-8])
table(vlabs,test.n$type)
adjustedRandIndex(vlabs,test.n$type)



# MSE for dirichlet processes 
# first one is default
potential_priors <- list(c(2,4),c(1,2),c(3,5),c(3,3))

ari_boot_dp <- function(prior,num_boots = 50)
{
  ari_class <- rep(0,num_boots)
  # boot strap draws 50 times 
  for(z in 1:num_boots){
    pima <- rbind(Pima.tr,Pima.te)
    # Scale 
    pima[,-8] <- scale(pima[,-8])
    # classes are not even so I am going to do bootstrap
    # sampling to create my own training set
    len_pima <- dim(pima)[1]
    ind_train <- sample(1:len_pima, as.integer(len_pima*0.80))
    train.n <- pima[ind_train,]
    test.n  <- pima[-ind_train,]
    # scaling some stuff
    train.n[,-8] <- scale(train.n[,-8])
    test.n[,-8] <- scale(test.n[,-8])
    
    #fit the model
    dp <- DirichletProcessMvnormal(as.matrix(train.bs[,-8]),alphaPriors = )
    dp <- Fit(dp, 1000)
    clus_labels <- dp$clusterLabels # get the cluster labels from the training set 

    # using the custom function get the predictions on the test set
    vlabs <- predict_dp(dp,test.n[,-8])
    # get the ari and allocate to the the vector keeping track of it
    ari_class[z] <- adjustedRandIndex(vlabs,test.n$type)
  }
  # return the ari_class 
  ari_class
}

# test function for results 
# each run takes approximately 8 minutes. times 50 times x number of priors is a very long time
rez_2 <- ari_boot_dp(c(1,2),num_boots = 50) 
# running this on the server takes a few minutes. 


library(parallel)
# i had to parallize this because it ran too long serially. 
ari_boot_par_dp <- function(prior,num_boots = 50)
{
  # boot strap draws 50 times 
  run_model <- function(b_id){
    pima <- rbind(Pima.tr,Pima.te)
    ## Pairs plot for the two groups
    pairs <- ggpairs(pima[,-8], aes(colour = pima$type ))
    pairs
    # Scale 
    pima[,-8] <- scale(pima[,-8])
    # classes are not even so I am going to do bootstrap
    # sampling to create my own training set
    len_pima <- dim(pima)[1]
    ind_train <- sample(1:len_pima, as.integer(len_pima*0.80))
    train.n <- pima[ind_train,]
    test.n  <- pima[-ind_train,]
    # scaling some stuff
    train.n[,-8] <- scale(train.n[,-8])
    test.n[,-8] <- scale(test.n[,-8])
    
    #fit the model
    dp <- DirichletProcessMvnormal(as.matrix(train.bs[,-8]),alphaPriors = )
    dp <- Fit(dp, 1000)
    clus_labels <- dp$clusterLabels # get the cluster labels from the training set 
    
    # using the custom function get the predictions on the test set
    vlabs <- predict_dp(dp,test.n[,-8])
    # get the ari and allocate to the the vector keeping track of it
    adjustedRandIndex(vlabs,test.n$type)
  }
  
  ari_cluster <- mcmapply(run_model, 1:num_boots,num_boots) # clusterApply(cl,1:num_boots,run_model)
  ari_class <- unlist(ari_cluster)
}

rezlts <- list()
for(pr in potential_priors){
  rezlts <- append(rezlts,ari_boot_par_dp(c(1,2),50))
}

# now to plot results 

rezlts <- list()



