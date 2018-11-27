library(MASS)
library(mclust)
library(cluster)
library(ggplot2)
library(GGally)
library(devtools)
library(caret)
library(MixGHD)
library(gbm)
library(ROCR)
#source_gist("https://gist.github.com/mrdwab/6424112", filename = "stratified.R")



## Section 1 Introduction 
# =======================================
# citation("MASS") citation for MASS
# citation() citation for R
# Load diabetes dataset 
pima <- rbind(Pima.tr,Pima.te)

## Section 2 Pima Dataset
# =======================================
#pairs(pima, col = (as.numeric(pima$type) ))
ggpairs(pima[,-8], aes(colour = pima$type ))

# Scale 
pima[,-8] <- scale(pima[,-8])


## Section 3 Methodology 
# =======================================
# Split up into training and test set. 
# A training/ test set split of 80-20 is chosen for convience, in addition, a stratified sample of 
# each diabetic type is chosen to balance the training set in hopes to reduce over-fitting on
# a specific data type. 

# source_gist("https://gist.github.com/mrdwab/6424112", filename = "stratified.R")

# There are more yes sets than no, 
#set.seed(20)
#train.st <- stratified(pima, group = "type",size = as.integer(0.80*dim(pima[pima$type == "Yes",])[1]))
#train.st.indx <- as.numeric(rownames(train.st))

# So I will bootstrap on the yes, while adding no in the future. 

# Subsection 3.1 Data Preperation
# ================================
# Non stratified sample but bootstrapped, with 80-20 training test split


# run this for every section 
set.seed(20)
len_pima <- dim(pima)[1]
ind_train <- sample(1:len_pima, as.integer(len_pima*0.80))
train.n <- pima[ind_train,]
test.n  <- pima[-ind_train,]
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







# ==================================================
# Hyperbolic Discriminant Analysis
# ==================================================

library(MixGHD)

 # run this for every section 
set.seed(20)
len_pima <- dim(pima)[1]
ind_train <- sample(1:len_pima, as.integer(len_pima*0.80))
train.n <- pima[ind_train,]
test.n  <- pima[-ind_train,]
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


# Done cleaning Data
# ==================================================

skew_model <- DA(train = train.n[,-8],trainL = train.n[,8] + 1 ,test = test.n[,-8], testL = test.n[,8] + 1,
                 method="MGHD", # MGHD MGHFA, MSGHD, cMSGHD, MCGHD Other ones take too long to run
                 starting="km",
                 max.iter=1000,
                 eps=1e-2,q=1:3)

table(test.n$type,skew_model$testMembership)


adjust <- adjustedRandIndex(test.n$type,skew_model$testMembership)
c_error <- classError(test.n$type,skew_model$testMembership)

print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))


# ============================================================
# Boostrapped
skew_model.bs <- DA(train = train.bs[,-8],trainL = train.bs[,8] + 1 ,test = test.n[,-8], testL = test.n[,8] + 1,
                 method="MGHD", # MGHD MGHFA, MSGHD, cMSGHD, MCGHD Other ones take too long to run
                 starting="km",
                 max.iter=1000,
                 eps=1e-2,q=1:3)

table(test.n$type,skew_model$testMembership)


adjust <- adjustedRandIndex(test.n$type,skew_model.bs$testMembership)
c_error <- classError(test.n$type,skew_model.bs$testMembership)

print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))


# ==================================================
# Hyperbolic with Boosting 
# ==================================================
library(MixGHD)
model=MGHD(train.n[,-8],G=1:6,max.iter=100)
table(train.n[,8],model@map)
#ggpairs(train.n[,-8], aes(colour = factor(model@map) ))

# Generate a cross label feature map 
skew_lab <- as.numeric(model@map)
train.skew <- cbind(train.n,skew_lab)

# Initialize random memberships but DA will generate new ones that make sense 
rand_assignment <- sample(1:5,
                          size = length(test.n[,1]),
                          replace = TRUE)

skew_model <- DA(train = train.skew[,c(-8,-9)],trainL = model@map,test = test.n[,-8], testL = rand_assignment,
                 method="MGHD",starting="km",
                 max.iter=100,
                 eps=1e-2,q=1:4)

table(test.n$type,skew_model$testMembership)

test.skew <- test.n
test.skew$skew_lab <- skew_model$testMembership
skew_lab <- skew_model$testMembership
#train.skew$type <- as.numeric(train.skew$type == "Yes")


f_diabetes <- formula(type ~ age + ped + bmi + skin + bp + glu + npreg + skew_lab )

ntrees <- 376
boost.skew = gbm(f_diabetes,
                 data= train.skew,
                 distribution="bernoulli",
                 n.trees=ntrees,
                 interaction.depth=1,
                 shrinkage = 0.01,
                 n.minobsinnode = 10)

y_hat_1 = as.numeric( 0.5 < predict(boost.skew, newdata = train.skew, n.trees = ntrees,type=  "response"))
y_true_1 = train.n$type
table(y_hat_1, y_true_1)

test.skew <- as.data.frame(test.skew)

y_hat_2 = as.numeric(0.5 < predict(boost.skew, newdata = test.skew ,n.trees = ntrees,type=  "response"))
y_true_2 <- test.skew$type


adjust <- adjustedRandIndex(y_hat_2,y_true_2)
c_error <- classError(y_hat_2, y_true_2)

print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))

# new plots 

test.skew.n <- test.skew
test.skew.n$y_pred <- y_hat_2

y_hat_2 = as.numeric( predict(boost.skew, newdata = test.skew ,n.trees = ntrees,type=  "response"))
y_true_2 <- test.skew$type

pred_boost <- prediction(y_hat_2,y_true_2)
perf_boost <- performance(pred_boost, measure = "tpr", x.measure = "fpr")
plot(perf_boost)
abline(a=0, b=1)

perf_boost <- performance(pred_boost, measure = "tnr", x.measure = "fnr")
plot(perf_boost)
abline(a=0, b=1)


# ============================================================================= #
# Boostrapped version #
# ====================#
library(MixGHD)
model=MGHD(train.bs[,-8],G=1:6,max.iter=100)
table(train.bs[,8],model@map)
#ggpairs(train.n[,-8], aes(colour = factor(model@map) ))

# Generate a cross label feature map 
skew_lab <- as.numeric(model@map)
train.skew.bs <- cbind(train.bs,skew_lab)

# Initialize random memberships but DA will generate new ones that make sense 
rand_assignment <- sample(1:5,
                          size = length(test.n[,1]),
                          replace = TRUE)

skew_model <- DA(train = train.skew.bs[,c(-8,-9)],trainL = model@map,test = test.n[,-8], testL = rand_assignment,
                 method="MGHD",starting="km",
                 max.iter=100,
                 eps=1e-2,q=1:4)

table(test.n$type,skew_model$testMembership)

test.skew <- test.n
test.skew$skew_lab <- skew_model$testMembership
skew_lab <- skew_model$testMembership


f_diabetes <- formula(type ~ age + ped + bmi + skin + bp + glu + npreg + skew_lab )

# Caret with 5 cv validation founds 600 to be the best 
ntrees <- 376
boost.skew.bs = gbm(f_diabetes,
                 data= train.skew,
                 distribution="bernoulli",
                 n.trees=ntrees,
                 interaction.depth=1,cv.folds = 5,
                 shrinkage = 0.01,
                 n.minobsinnode = 10)

y_hat_1 = as.numeric( 0.5 < predict(boost.skew.bs, newdata = train.skew.bs, n.trees = ntrees,type=  "response"))
y_true_1 = train.bs$type
table(y_hat_1, y_true_1)

test.skew <- as.data.frame(test.skew)

y_hat_2 = as.numeric(0.5 < predict(boost.skew.bs, newdata = test.skew ,n.trees = ntrees,type=  "response"))
y_true_2 <- test.skew$type


adjust <- adjustedRandIndex(y_hat_2,y_true_2)
c_error <- classError(y_hat_2, y_true_2)

print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))


# ======== TRAINING A SKEWED FEATURE BOOSTED MODEL =============================== #


#f_diabetes <- formula(type ~ age + ped + bmi + skin + bp + glu + npreg + skew_lab )


#train.skew$type <- factor(train.skew$type)
#levels(train.skew$type) <- c("No","Yes")


#train.skew.bs$type <- factor(train.skew.bs$type)
#levels(train.skew.bs$type) <- c("No","Yes")


# Training parameters
#train_gbm <- trainControl(method = "cv",
#                          number = 5, 
#                          classProbs = TRUE,
#                          summaryFunction = twoClassSummary)


#caretGrid <- expand.grid(interaction.depth=1:7,
#                         n.trees = seq(1,500,5),
#                         shrinkage=c(0.01, 0.1001),
#                         n.minobsinnode=10)

# Actually train the gbm 
#dskew.gbm <- train(f_diabetes,
 #                  data = train.skew.bs,
  #                 method = "gbm",
   #                distribution = "bernoulli",
    #               trControl = train_gbm,
     #              tuneGrid= caretGrid,
      #             metric = "ROC")


#summary(dskew.gbm)

#Tuning parameter 'n.minobsinnode' was held constant at a value of 10
#ROC was used to select the optimal model using the largest value.
#The final values used for the model were n.trees = 376, interaction.depth = 1, shrinkage = 0.01 and n.minobsinnode = 10.


# =========================================================================== #
# Boosted Trees ============================================================= #
# =========================================================================== #

library(e1071)
library(randomForest)
library(MASS)
library(tree)
library(gbm)


# run this for every section 
set.seed(20)
len_pima <- dim(pima)[1]
ind_train <- sample(1:len_pima, as.integer(len_pima*0.80))
train.n <- pima[ind_train,]
test.n  <- pima[-ind_train,]
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

train.n <- as.data.frame(train.n)
test.n <- as.data.frame(test.n)


# Done cleaning Data  
# ===========================================

ntrees <- 311
f_diabetes <- formula(type ~ age + ped + bmi + skin + bp + glu + npreg )

boost.pima=gbm(f_diabetes,
               data= train.n,
               distribution="bernoulli",
               n.trees=ntrees,
               interaction.depth=2,
               shrinkage = 0.01,
               n.minobsinnode = 10)

sm.boost.pima <- summary(boost.pima)
y_hat = predict(boost.pima, newdata = train.n,n.trees = ntrees,type=  "response")
pred_hat <- as.numeric(0.5 < y_hat)
true_train <- train.n$type
table(pred_hat, true_train)

adjust <- adjustedRandIndex(pred_hat,true_train)
c_error <- classError(pred_hat,true_train)


print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))



test_hat = as.numeric(0.5 < predict(boost.pima, newdata = test.n,n.trees = ntrees,type=  "response"))
true_test = test.n$type

table(test_hat, true_test)

adjust <- adjustedRandIndex(test_hat,true_test)
c_error <- classError(test_hat, true_test)


print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))



# Use below for training 
train.n$type <- as.factor(train.n$type)
levels(train.n$type) <- c("No","Yes")

## Training using caret. 

#ROC was used to select the optimal model using the largest value.
#The final values used for the model were n.trees = 311, interaction.depth =
#  2, shrinkage = 0.01 and n.minobsinnode = 10.



# Training parameters
train_gbm <- trainControl(method = "cv",
                          number = 5, 
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)


caretGrid <- expand.grid(interaction.depth=1:7,
                         n.trees = seq(1,500,5),
                         shrinkage=c(0.01, 0.001),
                         n.minobsinnode=10)

# Actually train the gbm 
dskew.gbm <- train(f_diabetes,
                   data = train.n,
                   method = "gbm",
                   distribution = "bernoulli",
                   trControl = train_gbm,
                   tuneGrid= caretGrid,
                   metric = "ROC")


summary(dskew.gbm)

print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))



