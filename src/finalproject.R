library(MASS)
library(mclust)
library(cluster)

# Load diabetes dataset 
pima <- rbind(Pima.tr,Pima.te)
pairs(pima, col = (as.numeric(pima$type) + 1))

# PCA Into Neural Network, Mixture Discermeent, and Boosted Trees

# neural network with scaled input parameters. 
library(neuralnet)
pima[,-8] <- scale(pima[,-8])
pima$type <- as.numeric(pima$type == "Yes")


results <- data.frame(1,1,1,1,1)
colnames(results) <- c("ARI","Error","Hid_1","Hid_2","Hid_3")
results <- results[-1,] 

f_diabetes <- formula(as.numeric(type=="Yes") ~ age + ped + bmi + skin + bp + glu + npreg)
for(i in 3:6){
  for(j in 0:6){
  for(z in 0:6){
    
    print(paste("Iteration:",i,j,z))
    #Run Neural network
    in_hid <-c(i)
    if(j != 0){
    in_hid <- append(in_hid,j)
    if(z != 0){
      in_hid <- append(in_hid,z)
    }
    }

    print(in_hid)
    nn <- neuralnet::neuralnet(formula = f_diabetes,
        data=pima, hidden=in_hid,err.fct = "ce",
        act.fct = "logistic",
        linear.output=FALSE)
    
    pred_results <- NULL
    pred_results <- as.numeric((0.5 < nn$net.result[[1]]))
    
    if(!is.na(pred_results[1])){
    # Calculate adjusted rand and class Error
    adjust <- adjustedRandIndex(pred_results, true_results)
    c_error <- classError(pred_results,true_results)
    new_results <- data.frame(adjust,c_error$errorRate,i,j,z)
    colnames(new_results) <- c("ARI","Error","Hid_1","Hid_2","Hid_3")
    results <- rbind(results,new_results) 
    }
    
  }
  }
}

# 6,5,2  
set.seed(10)
len_pima <- dim(pima)[1]
ind_train <- sample(1:len_pima, as.integer(len_pima*0.80))
train.pima <- pima[ind_train,]
test.pima <- pima[-ind_train,]
  
print(results)

f_diabetes <- formula(type ~ age + ped + bmi + skin + bp + glu + npreg)

nn <- neuralnet::neuralnet(formula = f_diabetes,
                           data=train.pima, hidden=c(7,2,1),err.fct = "ce",
                           #learningrate = 0.25,
                           act.fct = "logistic",
                           #algorithm = "backprop",
                           stepmax = 100000,
                           linear.output=FALSE)

true_results.train <- train.pima$type
pred_results <- NULL
pred_results <- as.numeric((0.5 < nn$net.result[[1]]))

# Calculate adjusted rand and class Error
tr_adjust <- adjustedRandIndex(pred_results, true_results.train)
tr_c_error <- classError(pred_results,true_results.train)


test_pred <- as.numeric(0.5 < compute(nn, test.pima[,-8])$net.result)
test_true <- test.pima$type

t_adjust <- adjustedRandIndex(test_pred, test_true)
t_c_error <- classError(test_pred, test_true)

# PCA =========================================================================== #


library(pgmm)
library(e1071)
pima <- rbind(Pima.tr,Pima.te)

pima_pca <- prcomp(pima[,-8], scale = TRUE)
pima_pca
summary(pima_pca)
pima$type <- as.numeric(pima$type == "Yes")
pima_pca_data <- cbind(pima$type,pima_pca$x)
colnames(pima_pca_data) <- c("type","PC1","PC2","PC3","PC4", "PC5", "PC6", "PC7")



# === T- Eigen with GBM  Directly ========================================================= #
#install.packages("teigen")

train.pima <- pima[ind_train,]
train.pima[,-8] <- scale(train.pima[,-8])
test.pima <- pima[-ind_train,]
test.pima[,-8] <- scale(test.pima[,-8])
test.pima <- as.data.frame(test.pima)

library(teigen)
model = teigen(train.pima[,-8], Gs=1:3, init="soft",parallel.cores=TRUE)
plot(model, what = "contour")
#plot(model, what = "uncertainty")
crossLabel <- as.numeric(as.factor(paste(train.pima$type,model$classification, sep = "")))

fuz <- model$fuzzy
colnames(fuz) <- c("Class_1","Class_2")

c_1 <- cbind(train.pima[model$classification == 1,],fuz[model$classification == 1, ])
c_2 <- cbind(train.pima[model$classification == 2,],fuz[model$classification == 2, ])

uncer_c1 <- 1 - apply(c_1[,9:10], 1, max)
uncer_c2 <- 1 - apply(c_2[,9:10],1,max)

plot(sort(uncer_c1),type = "l",col = 2)
plot(sort(uncer_c2),type = "l",col = 3)

# split train into two models 

t.p1 <- train.pima[model$classification == 1,]
t.p2 <- train.pima[model$classification == 2,]

f_diabetes <- formula(type ~ age + ped + bmi + skin + bp + glu + npreg )

# Component 1 
boost.p1 = gbm(f_diabetes,
               data= t.p1,
               distribution="bernoulli",
               n.trees=ntrees,
               interaction.depth=5)

y_hat = as.numeric( 0.5 < predict(boost.p1, newdata = t.p1, n.trees = ntrees,type=  "response"))
y_true = t.p1$type
table(y_hat, y_true)

# Component 2
boost.p2 = gbm(f_diabetes,
               data= t.p2,
               distribution="bernoulli",
               n.trees=ntrees,
               interaction.depth=5)

y_hat = as.numeric( 0.5 < predict(boost.p2, newdata = t.p2, n.trees = ntrees,type=  "response"))
y_true = t.p2$type
table(y_hat, y_true)

#Generate new Test 
new_test <- predict(model, newdata = test.pima[,-8])
tr.p1 <- test.pima[new_test$classification == 1,]
tr.p2 <- test.pima[new_test$classification == 2,]

y_hat_1 = as.numeric( 0.5 < predict(boost.p1, newdata = tr.p1, n.trees = ntrees,type=  "response"))
y_true_1 = tr.p1$type

y_hat_2 = as.numeric( 0.5 < predict(boost.p2, newdata = tr.p2, n.trees = ntrees,type=  "response"))
y_true_2 = tr.p2$type


table(c(y_hat_1,y_hat_2),c(y_true_1, y_true_2))

adjust <- adjustedRandIndex(c(y_hat_1,y_hat_2),c(y_true_1, y_true_2))
c_error <- classError(c(y_hat_1,y_hat_2),c(y_true_1, y_true_2))

print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))


# === T- Eigen with GBM  Indirectly ========================================================= #
#install.packages("teigen")

library(teigen)

train.pima <- pima[ind_train,]
train.pima[,-8] <- scale(train.pima[,-8])
test.pima <- pima[-ind_train,]
test.pima[,-8] <- scale(test.pima[,-8])
test.pima <- as.data.frame(test.pima)


model = teigen(train.pima[,-8], Gs=1:6, init="soft",parallel.cores=TRUE)
train.pima$t_lab <- model$classification


f_diabetes <- formula(type ~ age + ped + bmi + skin + bp + glu + npreg + t_lab)

boost.t = gbm(f_diabetes,
               data= train.pima,
               distribution="bernoulli",
               n.trees=ntrees,
               interaction.depth=5)

y_hat_1 = as.numeric( 0.5 < predict(boost.t, newdata = train.pima, n.trees = ntrees,type=  "response"))
y_true_1 = train.pima$type
table(y_hat_1, y_true_1)

test.pima <- pima[-ind_train,]
test.pima[,-8] <- scale(test.pima[,-8])
test.pima <- as.data.frame(test.pima)


new_test <- predict(model, newdata = test.pima[,-8])
test.pima$t_lab <- new_test$classification

y_hat_2 = as.numeric( 0.5 < predict(boost.t, newdata = test.pima, n.trees = ntrees,type=  "response"))
y_true_2 = test.pima$type
table(y_hat_2, y_true_2)

adjust <- adjustedRandIndex(y_hat_2,y_true_2)
c_error <- classError(y_hat_2, y_true_2)

print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))



# Boosted Trees =========================================================================== #

library(e1071)
library(randomForest)
library(MASS)
library(tree)
library(gbm)

train.pima <- pima[ind_train,]
train.pima[,-8] <- scale(train.pima[,-8])
test.pima <- pima[-ind_train,]
test.pima[,-8] <- scale(test.pima[,-8])
test.pima <- as.data.frame(test.pima)

set.seed(10)
ntrees <- 2000
  

f_diabetes <- formula(type ~ age + ped + bmi + skin + bp + glu + npreg )

boost.pima=gbm(f_diabetes,
               data= train.pima,
               distribution="bernoulli",
               n.trees=ntrees,
               interaction.depth=10)

sm.boost.pima <- summary(boost.pima)
y_hat = predict(boost.pima, newdata = train.pima,n.trees = ntrees,type=  "response")
pred_hat <- as.numeric(0.5 < y_hat)
true_train <- train.pima$type

table(pred_hat, true_train)

test_hat = as.numeric(0.5 < predict(boost.pima, newdata = test.pima,n.trees = ntrees,type=  "response"))
true_test = test.pima$type

table(test_hat, true_test)

adjust <- adjustedRandIndex(test_hat,true_test)
c_error <- classError(test_hat, true_test)

print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))

# PCA + Boosted Trees ========================================================================== #

# Generate PCA training and Test sets.
ntrees <- 800
len_pima <- dim(pima)[1]
ind_train <- sample(1:len_pima, as.integer(len_pima*0.80))
train.pima.pca <- as.data.frame(pima_pca_data[ind_train,])
test.pima.pca <-as.data.frame(pima_pca_data[-ind_train,])

cov_paste <- paste(colnames(train.pima.pca)[-1], collapse = "+")
pca_diabetes <- as.formula(paste("type ~" ,cov_paste))


boost.pima=gbm(pca_diabetes,
               data= train.pima.pca,
               distribution="bernoulli",
               n.trees=ntrees,
               interaction.depth=8)

sm.boost.pima <- summary(boost.pima)
y_hat = predict(boost.pima, newdata = train.pima.pca ,n.trees = ntrees,type=  "response")
pred_hat <- as.numeric(0.5 < y_hat)
true_train <- train.pima.pca[,1]

table(pred_hat, true_train)

test_hat = as.numeric(0.5 < predict(boost.pima, newdata = test.pima.pca,n.trees = ntrees,type=  "response"))
true_test = test.pima.pca$type

table(test_hat, true_test)

adjust <- adjustedRandIndex(test_hat,true_test)
c_error <- classError(test_hat, true_test)

print(paste("Adjusted Rand Index:",adjust, "Class Error",c_error$errorRate))
