### Final Project for 4740 R Code - Zack Downey, Jonathan Mac, Yi Zhu

setwd("C:/Users/Jonathan Mac/Desktop/Cornell AEM &BTRY Coursework/STSCI 4740/4740_Project")
divorce <- read.csv('divorce.csv', header=TRUE)
#fix(divorce)
attach(divorce)
#install.packages("Hmisc")
#library(Hmisc)
#describe(divorce)
Class <- factor(Class)
is.factor(Class)

### Quick correlation matrix output to see which parameters are good
p <- 54
k <- 5
folds <- sample (1:k,nrow(divorce),replace=TRUE)
for(j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  
  corr_matrix <- matrix(c(rep(1,p),rep(1,p)), ncol=2)
  colnames(corr_matrix) <- c("Atr","Correlation")
  
  for (i in 1:54){
    x <- c("Atr", i)
    y <- paste(x, collapse= "")
    correl = cor(train[ , i], train[ , 55])
    corr_matrix[i,2] <- correl
    corr_matrix[i,1] <- y
  }
  corr_matrix_sort <- corr_matrix[order(corr_matrix[,2]),]
  print(tail(corr_matrix_sort))
}

### Fold 1 = 11, 18, 40, 19, 17
### Fold 2 = 11, 18, 17, 19, 40
### Fold 3 = 11, 9, 17, 40, 19
### Fold 4 = 11, 19, 18, 17, 40
### Fold 5 = 11, 18, 19, 17, 40

# Top 5 parameters across folds are: 
# Atr40, Atr17, Atr19, Atr18, and Atr11


plot(Atr40, Class, main="Scatterplot of Atr40 vs. Class",
     xlab="Atr40", ylab="Class", pch=19)
### Interesting example - not a single person that put 3 or 4 for atr40
### fit into class 1.

set.seed(1)

# linear regression #
#1) Perform 5-fold CV to get mse
lin.mod.mse_vector = rep(0,k)

for(j in 1:k){
  set.seed(1)
  
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  lin.mod = lm(Class~., data=train)
  lin.mod_predict = predict(lin.mod, newdata = test)
  
  error = mean((test$Class - lin.modb6_predict)^2)
  lin.mod.mse_vector[j]=error
}
lin.mod.mse_vector
mean(lin.mod.mse_vector)

# RSS value of 0.0368 - 3.68% (not technically classification error)

### Because of issues with RSS vs. classification error, we use a simple
### 0.5 threshold to have the linear regression output choose a class value and
### we use that prediction to get a Test MSE similar to other methods.

# Linear regression - all parameters / classification threshold
lin.mod.class.mse_vector = rep(0,k)
for(j in 1:k){
  set.seed(1)
  
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  lin.mod = lm(Class~., data=train)
  lin.mod_predict = predict(lin.mod, newdata = test)
  for(i in 1:length(lin.mod_predict)){
    if(lin.mod_predict[i]>=0.5){
      lin.mod_predict[i]=1
    } else{
      lin.mod_predict[i]=0
    }
  }
  
  error = mean((test$Class - lin.mod_predict)^2)
  lin.mod.class.mse_vector[j]=error
}
lin.mod.class.mse_vector
mean(lin.mod.class.mse_vector)

# Test MSE of 0.0228 - 2.28%

# Basic polynomial linear regression with value of 2 - all parameters
log.mod.mse_vector = rep(0,k)

for(j in 1:k){
  set.seed(1)
  
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  xnam=paste("Atr",2:54,sep="")
  fmla=as.formula(paste("Class~poly(Atr1+",paste(xnam, collapse="+"),paste(",2,raw=T)")))
  log.mod=lm(fmla,data=train) #use raw polynomial x, x^2,x^3,...
  log.mod_predict = predict(log.mod, newdata = test)
  error = mean((test$Class - log.mod_predict)^2)
  log.mod.mse_vector[j]=error
}
log.mod.mse_vector
mean(log.mod.mse_vector)

# RSS value of 0.0157 - 1.57%

# Polynomial - value of 2 - all parameters / include classification threshold
log.mod.class.mse_vector = rep(0,k)
for(j in 1:k){
  set.seed(1)
  
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  xnam=paste("Atr",2:54,sep="")
  fmla=as.formula(paste("Class~poly(Atr1+",paste(xnam, collapse="+"),paste(",2,raw=T)")))
  log.mod=lm(fmla,data=train)
  log.mod_predict = predict(log.mod, newdata = test)
  for(i in 1:length(log.mod_predict)){
    if(log.mod_predict[i]>=0.5){
      log.mod_predict[i]=1
    } else{
      log.mod_predict[i]=0
    }
  }
  error = mean((test$Class - log.mod_predict)^2)
  log.mod.class.mse_vector[j]=error
}
log.mod.class.mse_vector
mean(log.mod.class.mse_vector)

# Test MSE / classification error of 0.0176 - 1.76%

# polynomial logistic regression #
log.mod.mse_vector1 = rep(0,k)

for(j in 1:k){
  set.seed(1)
  
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  xnam=paste("Atr",2:54,sep="")
  fmla=as.formula(paste("Class~poly(Atr1+",paste(xnam, collapse="+"),paste(",2,raw=T)")))
  log.mod=lm(fmla,data=train,family=binomial) #only difference: family=binomial
  log.mod_predict = predict(log.mod, newdata = test)
  error = mean((test$Class - log.mod_predict)^2)
  log.mod.mse_vector1[j]=error
}
log.mod.mse_vector1
mean(log.mod.mse_vector1)

# Test MSE / classification error of 0.0161 - 1.61%

### Perform basic LDA with all parameters
library(MASS)
lda.error=rep(0,k)
for (j in 1:k){
  train = divorce[folds!=j,]
  test = divorce[folds==j,]
  class.test = Class[folds==j]
  lda.fit=lda(as.factor(Class)~., data=train)
  lda.fit
  
  lda.pred=predict(lda.fit, test)
  lda.class=lda.pred$class
  #print(table(lda.class,class.test))
  mean(lda.class==class.test)
  
  topleft <- table(lda.class,class.test)[1]
  bottomleft <- table(lda.class,class.test)[2]
  topright <- table(lda.class,class.test)[3]
  bottomright <- table(lda.class,class.test)[4]
  
  lda.error[j]= (topright + bottomleft)/(topleft + bottomright + topright + bottomleft)
}
lda.error
mean(lda.error)

### Perform basic QDA with all parameters

### QDA with all parameters does not work because the design matrix
### would not have full rank - columns are linearly dependent of each
### other and thus the modelling cannot take place. To fix, we would want
### to remove columns that are linearly dependent - for simplicity we'll
### just limit our parameters to a handful later on in the analysis.


### Perform CV for k in KNN with all parameters
library(class)
knn.error=rep(0,k)
for (i in 1:k){
  train = divorce[folds!=i,]
  test = divorce[folds==i,]
  class.test = Class[folds==i]
  train.X = cbind(train[,c(1:54)])
  test.X = cbind(test[,c(1:54)])
  class.train = Class[folds!=i]
  
  k.error=rep(0,100)
  
  for(j in 1:100){
    knn.pred=knn(train.X,test.X,class.train,k=j)
    knn.class <- knn.pred
    #print(table(knn.class,class.test))
    
    topleft <- table(knn.class,class.test)[1]
    bottomleft <- table(knn.class,class.test)[2]
    topright <- table(knn.class,class.test)[3]
    bottomright <- table(knn.class,class.test)[4]
    
    error_rate <- (topright + bottomleft)/(topleft + bottomright + topright + bottomleft)
    k.error[j] <- error_rate
  }
  k.error
  min <- which.min(k.error)
  
  knn.pred=knn(train.X,test.X,class.train,k=min)
  knn.class <- knn.pred
  table(knn.class,class.test)
  
  error_rate <- (topright + bottomleft)/(topleft + bottomright + topright + bottomleft)
  knn.error[i] <- error_rate
  
}
knn.error
mean(knn.error)

### Basic Decision Tree (all 54 parameters)
library(tree)

tree.error=rep(0,k)
for (j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds==j,]
  class.test = Class[folds==j]
  
  tree.divorce=tree(as.factor(Class)~.,train)
  plot(tree.divorce)
  text(tree.divorce)
  tree.pred=predict(tree.divorce,test,type="class")
  tree.table=table(tree.pred,class.test)
  
  topleft <- tree.table[1]
  bottomleft <- tree.table[2]
  topright <- tree.table[3]
  bottomright <- tree.table[4]
  
  tree.error[j]= (topright + bottomleft)/(topleft + bottomright + topright + bottomleft)
}
tree.error
mean(tree.error)

# Test MSE of 0.0288 ~ 2.88%.

### Re-run LDA with only top 5 parameters
lda5.error=rep(0,k)
for (j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds==j,]
  class.test = Class[folds==j]
  lda.fit=lda(as.factor(Class)~Atr40+Atr17+Atr19+Atr18+Atr11, data=train)
  lda.fit
  
  lda.pred=predict(lda.fit, test)
  lda.class=lda.pred$class
  table(lda.class,class.test)
  mean(lda.class==class.test)
  
  topleft <- table(lda.class,class.test)[1]
  bottomleft <- table(lda.class,class.test)[2]
  topright <- table(lda.class,class.test)[3]
  bottomright <- table(lda.class,class.test)[4]
  
  lda5.error[j]= (topright + bottomleft)/(topleft + bottomright + topright + bottomleft)
}
lda5.error
mean(lda5.error)

# Test MSE of 0.0228 - 2.28%

### Re-run QDA with only top 5 parameters - should work now
qda5.error=rep(0,k)
for (j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds==j,]
  class.test = Class[folds==j]
  
  qda.fit=qda(as.factor(Class)~Atr40+Atr17+Atr19+Atr18+Atr11, data=train)
  qda.class=predict(qda.fit,test)$class
  print(table(qda.class,class.test))
  mean(qda.class==class.test)
  
  topleft <- table(qda.class,class.test)[1]
  bottomleft <- table(qda.class,class.test)[2]
  topright <- table(qda.class,class.test)[3]
  bottomright <- table(qda.class,class.test)[4]
  
  qda5.error[j]= (topright + bottomleft)/(topleft + bottomright + topright + bottomleft)
}
qda5.error
mean(qda5.error)

# A test MSE of 0.0176 - 1.76%

### CV for k with KNN with only top 5 parameters
knn5.error=rep(0,k)
for (i in 1:k){
  set.seed(1)
  train = divorce[folds!=i,]
  test = divorce[folds==i,]
  class.test = Class[folds==i]
  train.X = cbind(train[,c(11,17,18,19,40)])
  test.X = cbind(test[,c(11,17,18,19,40)])
  class.train = Class[folds!=i]
  
  k.error=rep(0,100)
  
  for(j in 1:100){
    knn.pred=knn(train.X,test.X,class.train,k=j)
    knn.class <- knn.pred
    table(knn.class,class.test)
    
    topleft <- table(knn.class,class.test)[1]
    bottomleft <- table(knn.class,class.test)[2]
    topright <- table(knn.class,class.test)[3]
    bottomright <- table(knn.class,class.test)[4]
    
    error_rate <- (topright + bottomleft)/(topleft + bottomright + topright + bottomleft)
    k.error[j] <- error_rate
  }
  k.error
  min <- which.min(k.error)
  print(min)
  
  knn.pred=knn(train.X,test.X,class.train,k=min)
  knn.class <- knn.pred
  table(knn.class,class.test)
  
  error_rate <- (topright + bottomleft)/(topleft + bottomright + topright + bottomleft)
  knn5.error[i] <- error_rate
  
}
knn5.error
mean(knn5.error)

# Test MSE of 0.324 - 32.4%

### Decision Tree with 5 parameters only
tree5.error=rep(0,k)
for (j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds==j,]
  class.test = Class[folds==j]
  
  tree5.divorce=tree(as.factor(Class)~.,train)
  plot(tree5.divorce)
  text(tree5.divorce)
  tree5.pred=predict(tree5.divorce,test,type="class")
  tree5.table=table(tree5.pred,class.test)
  
  topleft <- tree.table[1]
  bottomleft <- tree.table[2]
  topright <- tree.table[3]
  bottomright <- tree.table[4]
  
  tree5.error[j]= (topright + bottomleft)/(topleft + bottomright + topright + bottomleft)
}
tree5.error
mean(tree5.error)

# Test MSE of 0.0303 ~ 3.03%

### Formula set up for logistic regression + GAMs ###

xnam=paste("Atr",1:54,sep="")
fmla=as.formula(paste("Class~",paste(xnam, collapse="+")))
fmla

# ***** Logistic Regression ******

# First Step:
#fitting the model without using any subset selections
library(boot)
set.seed(1)
cv.logi_error1.5=rep(0,5)
for (i in 1:5){
  logi.fit=glm(Class~.,data=divorce,family='binomial')
  cv.logi_error1.5[i]=cv.glm(divorce,logi.fit, K=5)$delta[1]
}
cv.logi_error1.5
mean(cv.logi_error1.5)

# Classification error of 0.0388 - 3.88%

#without additing a control, the model does not converge
#try adding controls

cv.logiWC_error1.5=rep(0,5)
for (i in 1:5){
  logiWC.fit=glm(Class~., data=divorce, family='binomial',control=list(maxit=1000))
  cv.logiWC_error1.5[i]=cv.glm(divorce,logiWC.fit, K=5)$delta[1]
}
cv.logiWC_error1.5
mean(cv.logiWC_error1.5)

# Classification error of 0.0315 - 3.15%

# The model breaks even after adding additional control restraint
# even though the algorithm converges now, it still produces many warnings that 
# fitted probabilities numerically 0 or 1 occurred 
# the model has too many parameters and not enough observations, thus violates the one in ten rule
# the maximum likelihood estimation does not exist
# A complete or quasi-complete separation occurs


# ******* Natural Splines , Cubic Splines, Smoothing Splines *********
#not applicable for more than one predictors
#use GAM with splines instead


# ****** GAM Setup *******
library(gam)
newx=paste("s(Atr",1:54,',df=k)',sep="")
fmla2=as.formula(paste('I(Class==1)~',paste(newx, collapse="+")))
fmla2

gam.fit=gam(fmla2,family=binomial,data=divorce)
summary(gam.fit)
gam.error=rep(0,5)

# ************** GAM with Smoothing Splines with degree of freedom=5 ***************
for (j in 1:5){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds==j,]
  class.test = Class[folds==j]
  
  gam.fit=gam(fmla2,family=binomial,data=train)
  gam.pred=predict(gam.fit,test,type='response')
  conf_gam= table(gam.pred>.5,class.test)
  
  print(conf_gam)
  
  #Apply it to the test data and get the confusion matrix and error rate.
  gam.error[j]=1-sum(diag(conf_gam))/sum(conf_gam)
}
gam.error
mean(gam.error)

# Classification rate of 0.0287 - 2.87%

# **************** GAM with natural splines with degree of freedom = 5 **************
library(splines)
newx_ns=paste("ns(Atr",1:54,',df=k)',sep="")
fmla_ns=as.formula(paste('Class~',paste(newx_ns, collapse="+")))
fmla_ns

#gam.fit2=gam(fmla_ns,divorce,family=binomial)
#encountered major error when using natural splines
# possible explanation:
#https://stat.ethz.ch/pipermail/r-devel/2011-May/061035.html
#natural splines are designed for continuous data

## Code for best subset selection within k-fold for linear regression ##
set.seed(1)
val <- 8 #max preds for bestsubset
best.lin.cv.errors = matrix (NA,k,val, dimnames =list(NULL , paste (1:val) ))

#install.packages("leaps")
library(leaps)

# predict function for best subset selection #
predict.regsubsets = function(object,newdata,id,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

for(j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  best.fit=regsubsets(Class~.,data=train,nvmax=val,really.big=T)
  for(i in 1:val){
    pred=predict(best.fit,test,id=i)
    for(m in 1:length(pred)){
      if(pred[m]>=0.5){
        pred[m]=1
      } else{
        pred[m]=0
      }
    }
    best.lin.cv.errors[j,i]= mean((test$Class-pred)^2)
  }
}
best.lin.cv.errors
mean.best.lin.cv.errors=apply(best.lin.cv.errors ,2, mean)
mean.best.lin.cv.errors
x = which.min(mean.best.lin.cv.errors)

reg.best=regsubsets(Class~.,data=divorce , nvmax=val,really.big=T)
coef(reg.best,x)
### atr6, atr18, atr40

## Code for forward selection within k-fold for linear regression ##
set.seed(1)
val <- 54 #max preds for bestsubset
fwd.lin.cv.errors = matrix (NA,k,val, dimnames =list(NULL , paste (1:val) ))

#install.packages("leaps")
library(leaps)

for(j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  regfit.fwd.fit=regsubsets(Class~.,data=train,nvmax=val,method="forward")
  for(i in 1:val){
    pred=predict(regfit.fwd.fit,test,id=i)
    
    for(m in 1:length(pred)){
      if(pred[m]>=0.5){
        pred[m]=1.0
      } else{
        pred[m]=0.0
      }
    }
    
    fwd.lin.cv.errors[j,i]= mean((test$Class-pred)^2)
  }
}
fwd.lin.cv.errors

mean.fwd.lin.cv.errors=apply(fwd.lin.cv.errors ,2, mean)
mean.fwd.lin.cv.errors
mean(mean.fwd.lin.cv.errors)
x = which.min(mean.fwd.lin.cv.errors)

reg.best=regsubsets(Class~.,data=divorce , nvmax=val,really.big=T,method="forward")
coef(reg.best,x)

## atr 6, atr18, atr40

## Code for backward selection within k-fold for linear regression ##
set.seed(1)
val <- 54 #max preds for bestsubset
bwd.lin.cv.errors = matrix (NA,k,val, dimnames =list(NULL , paste (1:val) ))

#install.packages("leaps")
library(leaps)

for(j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  regfit.fwd.fit=regsubsets(Class~.,data=train,nvmax=val,method="backward")
  for(i in 1:val){
    pred=predict(regfit.fwd.fit,test,id=i)
    for(m in 1:length(pred)){
      if(pred[m]>=0.5){
        pred[m]=1
      } else{
        pred[m]=0
      }
    }
    bwd.lin.cv.errors[j,i]= mean((test$Class-pred)^2)
  }
}
bwd.lin.cv.errors

mean.bwd.lin.cv.errors=apply(bwd.lin.cv.errors ,2, mean)
mean.bwd.lin.cv.errors
mean(mean.bwd.lin.cv.errors)
x = which.min(mean.bwd.lin.cv.errors)

reg.best=regsubsets(Class~.,data=divorce , nvmax=val,really.big=T,method="backward")
coef(reg.best,x)

## atr17, atr26 atr40

## Code for lasso with linear ##
library(glmnet)
lasso.lin.cv.errors = rep(NA,k)

for(j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  x=model.matrix(Class~.,train)[,-1]  #remove the intercept (training)
  y=train[,dim(divorce)[2]] #response Class (training)
  
  cv.out=cv.glmnet(x,y,alpha=1)
  bestlam=cv.out$lambda.min #get optimal tuning parameter (lambda)
  
  lasso.mod=glmnet(x,y,alpha=1,lambda=bestlam)
  newx = model.matrix(Class~.,test )[,-1] #remove the intercept (test)
  newy = test[,dim(divorce)[2]] #response Class (test)
  pred.lasso = predict(lasso.mod, s = bestlam, newx = newx) #predict
  
  for(m in 1:length(pred.lasso)){
    if(pred.lasso[m]>=0.5){
      pred.lasso[m]=1
    } else{
      pred.lasso[m]=0
    }
  }
  
  #find MSE of lasso on this kth fold
  error = mean((newy - pred.lasso)^2)
  error
  
  #append
  lasso.lin.cv.errors[j] = error
  
  #to see the coefs of the kth-fold's lowest MSE
  lasso.coef=coef(lasso.mod)[,1]
  print(lasso.coef[lasso.coef!=0])
  
  
}
lasso.lin.cv.errors

## Code for lasso with logistic##
lasso.log.cv.errors = rep(NA,k)

for(j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce[folds ==j,]
  
  x=model.matrix(Class~.,train)[,-1]  #remove the intercept (training)
  y=train[,dim(divorce)[2]] #response Class (training)
  cv.out=cv.glmnet(x,y,alpha=1)
  bestlam=cv.out$lambda.min #get optimal tuning parameter (lambda)
  
  lasso.mod=glmnet(x,y,alpha=1,lambda=bestlam, family="binomial")
  newx = model.matrix(Class~.,test)[,-1] #remove the intercept (test)
  newy = test[,dim(divorce)[2]] #response Class (test)
  pred.lasso = predict(lasso.mod, s = bestlam, newx = newx, type = "response") #predict
  pred.lasso[pred.lasso>=.5]=1
  pred.lasso[pred.lasso<.5]=0
  print(pred.lasso)
  print(newy)
  
  #find MSE of lasso on this kth fold
  error = mean((newy != pred.lasso)^2)
  error
  
  #append
  lasso.log.cv.errors[j] = error
  
  #to see the coefs of the kth-fold's lowest MSE
  lasso.coef=coef(lasso.mod)[,1]
  print(lasso.coef[lasso.coef!=0])
  
  
}
lasso.log.cv.errors

### Compare all methods? Assign test MSE to variables

# Linear regression with all 54 parameters - RSS
mean(lin.mod.mse_vector)
# Linear regression with all 54 parameters - MSE / Class Error
mean(lin.mod.class.mse_vector)
# Polynomial regression (exponent of 2) with all 54 parameters - RSS
mean(log.mod.mse_vector)
# Polynomial regression (exponent of 2) with all 54 parameters - MSE / Class Error
mean(log.mod.class.mse_vector)
# Polynomial logistic regression (exponent of 2) with all 54 parameters
mean(log.mod.mse_vector1)
# LDA with all 54 parameters
mean(lda.error)
# KNN with all 54 parameters (CV for value of K in 5-fold)
mean(knn.error)
# Decision tree with all 54 parameters
mean(tree.error)
# LDA with only top 5 correlated parameters
mean(lda5.error)
# QDA with only top 5 correlated parameters
mean(qda5.error)
# KNN with only top 5 correlated parameters (CV for value of k in 5-fold)
mean(knn5.error)
# Decision tree with only top 5 correlated parameters
mean(tree5.error)
# Logistic regression with all 54 parameters
mean(cv.logi_error1.5)
# Logistic regression (with control) with all 54 parameters
mean(cv.logiWC_error1.5)
# GAM w/ Smoothing Splines and 5 df
mean(gam.error)
# Linear regression with best subset selection
mean(mean.best.lin.cv.errors)
# Linear regression with forward selection
mean(mean.fwd.lin.cv.errors)
# Linear regression with backward selection
mean(mean.bwd.lin.cv.errors)
# Linear regression with lasso
mean(lasso.lin.cv.errors)
# Logistic regression with lasso
mean(lasso.log.cv.errors)


### Fun testing set - applied to group members!

# For this analysis, one of the team members took the test along with
# his significant other. The following code predicts, based on the top
# model (no CV), if they would theoretically get a divorce or not.

divorce_test <- read.csv('C:/Users/14014/Documents/Cornell_Fall_2019/STSCI_4740/STSCI_4740_FinalProject/divorce_fun_test.csv', header=TRUE)
attach(divorce_test)
#View(divorce_test)

# With QDA and 5 parameters only
for (j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce_test
  
  qda.fit=qda(as.factor(Class)~Atr40+Atr17+Atr19+Atr18+Atr11, data=train)
  qda.class=predict(qda.fit,test)$class
  print(qda.class)
}

# With Decision Tree with only 5 parameters
for (j in 1:k){
  set.seed(1)
  train = divorce[folds!=j,]
  test = divorce_test
  
  tree5.divorce=tree(as.factor(Class)~.,train)
  tree5.pred=predict(tree5.divorce,test,type="class")
  print(tree5.pred)
}

