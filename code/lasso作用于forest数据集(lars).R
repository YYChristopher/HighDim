s<-read.table('C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\充分降维\\数据集\\Algerian_forest_fires_dataset_UPDATE2.csv',sep = ",")
path="C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\充分降维\\code"
setwd(path)
s <- as.matrix(s[2:122,])
s <- s[,4:dim(s)[2]]
n <- dim(s)[1]
p <- dim(s)[2]#取出数据表的维数
s <- apply(s, 2, as.numeric)
x <- s[,1:(p-1)]
y <- s[,p]
forest1 <- as.data.frame(cbind(y,x))
p <- dim(forest1)[2]

#分为测试集与验证集探究实验结果：
set.seed(1234)
index <- sample(2,nrow(forest1),replace = TRUE,prob = c(0.7,0.3))#设立种子并随机抽样，得到训练集与测试集
trainforest1 <- forest1[index==1,]
testforest1 <- forest1[index==2,]

##用lars包(最小角回归)计算Lasso解
require(lars)
trainy <- as.vector(trainforest1[,1])
trainx <- as.matrix(trainforest1[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
M_index_train <- which(trainy==1)
B_index_train <- which(trainy==0)
lassomod <- lars(trainx,trainy,type = "lasso")#用lasso建模
#Lasso图绘制:
plot(lassomod)

#交叉验证选择压缩程度t值:
set.seed(1234)
tcv <- cv.lars(trainx,trainy)
tcv$index[which.min(tcv$cv)]
#lasso的拟合系数结果:
trainfit <- round(predict(lassomod,s=tcv$index[which.min(tcv$cv)],type = "coef",mode = "fraction")$coef,3)
active.index <- which(trainfit!=0)

predlars <- predict(lassomod,trainx,tcv$index[which.min(tcv$cv)],mode = "fraction")
y_pred_train <- rep(0,length(predlars$fit))
for(i in 1:length(predlars$fit)){
  if(predlars$fit[i]>0){
    y_pred_train[i] <- 1
  }
  else{
    y_pred_train[i] <- 0
  }
}

rmse <- function(x,y) (((x-y)^2))
wrong_num <- sum(rmse(y_pred_train,trainy))
rightrate_train <- (length(trainy)-wrong_num)/length(trainy)

testy <- as.vector(testforest1[,1])
testx <- as.matrix(testforest1[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
predlars <- predict(lassomod,testx,tcv$index[which.min(tcv$cv)],mode="fraction")
y_pred_test <- rep(0,length(predlars$fit))
for(i in 1:length(predlars$fit)){
  if(predlars$fit[i]>0){
    y_pred_test[i] <- 1
  }
  else{
    y_pred_test[i] <- 0
  }
}

rmse <- function(x,y) (((x-y)^2))
wrong_num <- sum(rmse(y_pred_test,testy))
rightrate_test <- (length(testy)-wrong_num)/length(testy)





s<-read.table('C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\充分降维\\数据集\\Algerian_forest_fires_dataset_UPDATE2.csv',sep = ",")
s <- as.matrix(s[127:248,])
s <- s[,4:dim(s)[2]]
n <- dim(s)[1]
p <- dim(s)[2]#取出数据表的维数
s <- apply(s, 2, as.numeric)
x <- s[,1:(p-1)]
y <- s[,p]
forest2 <- as.data.frame(cbind(y,x))
p <- dim(forest2)[2]

#分为测试集与验证集探究实验结果：
set.seed(1234)
index <- sample(2,nrow(forest2),replace = TRUE,prob = c(0.7,0.3))#设立种子并随机抽样，得到训练集与测试集
trainforest2 <- forest2[index==1,]
testforest2 <- forest2[index==2,]


trainy <- as.vector(trainforest2[,1])
trainx <- as.matrix(trainforest2[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
M_index_train <- which(trainy==1)
B_index_train <- which(trainy==0)
lassomod <- lars(trainx,trainy,type = "lasso")#用lasso建模
#Lasso图绘制:
plot(lassomod)

#交叉验证选择压缩程度t值:
set.seed(1234)
tcv <- cv.lars(trainx,trainy)
tcv$index[which.min(tcv$cv)]
#lasso的拟合系数结果:
trainfit <- round(predict(lassomod,s=tcv$index[which.min(tcv$cv)],type = "coef",mode = "fraction")$coef,3)
active.index <- which(trainfit!=0)

predlars <- predict(lassomod,trainx,tcv$index[which.min(tcv$cv)],mode = "fraction")
y_pred_train <- rep(0,length(predlars$fit))
for(i in 1:length(predlars$fit)){
  if(predlars$fit[i]>0){
    y_pred_train[i] <- 1
  }
  else{
    y_pred_train[i] <- 0
  }
}

rmse <- function(x,y) (((x-y)^2))
wrong_num <- sum(rmse(y_pred_train,trainy))
rightrate_train <- (length(trainy)-wrong_num)/length(trainy)

testy <- as.vector(testforest2[,1])
testx <- as.matrix(testforest2[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
predlars <- predict(lassomod,testx,tcv$index[which.min(tcv$cv)],mode="fraction")
y_pred_test <- rep(0,length(predlars$fit))
for(i in 1:length(predlars$fit)){
  if(predlars$fit[i]>0){
    y_pred_test[i] <- 1
  }
  else{
    y_pred_test[i] <- 0
  }
}

rmse <- function(x,y) (((x-y)^2))
wrong_num <- sum(rmse(y_pred_test,testy))
rightrate_test <- (length(testy)-wrong_num)/length(testy)