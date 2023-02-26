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

##用msgps包计算Lasso解
require(msgps)
trainy <- as.vector(trainforest1[,1])
trainx <- as.matrix(trainforest1[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
M_index_train <- which(trainy==1)
B_index_train <- which(trainy==0)

alasso <- msgps(trainx,trainy,penalty="alasso",gamma=1,lambda=0)
summary(alasso)
plot(alasso)
predmsgps <- predict(alasso,trainx)
head(predmsgps)#取头部几个预测值展示出来

predi <- function(pred.for){
  pred.result <- rep(0,length(pred.for))
  for(i in 1:length(pred.for)){
    if(pred.for[i]>=0.5){ 
      pred.result[i] <- 1
    }
  }
  return(pred.result)
}

#用均方根误差衡量预测性能:
rmse <- function(x,y) sqrt(mean((x-y)^2))
#将预测值以0.5为划分依据转化为类别
predres1 <- predi(predmsgps[,1])
predres2 <- predi(predmsgps[,2])
predres3 <- predi(predmsgps[,3])
predres4 <- predi(predmsgps[,4])

#计算分类正确率的函数：
countright <- function(predres,trainy){
  pred.diff <- trainy-predres
  count_right <- tabulate(match(pred.diff,0))
  rate <- count_right/length(predres)
  count_right
  return(rate)
}

#计算不同准则下的分类正确率
rate1 <- countright(predres1,trainy)
rate2 <- countright(predres2,trainy)
rate3 <- countright(predres3,trainy)
rate4 <- countright(predres4,trainy)

testy <- as.vector(testforest1[,1])
testx <- as.matrix(testforest1[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
predmsgps <- predict(alasso,testx)
head(predmsgps)

#作用于测试集的运算结果：
predres1 <- predi(predmsgps[,1])
predres2 <- predi(predmsgps[,2])
predres3 <- predi(predmsgps[,3])
predres4 <- predi(predmsgps[,4])

#计算不同准则下的分类正确率
rate1 <- countright(predres1,testy)
rate2 <- countright(predres2,testy)
rate3 <- countright(predres3,testy)
rate4 <- countright(predres4,testy)






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

alasso <- msgps(trainx,trainy,penalty="alasso",gamma=1,lambda=0)
summary(alasso)
plot(alasso)
predmsgps <- predict(alasso,trainx)
head(predmsgps)#取头部几个预测值展示出来

predi <- function(pred.for){
  pred.result <- rep(0,length(pred.for))
  for(i in 1:length(pred.for)){
    if(pred.for[i]>=0.5){ 
      pred.result[i] <- 1
    }
  }
  return(pred.result)
}

#用均方根误差衡量预测性能:
rmse <- function(x,y) sqrt(mean((x-y)^2))
#将预测值以0.5为划分依据转化为类别
predres1 <- predi(predmsgps[,1])
predres2 <- predi(predmsgps[,2])
predres3 <- predi(predmsgps[,3])
predres4 <- predi(predmsgps[,4])

#计算分类正确率的函数：
countright <- function(predres,trainy){
  pred.diff <- trainy-predres
  count_right <- tabulate(match(pred.diff,0))
  rate <- count_right/length(predres)
  count_right
  return(rate)
}

#计算不同准则下的分类正确率
rate1 <- countright(predres1,trainy)
rate2 <- countright(predres2,trainy)
rate3 <- countright(predres3,trainy)
rate4 <- countright(predres4,trainy)

testy <- as.vector(testforest2[,1])
testx <- as.matrix(testforest2[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
predmsgps <- predict(alasso,testx)
head(predmsgps)

#作用于测试集的运算结果：
predres1 <- predi(predmsgps[,1])
predres2 <- predi(predmsgps[,2])
predres3 <- predi(predmsgps[,3])
predres4 <- predi(predmsgps[,4])

#计算不同准则下的分类正确率
rate1 <- countright(predres1,testy)
rate2 <- countright(predres2,testy)
rate3 <- countright(predres3,testy)
rate4 <- countright(predres4,testy)