s<-read.table('C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\大报告\\dataset\\乳腺癌数据集\\wdbc.data.csv',sep = ",")
s <- s[-1,]
y <- as.numeric(s[,2])
nametri <- s[,1]
s <- as.matrix(s)
x <- as.data.frame(apply(s[,3:dim(s)[2]],2,as.numeric))
wdbc <- as.data.frame(cbind(y,x))
p <- dim(wdbc)[2]
#cor(x_std)#计算相关系数矩阵

#分为测试集与验证集探究实验结果：
set.seed(1234)
index <- sample(2,nrow(wdbc),replace = TRUE,prob = c(0.7,0.3))#设立种子并随机抽样，得到训练集与测试集
trainwdbc <- wdbc[index==1,]
testwdbc <- wdbc[index==2,]

##用msgps包计算Lasso解
require(msgps)
trainy <- as.vector(trainwdbc[,1])
trainx <- as.matrix(trainwdbc[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
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

testy <- as.vector(testwdbc[,1])
testx <- as.matrix(testwdbc[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
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