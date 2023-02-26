library(DMwR2)#改包中的函数可以用来处理缺失值
s<-read.table('C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\大报告\\dataset\\PersonGaitDataSet.csv',sep = ",")
y <- as.numeric(s[,dim(s)[2]])
s <- as.matrix(s,nrow=48)
s <- centralImputation(s)
x <- as.data.frame(apply(s[,1:dim(s)[2]-1],2,as.numeric))
#用中心值趋势处理缺失值
x <- centralImputation(x)
pgds <- as.data.frame(cbind(y,x))
p <- dim(pgds)[2]

#分为测试集与验证集探究实验结果：
set.seed(1234)
index <- sample(2,nrow(pgds),replace = TRUE,prob = c(0.7,0.3))#设立种子并随机抽样，得到训练集与测试集
trainpgds <- pgds[index==1,]
testpgds <- pgds[index==2,]

##用lars包(最小角回归)计算Lasso解
require(lars)
trainy <- as.vector(trainpgds[,1])
trainx <- as.matrix(trainpgds[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
M_index_train <- which(trainy==1)
B_index_train <- which(trainy==0)
lassomod <- lars(trainx,trainy,type = "lasso")#用lasso建模
#Lasso图绘制:
plot(lassomod)

#交叉验证选择压缩程度t值:
set.seed(12345)
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

testy <- as.vector(testpgds[,1])
testx <- as.matrix(testpgds[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
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


