s<-read.table('C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\大报告\\dataset\\乳腺癌数据集\\wpbc.data.csv',sep = ",")
s <- s[-1,]
y <- as.numeric(s[,2])
nametri <- s[,1]
s <- s[,-dim(s)[2]]
s <- as.matrix(s)
x <- as.data.frame(apply(s[,3:dim(s)[2]],2,as.numeric))
wpbc <- as.data.frame(cbind(y,x))
p <- dim(wpbc)[2]
#cor(x_std)#计算相关系数矩阵

#分为测试集与验证集探究实验结果：
set.seed(1234)
index <- sample(2,nrow(wpbc),replace = TRUE,prob = c(0.7,0.3))#设立种子并随机抽样，得到训练集与测试集
trainwpbc <- wpbc[index==1,]
testwpbc <- wpbc[index==2,]

##用lars包(最小角回归)计算Lasso解
require(lars)
trainy <- as.vector(trainwpbc[,1])
trainx <- as.matrix(trainwpbc[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
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

newtrainx <- as.matrix(trainx[,active.index])
#建立logistic回归模型
glmfit_train <- glm(trainy~newtrainx,data = as.data.frame(cbind(newtrainx,trainy)),family = binomial(link = 'logit'))
summary(glmfit_train)

#预测变量正确率
pred.train <- predict.glm(glmfit_train,newdata = as.data.frame(cbind(newtrainx,trainy)),type = 'response')
pred.train

predi <- function(pred.for){
  pred.result <- rep(0,length(pred.for))
  for(i in 1:length(pred.for)){
    if(pred.for[i]>=0.5){ 
      pred.result[i] <- 1
    }
  }
  return(pred.result)
}

train.result <- predi(pred.train)

pred.diff <- trainy-train.result
count_right <- tabulate(match(pred.diff,0))
rate <- count_right/length(train.result)
count_right
rate

testy <- as.vector(testwpbc[,1])
testx <- as.matrix(testwpbc[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
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

newtestx <- as.matrix(testx[,active.index])
#建立logistic回归模型
glmfit_test <- glm(testy~newtestx,data = as.data.frame(cbind(newtestx,testy)),family = binomial(link = 'logit'))
summary(glmfit_test)

#预测变量正确率
pred.test <- predict.glm(glmfit_test,newdata = as.data.frame(cbind(newtestx,testy)),type = 'response')
pred.test

test.result <- predi(pred.test)

#作用于测试集正确率：
pred.diff <- testy-test.result
count_right <- tabulate(match(pred.diff,0))
rate <- count_right/length(test.result)
count_right
rate