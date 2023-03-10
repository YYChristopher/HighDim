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

##用ncvreg包计算Lasso解
require(ncvreg)
trainy <- as.vector(trainwdbc[,1])
trainx <- as.matrix(trainwdbc[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
M_index_train <- which(trainy==1)
B_index_train <- which(trainy==0)
#交叉验证选择超参lambda
cv.fit<-cv.ncvreg(trainx,trainy) 
plot(cv.fit)
coef(cv.fit)
cv.fit$lambda.min

trainfit<-ncvreg(trainx,trainy,penalty = "SCAD")
plot(trainfit)
coefficients<-coef(cv.fit)
coefficients <- coefficients[2:length(coefficients)]
Active.Index<-which(coefficients!=0)     #系数不为0的特征索引
Active.coefficients<-coefficients[Active.Index]   #系数不为0的特征系数值
#用选择的变量进行预测
newtrainx <- as.matrix(trainx[,Active.Index])
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

#作用于测试集的结果：
testy <- as.vector(testwdbc[,1])
testx <- as.matrix(testwdbc[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
#用选择的变量进行预测
newtestx <- as.matrix(testx[,Active.Index])
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