data(fat,package="faraway")
fat = fat[,c(2,9:18)]
fat_ex1 <- fat[,2:11]
ridgefat <- data.frame(scale(fat[,1],center = T,scale = F),scale(fat_ex1,center = T,scale = T))#自变量标准化，因变量中心化
names(ridgefat) <- names(fat)
p <- dim(fat)[2]
cor(fat_ex1)
#pairs(fat_ex1,gap = 0,cex.labels = 0.9)#10个变量间各取两个的散点图
#kappa.default(fat_ex1)

#分为测试集与验证集探究实验结果：
set.seed(123)
index <- sample(2,nrow(ridgefat),replace = TRUE,prob = c(0.7,0.3))#设立种子并随机抽样，得到训练集与测试集
trainfat <- ridgefat[index==1,]
testfat <- ridgefat[index==2,]

require(glmnet)
trainy <- as.vector(trainfat[,1])
trainx <- as.matrix(trainfat[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
M_index_train <- which(trainy==1)
B_index_train <- which(trainy==0)
#交叉验证选择
cv.fit<-cv.glmnet(trainx,trainy,family="gaussian") 
plot(cv.fit)
cv.fit$lambda.min
cv.fit$lambda.1se
trainfit<-glmnet(trainx,trainy,family="gaussian")
plot(trainfit)
coefficients<-coef(trainfit,s=cv.fit$lambda.min)
coefficients <- coefficients[2:length(coefficients)]
Active.Index<-which(coefficients!=0)     #系数不为0的特征索引
Active.coefficients<-coefficients[Active.Index]   #系数不为0的特征系数值
#用选择的变量进行预测
newtrainx <- as.matrix(trainx[,Active.Index])
#建立logistic回归模型
lmfit_train <- lm(trainy~newtrainx,data = as.data.frame(cbind(newtrainx,trainy)))
summary(lmfit_train)

#预测变量正确率
pred.train <- predict.lm(lmfit_train,newdata = as.data.frame(cbind(newtrainx,trainy)),type = 'response')
pred.train

rmse <- function(x,y) sqrt(mean((x-y)^2))
rmse(trainfat$siri,pred.train)


#作用于测试集的结果：
testy <- as.vector(testfat[,1])
testx <- as.matrix(testfat[,2:p])#需要将数据集变为矩阵形式，否则报错无法运算
#用选择的变量进行预测
newtestx <- as.matrix(testx[,Active.Index])
#建立logistic回归模型
lmfit_test <- glm(testy~newtestx,data = as.data.frame(cbind(newtestx,testy)))
summary(lmfit_test)

#预测变量正确率
pred.test <- predict.lm(lmfit_test,newdata = as.data.frame(cbind(newtestx,testy)),type = 'response')
pred.test

rmse(testfat$siri,pred.test)


