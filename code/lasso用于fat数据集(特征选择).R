data(fat,package="faraway")
fat = fat[,c(2,9:18)]
fat_ex1 <- fat[,2:11]
ridgefat <- data.frame(scale(fat[,1],center = T,scale = F),scale(fat_ex1,center = T,scale = T))#自变量标准化，因变量中心化
names(ridgefat) <- names(fat)
cor(fat_ex1)
pairs(fat_ex1,gap = 0,cex.labels = 0.9)#10个变量间各取两个的散点图
kappa.default(fat_ex1)

#分为测试集与验证集探究实验结果：
set.seed(123)
index <- sample(2,nrow(ridgefat),replace = TRUE,prob = c(0.7,0.3))#设立种子并随机抽样，得到训练集与测试集
trainfat <- ridgefat[index==1,]
testfat <- ridgefat[index==2,]

##用lars包(最小角回归)计算Lasso解
require(lars)
trainy <- trainfat$siri
trainx <- as.matrix(trainfat[,-1])
lassomod <- lars(trainx,trainy)#用lasso建模
#Lasso图绘制:
plot(lassomod)

#交叉验证选择压缩程度t值:
set.seed(345)
tcv <- cv.lars(trainx,trainy)
tcv$index[which.min(tcv$cv)]
#lasso的拟合系数结果:
round(predict(lassomod,s=tcv$index[which.min(tcv$cv)],type = "coef",mode = "fraction")$coef,3)
#一般ols的系数结果:
round(coef(lm(siri~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data = trainfat)),3)
ols <- lm(siri~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data = trainfat)


trainx <- as.matrix(trainfat[,-1])
predlars <- predict(lassomod,trainx,tcv$index[which.min(tcv$cv)],mode = "fraction")
#用均方根误差衡量预测性能:
rmse <- function(x,y) sqrt(mean((x-y)^2))
rmse(trainfat$siri,predlars$fit)
testx <- as.matrix(testfat[,-1])
predlars <- predict(lassomod,testx,tcv$index[which.min(tcv$cv)],mode="fraction")
rmse(testfat$siri,predlars$fit)
predols <- predict(ols,newdata = as.data.frame(testfat))
rmse(testfat$siri,predols)

#观察几个自变量用于预测因变量：
predlars <- predict(lassomod,s=tcv$index[which.min(tcv$cv)],type="coef",mode="fraction")
plot(predlars$coef,type="h",ylab="Coefficient")
abline(h=0,lty=3)
sum(predlars$coef!=0)






