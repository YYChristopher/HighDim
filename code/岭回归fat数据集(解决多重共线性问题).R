data(fat,package="faraway")
fat = fat[,c(2,9:18)]
fat_ex1 <- fat[,2:11]
cor(fat_ex1)
pairs(fat_ex1,gap = 0,cex.labels = 0.9)#10个变量间各取两个的散点图
kappa.default(fat_ex1)
require(faraway)
vif(lm(siri~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data = fat))#采用方差膨胀因子检验多重共线性
ridgefat <- data.frame(scale(fat[,1],center = T,scale = F),scale(fat_ex1,center = T,scale = T))#自变量标准化，因变量中心化
names(ridgefat) <- names(fat)

#绘制出岭迹图：
library(MASS)
##以0.01为间隔生成Lambda的序列建立出岭回归模型
rgmod <- lm.ridge(siri~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data = ridgefat,lambda = seq(0,1,0.01))
matplot(rgmod$lambda,coef(rgmod),type = "l",xlab = expression(lambda),ylab = expression(hat(beta)),col = 2)
##用select函数计算出根据几个统计量得到的lambda值
select(rgmod)
##用广义交叉验证方法(GCV)确定lambda的值：
which.min(rgmod$GCV)
abline(v=0.65)
##取lambda=0.65进行岭回归估计
lm.ridge(siri~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data = ridgefat,lambda = 0.65)
##一般的最小二乘估计结果：
round(coef(lm(siri~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data = ridgefat)),2)

#分为测试集与验证集探究实验结果：
set.seed(123)
index <- sample(2,nrow(ridgefat),replace = TRUE,prob = c(0.7,0.3))#设立种子并随机抽样，得到训练集与测试集
trainfat <- ridgefat[index==1,]
testfat <- ridgefat[index==2,]

#最小二乘拟合结果：
lmod <- lm(siri~.,trainfat)
summary(lmod)$r.squared

#用均方根误差衡量预测性能
rmse <- function(x,y) sqrt(mean((x-y)^2))
rmse(fitted(lmod),trainfat$siri)
rmse(predict(lmod,testfat),testfat$siri)

#对训练集进行岭回归：
rgmodt <- lm.ridge(siri~.,trainfat,lambda = seq(0,1,len = 50))
which.min(rgmodt$GCV)
ypred <- cbind(1,as.matrix(trainfat[,-1]))%*%coef(rgmodt)[33,]#第33行对应的lambda值约为0.65左右
rmse(ypred,trainfat$siri)
ypred <- cbind(1,as.matrix(testfat[,-1]))%*%coef(rgmodt)[33,]
rmse(ypred,testfat$siri)

data.frame(ypred,testfat$siri)





