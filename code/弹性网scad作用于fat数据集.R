data(fat,package="faraway")
fat = fat[,c(1,9:18)]
fat_ex1 <- fat[,2:11]
ridgefat <- data.frame(scale(fat[,1],center = T,scale = F),scale(fat_ex1,center = T,scale = T))#自变量标准化，因变量中心化
names(ridgefat) <- names(fat)
cor(fat_ex1)
#pairs(fat_ex1,gap = 0,cex.labels = 0.9)#10个变量间各取两个的散点图
#kappa.default(fat_ex1)

#分为测试集与验证集探究实验结果：
set.seed(123)
index <- sample(2,nrow(ridgefat),replace = TRUE,prob = c(0.7,0.3))#设立种子并随机抽样，得到训练集与测试集
trainfat <- ridgefat[index==1,]
testfat <- ridgefat[index==2,]

require(msaenet)
trainy <- trainfat$brozek
trainx <- as.matrix(trainfat[,-1])
#选择较好的cv值：
asnetfit <- asnet(trainx, trainy)
summary(asnetfit)
plot(asnetfit)
coef(asnetfit)
predasnet <- predict(asnetfit,trainx)
#用均方根误差衡量预测性能:
rmse <- function(x,y) sqrt(mean((x-y)^2))
rmse(trainfat$brozek,predasnet)

testx <- as.matrix(testfat[,-1])
predasnet <- predict(asnetfit,testx)
rmse(testfat$brozek,predasnet)





