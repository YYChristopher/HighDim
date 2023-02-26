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

require(msgps)
trainy <- trainfat$siri
trainx <- as.matrix(trainfat[,-1])
#自适应Lasso建模：
elasticnet <- msgps(trainx,trainy,penalty="enet",alpha = 0.5)
summary(elasticnet)
plot(elasticnet)
predmsgps <- predict(elasticnet,trainx)
head(predmsgps)#取头部几个预测值展示出来
#用均方根误差衡量预测性能:
rmse <- function(x,y) sqrt(mean((x-y)^2))
rmse(trainfat$siri,predmsgps[,1])
rmse(trainfat$siri,predmsgps[,2])
rmse(trainfat$siri,predmsgps[,3])
rmse(trainfat$siri,predmsgps[,4])

testx <- as.matrix(testfat[,-1])
predmsgps <- predict(elasticnet,testx)
head(predmsgps)
rmse(testfat$siri,predmsgps[,1])
rmse(testfat$siri,predmsgps[,2])
rmse(testfat$siri,predmsgps[,3])
rmse(testfat$siri,predmsgps[,4])