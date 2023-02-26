s<-read.table('C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\充分降维\\数据集\\Algerian_forest_fires_dataset_UPDATE2.csv',sep = ",")
path="C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\充分降维\\code"
setwd(path)
source('sequential_test_SAVE.R')
source('sequential_test_SIR.R')
source('SAVE_func.R')
source('SIR_func.R')
s <- as.matrix(s[2:122,])
s <- s[,4:dim(s)[2]]
n <- dim(s)[1]
p <- dim(s)[2]#取出数据表的维数
s <- apply(s, 2, as.numeric)
x <- s[,1:(p-1)]
y <- s[,p]
h <- 20
for(r in 1:(p-1)){
  testresult <- seqtestsave(x,y,h,r,"continuous")
  print(testresult)
}
r <- 4
savey <- save(x,y,h,r,"continuous")
xnew <- x%*%savey
plot(xnew,y,xlab="x轴",ylab="y轴")

fit_for <- glm(y~xnew,data = as.data.frame(cbind(xnew,y)),family = binomial(link = 'logit'))
summary(fit_for)

pred.for <- predict.glm(fit_for,newdata = as.data.frame(cbind(xnew,y)),type = 'response')
pred.for

pred.result <- rep(0,length(pred.for))
for(i in 1:length(pred.for)){
  if(pred.for[i]>=0.5){ 
    pred.result[i] <- 1
  }
}
pred.diff <- y-pred.result
count_right <- tabulate(match(pred.diff,0))
rate <- count_right/length(pred.for)
count_right
rate




s<-read.table('C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\充分降维\\数据集\\Algerian_forest_fires_dataset_UPDATE2.csv',sep = ",")
s <- as.matrix(s[127:248,])
s <- s[,4:dim(s)[2]]
n <- dim(s)[1]
p <- dim(s)[2]#取出数据表的维数
s <- apply(s, 2, as.numeric)
x <- s[,1:(p-1)]
y <- s[,p]
h <- 20
for(r in 1:(p-1)){
  testresult <- seqtestsave(x,y,h,r,"continuous")
  print(testresult)
}
r <- 4
savey <- save(x,y,h,r,"continuous")
xnew <- x%*%savey
plot(xnew,y,xlab="x轴",ylab="y轴")

fit_for <- glm(y~xnew,data = as.data.frame(cbind(xnew,y)),family = binomial(link = 'logit'))
summary(fit_for)

pred.for <- predict.glm(fit_for,newdata = as.data.frame(cbind(xnew,y)),type = 'response')
pred.for

pred.result <- rep(0,length(pred.for))
for(i in 1:length(pred.for)){
  if(pred.for[i]>=0.5){ 
    pred.result[i] <- 1
  }
}
pred.diff <- y-pred.result
count_right <- tabulate(match(pred.diff,0))
rate <- count_right/length(pred.for)
count_right
rate