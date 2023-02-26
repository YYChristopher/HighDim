s<-read.table('C:\\Users\\user\\Documents\\统计实践与高维\\高维数据推断\\作业\\大报告\\dataset\\乳腺癌数据集\\wpbc.data.csv',sep = ",")
s <- s[-1,]
s <- s[,-35]
y <- as.numeric(s[,2])
nametri <- s[,1]
s <- as.matrix(s)
x <- as.data.frame(apply(s[,3:dim(s)[2]],2,as.numeric))