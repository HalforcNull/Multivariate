tmp <- read.csv('Digits_train.csv')
#head(tmp)


for(i in 0:9)
{
  Data <- tmp[tmp[,1]==i,-1]
  my.mean <- sapply(Data, function(x) mean(x))
  my.mean.matrix <- matrix(my.mean[2:785], nrow=28,ncol=28, byrow = FALSE)
  my.mean.matrix <- my.mean.matrix[,28:1]
  my.cov <- by(Data, Data[,1], cov)
  if( i == 8){
    mean8 <- my.mean
    cov8 <- my.cov
  }
  
  image(my.mean.matrix)
}     


Data.8 <- tmp[tmp[,1]==8,-1]
indice.8 <- tmp[tmp[,1]==8,1]


#my.matrix <- matrix(Data.8, nrow=28,ncol=28, byrow = FALSE, dimnames = FALSE)
#my.matrix <- my.matrix[,28:1]
mean.8 <-  sapply(Data.8, function(x) mean(x))
cov.8 <- cov(Data.8)
eigen.8 <- eigen(cov.8)

my.value <- eigen.8$value
my.vector <- eigen.8$vectors
cumsum(my.value)/sum(my.value)
## we got .9504 at 150th data

dim(eigen.8$vectors)

dim(Data.8)
# remeber we could do Data.8 - mean, but it won't give us the correct answer because it will process by column

# how to center the data:
# scale() command 
data<- t( t(Data.8) - mean.8 )
dim(data)
center.8 <- scale(data, scale = FALSE)
center.8.sd<- center.8/sd(center.8)
dim(center.8)
sd(center.8)
#x1 <- center.8.sd[1,]
k <- 1
v <- my.vector[,1:5]
center.x <- as.matrix(center.8)[1,]
center.x.sd <- as.matrix(center.8.sd)[1,]

#value <- eigen.8$value
#value
#cent8Matrix <- as.matrix(center.8.sd)[1,]
#myscore <- t( v %*% t( cent8Matrix ) )
dim(t(v))
u <-as.matrix(center.x, nrow=784, ncol = 1) 
s <- as.matrix(center.x.sd, nrow=784, ncol = 1) 
dim(u)
myscore <- matrix( t(v)%*%u, nrow = 1 )
#myscore <- matrix( t(v)%*%s, nrow = 1 )
cor(myscore)

dim(center.8)
dim(v)
tempsomething <- t(v) %*% t(center.8)
tempsomething2 <- tempsomething[1,]

var(tempsomething2)















library(mvtnorm)
library(Matrix)

train.digit <- NULL
test.digit <- NULL
tmp <- read.csv('Digits_train.csv')

for( i in 0:9 ){
  tmp.digit.i <- tmp[tmp[,1]==i,-1]
  tmp.id.i <- sample(1:dim(tmp.digit.i)[1], round(dim(tmp.digit.i)[1]/3))
  train.digit <- rbind(train.digit, tmp[tmp.id.i,])
  test.digit <- rbind(train.digit, tmp[-tmp.id.i,])
}


data_0 <- train.digit[train.digit[,1] == 0,]
data_8 <- train.digit[train.digit[,1] == 8,]
data_9 <- train.digit[train.digit[,1] == 9,]

test_0 <- test.digit[test.digit[,1] == 0,]
test_8 <- test.digit[test.digit[,1] == 8,]
test_9 <- test.digit[test.digit[,1] == 9,]


# Chapter 7, QDA does not work since [data set] is to small 
# Many variable has 0 variance which is not helping the classification
# LDA will help us low the varirables

# Rememeber the mean, cov calc and score calc should done with all data ( 0, 8, 9) together
# we should not calc mean and variance individually or center data individually

dim(data_0)
dim(data_8)
dim(data_9)
dim(all_data)

all_data <- rbind( rbind(data_0, data_8), data_9)
all_test <- rbind( rbind(test_0, test_8), test_9)
#dim(all_data)
#dim(all_test)

all_mean <- sapply(all_data[,-1], function(x) mean(x))
all_cov <- cov(all_data[,-1])
#dim(all_cov)

all_eigen <- eigen(all_cov)

all_eigen$values

cumsum(all_eigen$values)/sum(all_eigen$values)

all_test - all_mean 

# mclost R package




