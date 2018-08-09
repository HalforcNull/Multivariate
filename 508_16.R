library(Flury)
library(dplyr)
data("wines")

compute.withIn <- function(data, classColIndex){
  names<-unique(data[classColIndex])
  
}


# pre prob
t <- as.data.frame(table(wines[1]))
temp_result <- t[,2] / nrow(wines)
result.weight <- cbind(t[1], 1/temp_result)[,2]
result.preProb[,2] = c(1/3,1/3,1/3)
names(result.weight) <- c('Country', 'weight')
names(result.preProb) <- c('Country', 'PreProb')
rm(t, temp_result)


# mean
t<-wines
temp <- as.vector( by(t[,-1], t$Country, function(x) colMeans(x))) 
result.mean <- rbind(temp$'South Africa', temp$Germany, temp$Italy)
rm(t, temp)


# variance
t<-wines
result.cov <- by(t[,-1], t$Country, cov)
result.cov$`South Africa` = result.cov$`South Africa` * as.numeric(result.weight[1])
result.cov$Germany = result.cov$Germany * as.numeric(result.weight[2])
result.cov$Italy = result.cov$Italy * as.numeric(result.weight[3])
rm(t)

# mean_bar = sum( preprob * mean )
mean_bar = as.data.frame( t(colSums( result.preProb[,2] * result.mean ) ))
with_in_var = result.preProb[1,2] * result.cov$'South Africa' + 
              result.preProb[2,2] * result.cov$Germany        +
              result.preProb[3,2] * result.cov$Italy


mean_barMatrix = rbind(mean_bar,mean_bar,mean_bar)
mean_diff_Matrix = as.matrix(result.mean[,-1] - mean_barMatrix)
in_between_var = result.preProb[1,2] * t(mean_diff_Matrix) %*% mean_diff_Matrix

a = mean_bar
b_1 = result.mean[1,]
b_2 = result.mean[2,]
b_3 = result.mean[3,]
d_1 = as.matrix(b_1-a)
d_2 = as.matrix(b_2-a)
d_3 = as.matrix(b_3-a)
in_between_var_v2 = t(d_1)%*%d_1 + t(d_2)%*%d_2 + t(d_3)%*%d_3 

var_bar = with_in_var+in_between_var

k <- solve(with_in_var) %*% in_between_var

ev <- eigen(k)

my_Vec <- ev$vectors
ev$values







dat <- wines
head(dat)

class.labs <- dat[,1]
dat.no.labs <- as.matrix(dat[,-1])


n.classes <- length(unique(class.labs))
class.labs <- as.numeric(class.labs)

### get mean vectors
mean.vects <- by(dat.no.labs, class.labs, colMeans)
grand.mean <- colMeans(dat.no.labs)

### get class covariance matrices
cov.mats <- by (dat.no.labs, class.labs, cov)

### get within covariance
within.cov <- (1/length(class.labs))*Reduce("+", lapply(1:n.classes, function(x, class.labs, cov.mats){(sum(class.labs==x)-1)*cov.mats[[x]]}, class.labs, cov.mats))

### get between covariance
between.cov <- Reduce("+",lapply(1:n.classes, function(x, class.labs, mean.vects, grand.mean){sum(class.labs==x)/length(class.labs)*(mean.vects[[x]]-grand.mean)%*%t(mean.vects[[x]]-grand.mean)}, class.labs, mean.vects, grand.mean))

### do eigenvalue decomposition
eigen.vects <- eigen(solve(within.cov)%*%between.cov)$vectors
eigen.vals <- eigen(solve(within.cov)%*%between.cov)$values

### transform data
proj.dat <- as.matrix(dat.no.labs)%*%matrix(as.numeric(eigen.vects[,1:(n.classes-1)]), dim(eigen.vects)[1], n.classes-1)
plot(proj.dat[,1], proj.dat[,2], col=class.labs, pch=16)

### predict classes
proj.means <- by(proj.dat, class.labs, colMeans)
proj.means <- rbind(proj.means[[1]], proj.means[[2]], proj.means[[3]])

pred.labs <- apply(proj.dat, 1, function(x, proj.means){which.min(diag((x-proj.means)%*%t(x-proj.means)))}, proj.means)

table(pred.labs, class.labs)
sum(pred.labs==class.labs)/length(class.labs)


