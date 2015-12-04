### Toy Example

set.seed(2) ### set a random seed

x=matrix(rnorm(75*2), ncol=2)

fix(x)

### first group center is (4, -5), 
### second group center is (6, 2), 
### third group center is (0, 0)

x[1:25,1] = x[1:25, 1] + 4  ### shift the first halft group x-coordinate by 4

x[1:25,2] = x[1:25, 2] - 5  ### shift the first half group y-coordinate by -5

x[26:50,1] = x[26:50, 1] + 6  ### shift the second halft group x-coordinate by 6

x[26:50,2] = x[26:50, 2] + 2  ### shift the second half group y-coordinate by 2

km1.out = kmeans(x=x, centers=3, nstart=20)  ### choose nstart random sets with K=3 clusters
km1.out$cluster  ### result
km1.out$tot.withinss

plot(x, col=(km1.out$cluster+1), 
     main="K-Means Clustering Results with K=3", 
     xlab="", ylab="", pch=20, cex=2)

### misspecify the number of clusters as K=4 instead of K=3

set.seed(4)
km2.out = kmeans(x=x, centers=4, nstart=20)
km2.out
km2.out$tot.withinss

plot(x, col=(km2.out$cluster+1), 
     main="K-Means Clustering Results with K=4", 
     xlab="", ylab="", pch=20, cex=2)

### misspecify the number of clusters as K=2 instead of K=3

set.seed(8)
km3.out = kmeans(x=x, centers=2, nstart=20)
km3.out
km3.out$tot.withinss

plot(x, col=(km3.out$cluster+1), 
     main="K-Means Clustering Results with K=2", 
     xlab="", ylab="", pch=20, cex=2)

### contructing the AIC

km1.out$tot.withinss+75*3

km2.out$tot.withinss+75*4

km3.out$tot.withinss+75*2
