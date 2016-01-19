#Create data
set.seed(12345)
dat = data.frame(
  x1 = c(rnorm(100), rnorm(100)+3),
  x2 = c(rnorm(200)),
  y = c(rep(1,100), rep(2,100))
)

#Plot x1 x2
plot(dat[,1:2], col = dat$y, pch = 15 + dat$y)
fit = kmeans(dat[,1:2], 2, nstart = 100)
summary(fit)

table(fit$cluster, dat$y) #contingency table

plot(dat[,1:2], col = fit$cluster, pch = 15+fit$cluster)
wrong = (fit$cluster != dat$y)
points(dat[wrong, 1:2], cex = 2) #Mark points that have been missclassified
points(fit$center, pch =3, cex = 3) #Mark cluster centers


#Effects of multicollinearity and incomeensurate units
#Let's simulate multicollinearity/
dat$x3 = dat$x2
dat$x4 = dat$x2
dat$x5 = dat$x2
fit2 = kmeans(dat[,-3], 2, nstart = 100)
summary(fit2)

plot(dat[,1:2], col = fit2$cluster, pch = 15 + fit2$cluster)
points(fit2$centers, pch = 3, cex = 3)
wrong = (fit2$cluster != dat$y)
points(dat[wrong, 1:2], cex = 2)


#Now incommensurate units
dat$x2 = 2*dat$x2
fit2 = kmeans(dat[,1:2],2,nstart = 100)
summary(fit2)
plot(dat[,1:2], col = fit2$cluster, pch = 15+fit2$cluster,ylab = "2*x2")
wrong = (fit2$cluster != dat$y)
points(fit2$center, pch =3, cex = 3) #Mark cluster centers


#Effects of unequal sizes or variances
#k means likes to produce clusters of equal size
#Unequal sized clusters
set.seed(12345)
dat = data.frame(
  x1 = c(rnorm(100), rnorm(50)+3),
  x2 = c(rnorm(150)),
  y = c(rep(1,100), rep(2,50))
)

fit = kmeans(dat[,1:2], 2, nstart = 100)
plot(dat[,1:2], col = dat$y, pch = 15 + dat$y)
points(dat[fit$cluster != dat$y, 1:2], cex = 2)
points(fit$centers, pch =3, cex = 3)
summary(fit)

#Variance test
#Reducing the variance by 2
set.seed(12345)

dat = data.frame(
  x1 = c(rnorm(100), rnorm(100)/3+2),
  x2 = c(rnorm(100), rnorm(100)/3),
  y = c(rep(1,100), rep(2,50))
)

fit = kmeans(dat[,1:2], 2, nstart = 100)
plot(dat[,1:2], col = dat$y, pch = 15 + dat$y)
points(dat[fit$cluster != dat$y, 1:2], cex = 2)
points(fit$centers, pch =3, cex = 3)
summary(fit)


