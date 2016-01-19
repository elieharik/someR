#Newspaper example in R

#Read data.
path = "/Users/elieharik/Desktop/WORK/NORTHWESTERN/Winter Quarter/MSiA 421 - Data Mining/R Code/Data"
np = read.table(paste(path, "/np.dat.txt", sep = ""), header = TRUE)
np2 = read.table(paste(path, "/np2.dat.txt", sep = ""), header = TRUE)

#Plot labels
npxlab = "Number Hours / Week"
npylab = "Number sections"

#Let's make a contour plot.
library(akima)
#Implement bivariate interpolation onto a grid for irregularly spaced data.
np.smooth = with(np, interp(time, sections, sqrt(count), xo = seq(0,7,length = 60), yo = seq(0,7, length = 60))) 
#@Note: @Question: How the hell does interp work???
image(np.smooth, col = gray((32:0)/32), xlab = npxlab, ylab = npylab)
contour(np.smooth, levels = 2:16, add = T)

#np[1:5,]
#np2[1:5,]

#K Means in R
set.seed(12345)
fit = kmeans(np2, 5,100,100) #kmeans(data, centers, maxiterations, how many random sets to choose initially)
names(fit)

#Check cluster assignment
fit$cluster
#Or alternatively get counts
fit$size

#Check centers of clusters
fit$centers

summary(fit) #Getting different output

#Check the within cluster SSE
fit$withinss

#Plot the data
#Jitter adds a small amount of noise to a numeric vector
plot(jitter(np2$time), jitter(np2$sections), col = fit$cluster, pch = 16, cex = .5, xlab = npxlab, ylab = npylab)
points(fit$centers, pch = 3, cex = 4) #Add cluster centers. 
