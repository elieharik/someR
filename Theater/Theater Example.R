library(lattice)
path = getwd()
theater = read.csv(paste(path, "/Data/theater.csv", sep = ""))
theater$age2 = theater$age  * 5 + 20 #transform response cat to age in yrs
Ztheater = data.frame(lapply(theater[,1:5], scale, scale = T))

set.seed(12345) #To get the same initial values
fit.th3 = kmeans(Ztheater, 3, nstart = 100)
fit.th4 = kmeans(Ztheater, 4, nstart = 100) #4 clusters
fit.th5 = kmeans(Ztheater, 5, nstart = 100) #5 clusters..

#Summaries 
summary(fit.th3)
summary(fit.th4)

