summary.kmeans = function(fit) {
  p = ncol(fit$center)
  k = nrow(fit$center)
  n = sum(fit$size)
  sse = sum(fit$size)
  xbar = t(fit$centers) %*% fit$size/n
  ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2) 
  print(data.frame(
    n=c(fit$size, n),
    Pct=(round(c(fit$size, n)/n,2)),
    round(rbind(fit$centers, t(xbar)), 2),
    RMSE = round(sqrt(c(fit$withinss/(p*fit$size-1), sse/(p*(n-k)))), 4)
  ))
  cat("SSE = ", sse, "; SSB = ", ssb, "\n")
  cat("R-Squared = ", ssb/(ssb+sse), "\n")
  cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
  invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}

plot.kmeans = function(fit,boxplot=F) {
  require(lattice)
  p = ncol(fit$centers) k = nrow(fit$centers) plotdat = data.frame(
    mu=as.vector(fit$centers),
    clus=factor(rep(1:k, p)),
    var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers))
  )
  print(dotplot(var~mu|clus, data=plotdat,
                panel=function(...){ panel.dotplot(...) panel.abline(v=0, lwd=.1)
                },
                layout=c(k,1), xlab="Cluster Mean"
  )) invisible(plotdat) }


summary(fit)
plot(fit)
