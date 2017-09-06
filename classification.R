library(cluster)
#clustering : Partitioning Around Medoids
x <- rbind(cbind(rnorm(10,0,0.5), rnorm(10,0,0.5)),
           cbind(rnorm(15,5,0.5), rnorm(15,5,0.5)))
pamx <- pam(x, 2)
pamx
summary(pamx)
plot(pamx)

#clara
y <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
           cbind(rnorm(300,50,8), rnorm(300,50,8)))
clarax <- clara(y, 2, samples=50)
clarax
clarax$clusinfo
all.equal(clarax[-8],
          clara(y, 2, samples=50, pamLike = TRUE)[-8])
plot(clarax)

#agnes n diana
data(votes.repub)
agn1 <- agnes(votes.repub, diss="FALSE", metric = "manhattan", stand = TRUE)
plot(agn1)
dv <- diana(votes.repub, metric = "manhattan", stand = TRUE)
plot(dv)

#kmeans
cl<-kmeans(x,2)
plot(cl$cluster)