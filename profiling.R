library(cluster)
library(profvis)
profvis({
  library(cluster)
  #clustering : Partitioning Around Medoids
  data(votes.repub)
  pamx <- pam(votes.repub, 3)
  pamx
  summary(pamx)
  plot(pamx)
  
  #clara
  clarax <- clara(votes.repub, 2, samples=50, correct.d = TRUE)
  clarax
  clarax$clusinfo
  all.equal(clarax[-8],
            clara(votes.repub, 2, samples=50, pamLike = TRUE,correct.d=TRUE)[-8])
  plot(clarax)
  
  #agnes n diana
  agn1 <- agnes(votes.repub, diss="FALSE", metric = "manhattan", stand = TRUE)
  plot(agn1)
  dv <- diana(votes.repub, metric = "manhattan", stand = TRUE)
  plot(dv)
  
  #kmeans
  x <- rbind(cbind(rnorm(10,0,0.5), rnorm(10,0,0.5)),
             cbind(rnorm(15,5,0.5), rnorm(15,5,0.5)))
  cl<-kmeans(x,2)
  plot(cl$cluster)
})
