library(cluster)
#Example 1
data(votes.repub)
dv <- diana(votes.repub, metric = "manhattan", stand = TRUE)
print(dv)
plot(dv)

#Example 2
data(votes.repub)
agn1 <- agnes(votes.repub, diss="FALSE", metric = "manhattan", stand = TRUE)
agn1
class(agn1)
#dendogram
pltree(agn1)
plot(agn1)
