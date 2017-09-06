library(profvis)
profvis(
  {
library(arules)
data_set<-data(Adult)
rules <- apriori(Adult, parameter<-list(support = 0.01, confidence = 0.6))
})