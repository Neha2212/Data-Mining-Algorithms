# Example 1
# data(agriculture)
# ## Example 1 in ref:
# ## Dissimilarities using Euclidean metric and without standardization
# d.agr <- daisy(agriculture, metric = "euclidean", stand = FALSE)
# d.agr
# as.matrix(d.agr)[,"DK"] # via as.matrix.dist(.)
# ## compare with
# diss<-as.matrix(daisy(agriculture, metric = "gower"))

data(flower)
## Example 2 
d.flower<-daisy(flower)
diss<-as.matrix(d.flower)