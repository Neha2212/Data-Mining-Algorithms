library(profvis)
profvis(
{data<-read.csv("D:/projects n assignments/8th sem/DM_ass/example.csv")
data<-data.frame(data)
minsupport<-0.3*nrow(data)

isPresent <- function(transaction, item) {
  ret_val <- TRUE
  for (attribute in item) {
    if (transaction[attribute] != 1) {
      ret_val <- FALSE
      break
    }
  }
  return(ret_val)
}

countSupport <- function(data, item) {
  support <- 0
  for (i in 1:nrow(data)) {
    if (isPresent(data[i, ], item)) {
      support <- support + 1
    }
  }
  return(support)
}

gen_candidate_itemsets <- function(L, k) {
  C <- list()
  for (item1 in L) {
    for (item2 in L) {
      isEqual <- TRUE
      i <- 1
      while (i < k - 1) {
        if (item1[i] != item2[i]) {
          isEqual <- FALSE
          break
        }
        i <- i + 1
      }
      if (isEqual && item1[k-1] < item2[k-1]) {
        new_item <- append(item1, item2[k-1])
        C[[length(C) + 1]] <- new_item
      }
    }
  }
  return(C)
}

in_list <- function(list, item) {
  belongs <- FALSE
  for (i in list) {
    if (identical(i, item)) {
      belongs <- TRUE
      break
    }
  }
  return(belongs)
}

prune <- function(C, L, k) {
  i <- 1
  while (i <= length(C)) {
    #print(C[[i]])
    for (d in combn(C[[i]], k, simplify = FALSE)) {
      belongs <- FALSE
      if (in_list(L, d)) {
        C <- C[-i]
      }
    }
    i <- i + 1
  }
  return(C)
}

apriori <- function(data, minSupport) {
  frequent_itemsets <- list()
  C <- list()
  L<- list()
  for(i in 1:ncol(data)){
    m<-length(which(data[,i]==1))
    C<-append(C,i)
    if(m>=minsupport)
    {
      L<-append(L,i)
    }
  }
  frequent_itemsets <- L
  k <- 2  # k represents pass number
  while (length(L) != 0) {
    C <- gen_candidate_itemsets(L, k)
    C <- prune(C, L, k)
    L <- list()
    for (item in C) {
      if (countSupport(data, item) >= minSupport) {
        L[[length(L) + 1]] <- item
      }
    }
    frequent_itemsets <- append(frequent_itemsets, L)
    k <- k + 1
  }
  
  return(frequent_itemsets)
}
itemsets<-apriori(data,3)
})