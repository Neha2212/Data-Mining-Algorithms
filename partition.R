T<-read.csv("D:/projects n assignments/8th sem/DM_ass/example.csv")
minSupport <- 3

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

countSupport <- function(T, item) {
  # item is a vector of attributes
  support <- 0
  # for every transaction, check that every attribute is 1 for that transaction
  # if yes, increment support
  for (i in 1:nrow(T)) {
    if (isPresent(T[i, ], item)) {
      support <- support + 1
    }
  }
  return(support)
}

gen_candidate_itemsets <- function(L, k) {
  C <- list()
  for (item1 in L) {
    for (item2 in L) {
      # print(item1)
      # print(item2)
      allEqual <- TRUE
      i <- 1
      while (i < k - 1) {
        if (item1[i] != item2[i]) {
          allEqual <- FALSE
          break
        }
        i <- i + 1
      }
      if (allEqual && item1[k-1] < item2[k-1]) {
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

apriori <- function(T, minSupport) {
  frequent_itemsets <- list()
  
  # Initialization
  k <- 1
  C <- list()
  L<- list()
  for(i in 1:ncol(T)){
    m<-length(which(T[,i]==1))
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
      if (countSupport(T, item) >= minSupport) {
        L[[length(L) + 1]] <- item
      }
    }
    frequent_itemsets <- append(frequent_itemsets, L)
    k <- k + 1
  }
  
  return(frequent_itemsets)
}

partition <- function(T, n, minSupport) {
  C <- list()
  freq<- list()
  # Phase 1
  for (i in seq(1, nrow(T), nrow(T)/n)) {
    tmp <- apriori(T[i:(i+(nrow(T)/n)-1), ], minSupport/n)
    # print(tmp)
    C <- append(C, tmp)
    unique(C)
    for(i in C){
      if(countSupport(T,i)>= minSupport)
      {
        freq[[length(freq)+1]]<- i
      }
    }
    
  }
  
  return((freq))
}
global_set<- partition(T,3,minSupport)