#' @name brute_force_knapsack
#' @param x dataframe
#' @param W number
#' @param parallel logical
#' @author Mahnaz , Bita
#' @description This approach is to use the a heuristic or approximation for the problem
#' @title brute_force_knapsack_parallel
#' @import parallel
#' @export brute_force_knapsack
#' @examples 
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#'set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000))
#'  brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)



#library(parallel)
brute_force_knapsack<-function(x,W,parallel=FALSE){
  stopifnot(is.data.frame(x),W>0)
  n<-2^(length(x[,2]))-1
  d<-c()
  myfunc <- function(i){
    binary<-intToBits(i)
    d<-which(binary == 1)
    Len_d<-length(d)
    weight<-c()
    value<-c()
    for(j in 1:Len_d)
    {
      weight[j]<-x[,1][d[j]]
      value[j]<-x[,2][d[j]]
    }
    sum1<-sum(weight)
    if (sum1<=W)
    {
      return(sum(value))
    }else {
      return(0)
    }
    
  }
  if (parallel==FALSE){
    sum_value <- c(lapply(1:n, myfunc))
  }else{
    cores <- parallel::detectCores()
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    sum_value <- parLapply(cl=cl, 1:n, fun = myfunc)
    # Shut down cluster
    parallel::stopCluster(cl)
  }
  

  max_sum_value <- sum_value[[which.max(sum_value)]]
  Sum_final<- round(max_sum_value)
  i1<-which( sum_value== max_sum_value)
  ele<-intToBits(i1)
  element<-which(ele == 1)
  Data<-list(value=Sum_final,elements=element)
  return(Data)
}


