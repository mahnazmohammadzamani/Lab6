#' @name greedy_programming
#' @param x dataframe
#' @param W number
#' @author Mahnaz , Bita
#' @description This approach is to use the a heuristic or approximation for the problem
#' @title greedy programming
#' @references https://en.wikipedia.org/wiki/Knapsack problem#Greedy approximation algorithm
#' @export greedy_knapsack
#' @examples 
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#'set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000))
#'  greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)



greedy_knapsack<-function(x,W){
  stopifnot(is.data.frame(x),W>0)
  i<-1
  weight_mat<-x[,1]
  Value_mat<-x[,2]
  n<-length(Value_mat)
  mat_final_greedy<-c(Value_mat/weight_mat)
  mat_final_greedy1<-sort(mat_final_greedy,decreasing = TRUE)
  i<-1
  Value<-c()
  elements<-c()
  repeat  
  {
    d<-which(mat_final_greedy == mat_final_greedy1[i])
    W<- W-weight_mat[d]
    Value[i]<-Value_mat[d]
    elements[i]<-d
    i<-i+1
    if (W<0){
      break
    }
  }
  L<-length(Value)
  Value<-Value[1:L-1]
  Value<- round(sum(Value))
  L1<-length(elements)
  elements<-elements[1:L1-1]
  Data<- list(value=Value, elements=elements)
  return(Data)
}

