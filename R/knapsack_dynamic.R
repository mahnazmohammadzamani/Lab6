#' @name Dynamic_programming
#' @param x dataframe
#' @param W number
#' @author Mahnaz , Bita
#' @description This approach is of complexity O(2n) since all possible combinations 2n needs to be evaluated
#' @title Dynamic programming
#' @references https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem
#' @export knapsack_dynamic
#' @examples 
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#'set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000))



knapsack_dynamic<-function(x, W){
  stopifnot(is.data.frame(x),W>0)
  weight_mat<-x[,1]
  Value_mat<-x[,2]
  n<-length(Value_mat)
  
  Dyn_mat<-matrix(0,nrow=n+1,ncol=W+1)
  for (i in 2:(n+1)){
    k<-(i-1)
    for(j in 2:(W+1)){
      q<-(j-1)
      if(weight_mat[k]>q){
        Dyn_mat[i,j]<-Dyn_mat[k,j]
      }else {
        M1<-Dyn_mat[k,j]
        M2<-Value_mat[k]
        M3<-(q-weight_mat[k])
        M4<-Dyn_mat[k,M3+1]
        M5<-M4+M2
        Dyn_mat[i,j]<-max(M1,M5)
      }
    }
    
  }
  Value_final<-Dyn_mat[i,j]
  Mat_final<-matrix(0,nrow=1,ncol=n)
  
  for (c in (n+1):2)
  {
    if(Dyn_mat[c,j]==Dyn_mat[c-1,j]){Mat_final[1,c-1]<-0
    } else {
      Mat_final[1,c-1]<-1
      j<-(W-weight_mat[c-1])+1
      
    }
  }
  elements<-which(Mat_final==1)
  value<-round(Value_final)
  Data<-list(value=value,elements=elements)
  return(Data)
}
