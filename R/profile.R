#' @import profvis

 RNGversion(min(as.character(getRversion()),"3.5.3"))
 set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
 n <- 2000
 knapsack_objects <-
   data.frame(
     w=sample(1:4000, size = n, replace = TRUE),
     v=runif(n = n, 0, 10000)
   )
 W <- 3500


#library(profvis)


profvis::profvis({knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)})
