RGLI <- function(a, B, num_trials = 50) {
 
     # delta(x) is the absolute error of the solution x
     delta <- function(x, a, B) {
         B - sum(x * a)
     }
 
     n <- length(a)
     x_best <- rep(0, n)
     for (trial in 1:num_trials) {
         # first phase: randomized selection
         x <- rep(0, n)
         for (i in sample(n)) {
             # go over elements in random order
             if (a[i] <= delta(x, a, B)) {
                 x[i] <- 1
             }
         }
 
         # second phase: local improvement
         I <- which(x == 1)
         for (i in sample(I)) {
             # in random order
             delta_x <- delta(x, a, B)
             if (delta_x == 0) {
                 break  # quit the inner for loop
             }
 
             # find potential elements to improve current solution
             T_idx <- which(x == 0 & a - a[i] > 0 & a - a[i] <= delta_x)
             if (length(T_idx) > 0) {
                 k <- T_idx[which.max(a[T_idx])]
                 x[k] <- 1
                 x[i] <- 0
             }
         }
 
         cat(paste("Trial", trial, "error:", delta(x, a, B), "\n"))
 
         # x_best update
         if (delta(x, a, B) < delta(x_best, a, B)) {
             cat("Current solution better than best so far. Updating.\n")
             x_best <- x
         }
 
         if (delta(x_best, a, B) == 0) {
             cat("Found perfect solution!\n")
             break  # quit the outer for loop
         }
     }
     return(as.logical(x_best))
 }

