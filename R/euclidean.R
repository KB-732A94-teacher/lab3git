# hyungyum kim
# hyuki392

#1.1.1

#' Calculate Greatest Common Dvisor(GCD) for two input variables
#' 
#' @param x A number(integer)
#' @param y A number(integer)
#' @return GCD(Greatest Common Divisor) of x and y
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export euclidean 

euclidean <- function (x,y){
  
  stopifnot(is.numeric(x) & is.numeric(y))
  
  if(x > y) {
    smaller = y
  } else {
    smaller = x
  }
  for(i in 1:abs(smaller)) {
    if((x %% i == 0) && (y %% i == 0)) {
      GCD = i
    }
  }
  return(GCD)
}