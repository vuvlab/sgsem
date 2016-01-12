##' This function Calculate R^2 and adj R^2 from nls result
##' @title nlsR2
##' @param p dataframe. By default it consideres all columns as stressors except the second column stores the system response.
##' @param y A number, it refers to the response vector.
##' @param nlsAns A number refers to the residual term.
nlsR2 <- function(nlsAns, # return from a nls() call
                  y, # response vector
                  p) # number of parameters
{
  n <- length(y)
  rsds <- residuals(nlsAns)
  rr <- sum(rsds^2) / var(y) / (length(y) - 1)
  R2 <- 1 - rr
  adjR2 <- 1 - rr * (n - 1) / (n - p)
  return(list(R2 = R2, adjR2 = adjR2))
}
