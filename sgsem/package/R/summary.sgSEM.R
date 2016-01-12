##' summarize sgSEM result
##'
##' summary.sgSEM gives a summary about the sgSEm analysis.
##'
##' @title Summary of Semi-gSEM
##' @param object An object of class "sgSEM", the returned list from sgSEM().
##' @param ... A S3 generic/method consistency.
##' @return NULL. A summary of data and fitting result is printed on screen.
##'
##' @export
##'
##' @examples
##' data(acrylic)
##' ans <- sgSEM(acrylic)
##' summary(ans)

summary.sgSEM <- function(object, ...){
  cat("sgSEM: It consideres all stressors and finds the best regression model \n")
  cat("\n")
  cat("Stressors (Multiple Stressors):", colnames(object$bestModels)[-2], "\n")
  cat("Response:", colnames(object$bestModels)[2], "\n")

  cat("\n")
  cat("Chosen models:\n")
  #sapply(1:nrow(object$table),function(ii) {
  #table<- c(object$table[ii,2], object$table[ii,1], object$table[ii,3], round(as.numeric(object$table[ii,5])))
  sapply(1:nrow(object$table), function(i) {
    paste(object$table[i,2], "--->", object$table[i,1],
          "|", object$table[i,3], "(Adjusted R-square = ",
          round(as.numeric(object$table[i,5]), 5), ")", collapse="\n")
  }
  )
}
