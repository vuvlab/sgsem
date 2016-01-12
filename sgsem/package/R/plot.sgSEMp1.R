##' Plot semi-gSEM principle 1 result
##' 
##' plot.sgSEMp1 plots a structural equation network model diagram based on best functional form for each selected pairwise variable. 
##' @title Plotting of Principle 1 of Semi-gSEM
##' @param x The returned list from sgSEMp1. Plotting uses the first element of this list (res.print) in which the first column of it is response, second column is variable and other columns are corresponding best functional form, r-squared, adj-r-squared, P-value1, P-value2 and P-value3.
##' @param cutoff1 A threshold value for the lower adjusted R-squared. Dotted lines represent relationships with adjusted R-sqr of lower than the cutoff1. The default is 0.5. Thin solid lines represent relationships with adjusted R-squared values between the two cutoffs.
##' @param cutoff2 A threshold value for the upper adjusted R-squared. Thick solid lines represent relationships with adjusted R-sqr of greater than the cutoff2. The default is 0.75. Thin solid lines represent relationships with adjusted R-squared values between the two cutoffs.
##' @param width A numeric describing the width of the plot output in pixels.
##' @param height A numeric describing the height of the plot output in pixels.
##' @param filename A character string naming a file to save as an html file.
##' @param ... Other parameters. Currently not used. 
##' @return An html style plot of pairwise relationship pathway diagram between stressors and repsonses. Arrows show relationships between each variable with given statistical relations along the connection lines.
##'   
##' @export
##' 
##' @examples
##' # Load acrylic data set
##' data(acrylic)
##' # Build a semi-gSEM model with principle 1
##' ans <- sgSEMp1(acrylic)
##' # Plot the network model with adjusted-R-squred cutoffs of 0.2 and 0.8
##' plot(ans, cutoff1 = 0.2, cutoff2 = 0.8)

plot.sgSEMp1 <- function(x, ...,
                         cutoff1 = 0.5,
                         cutoff2 = 0.75,
                         width = NULL,
                         height = NULL,
                         filename = NULL){
  
  rtp1 <- x$table
  rtp1[, -c(1:3)] <- round(rtp1[, -c(1:3)], 2)
  rtp1.a <- rtp1[rtp1[, "adj-R-Sqr"] <= cutoff1, ]
  rtp1.b <- rtp1[(rtp1[, "adj-R-Sqr"] > cutoff1) & (rtp1[, "adj-R-Sqr"] < cutoff2), ] 
  rtp1.c <- rtp1[rtp1[, "adj-R-Sqr"] >= cutoff2, ]
  
  
  
  # This generates syntax for connections between variables and responses. 
  # <br/> is for linebreak between AIC values.
  
  # node styling options:
  # [] for rectanguler, () for rounded edges in rectangle, (( )) for circle, {} for rhombus
  # Details in "http://knsv.github.io/mermaid/flowchart.html"
  
  if(dim(rtp1.a)[1] > 0) {  
    conp1.a <- sapply(1:nrow(rtp1.a) ,
                      function(i){  
                        paste0(rtp1.a[i,2], "(", rtp1.a[i,2], ")", "-.->|", 
                               paste0(colnames(rtp1.a[,c(3,5:8)]),
                                      ":", rtp1.a[i,c(3,5:8)],
                                      collapse="<br/>"),"|", rtp1.a[i,1], "(", rtp1.a[i,1], ")")
                      }
    )
  }
  
  if(dim(rtp1.b)[1] > 0) {
    conp1.b <- sapply(1:nrow(rtp1.b) ,
                      function(i){  
                        paste0(rtp1.b[i,2], "(", rtp1.b[i,2], ")", "-->|", 
                               paste0(colnames(rtp1.b[,c(3,5:8)]),
                                      ":", rtp1.b[i,c(3,5:8)],
                                      collapse="<br/>"),"|", rtp1.b[i,1], "(", rtp1.b[i,1], ")")
                      }
    ) 
  }
  
  
  if(dim(rtp1.c)[1] > 0) {
    conp1.c <- sapply(1:nrow(rtp1.c) ,
                      function(i){  
                        paste0(rtp1.c[i,2], "(", rtp1.c[i,2], ")", "==>|", 
                               paste0(colnames(rtp1.c[,c(3,5:8)]),
                                      ":", rtp1.c[i,c(3,5:8)],
                                      collapse="<br/>"),"|", rtp1.c[i,1], "(", rtp1.c[i,1], ")")
                      }
    ) 
  }
  
  # This generates syntax to run "mermaid" for plotting using above syntax
  # "LR" is left to right flow
  
  # For "fill" and "stroke", CSS style coloring can be used. 
  
  if(exists("conp1.a")==TRUE & exists("conp1.b")==TRUE & exists("conp1.c")==TRUE) {
    conp1.plot <- paste0(
      "graph LR;", "\n", 
      paste(conp1.a, collapse = "\n"), "\n", paste(conp1.b, collapse="\n"), "\n", paste(conp1.c, collapse="\n"), "\n", 
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
  }
  
  
  if(exists("conp1.a")==FALSE & exists("conp1.b")==TRUE & exists("conp1.c")==TRUE) { 
    conp1.plot <- paste0(
      "graph LR;", "\n",
      paste(conp1.b, collapse="\n"), "\n",  paste(conp1.c, collapse="\n"), "\n",
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
  }
  
  if(exists("conp1.b")==FALSE & exists("conp1.a")==TRUE & exists("conp1.c")==TRUE) { 
    conp1.plot <- paste0(
      "graph LR;", "\n",
      paste(conp1.a, collapse="\n"), "\n", paste(conp1.c, collapse="\n"), "\n",  
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
  }
  
  if(exists("conp1.c")==FALSE & exists("conp1.a")==TRUE & exists("conp1.b")==TRUE) { 
    conp1.plot <- paste0(
      "graph LR;", "\n",
      paste(conp1.a, collapse="\n"), "\n", paste(conp1.b, collapse="\n"), "\n",  
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
  }
  
  if(exists("conp1.a")==FALSE & exists("conp1.b")==FALSE & exists("conp1.c")==TRUE) {
    conp1.plot <- paste0(
      "graph LR;", "\n", 
      paste(conp1.c, collapse = "\n"), "\n", 
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
    cat("All of the adjusted R-sqr values are higher than the cutoff2: Only thick lines", "\n")
  }
  
  if(exists("conp1.a")==FALSE & exists("conp1.c")==FALSE & exists("conp1.b")==TRUE) {
    conp1.plot <- paste0(
      "graph LR;", "\n", 
      paste(conp1.b, collapse = "\n"), "\n", 
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
    cat("All of the adjusted R-sqr values are between the two cutoff: Only solid lines", "\n")
  }
  
  if(exists("conp1.b")==FALSE & exists("conp1.c")==FALSE & exists("conp1.a")==TRUE) {
    conp1.plot <- paste0(
      "graph LR;", "\n", 
      paste(conp1.a, collapse = "\n"), "\n", 
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
    cat("All of the adjusted R-sqr values are lower than the cutoff1: Only dotted lines", "\n")
  }
  
  
  p1 <- DiagrammeR::mermaid(conp1.plot, width=width, height=height) 
  
  if(!is.null(filename))
    saveWidget(p1, file = filename, selfcontained = TRUE)
  
  return(p1)
}