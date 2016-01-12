##' Plotting of Principle 2 of Semi-gSEM
##' 
##' plot.sgSEMp2 plots a structural equation network model diagram based on best functional form for additive pairwise relationships.
##' 
##' @param x The returned list from sgSEMp2. Plotting uses the first element of this list (res.print) in which the first column of it is response, second column is variable and other columns are corresponding statistical model, r-squared, adj-r-squared, P-value, P-value rank, p1ff, r2mark, and markrank.
##' @param cutoff1 A threshold value for the lower adjusted R-squared. Dotted lines represent relationships with adjusted R-sqr of lower than the cutoff1. The default is 0.5. Thin solid lines represen relationships with adjusted R-squared values between the two cutoffs.
##' @param cutoff2 A threshold value for the upper adjusted R-squared. Thick solid lines represent relationships with adjusted R-sqr of greater than the cutoff2. The default is 0.75. Thin solid lines represen relationships with adjusted R-squared values between the two cutoffs.
##' @param width A numeric describing the width of the plot output in pixels.
##' @param height A numeric describing the height of the plot output in pixels.
##' @param filename A character string naming a file to save as an html file.
##' @param ... Other parameters. Currently not used. 
##' @return An html style plot of pairwise relationship pathway diagram between stressors and repsonses. Arrows show relationships between each variable with given statistical relations along the connection lines.
##'   
##' @keywords Semi-gSEM, principle 2, network pathway diagram
##'
##' @export
##' 
##' @examples
##' # Load acrylic data set
##' data(acrylic)
##' # Build a semi-gSEM model with principle 2
##' ans <- sgSEMp2(acrylic)
##' # Plot the network model with adjusted-R-squred cutoffs of 0.2 and 0.8
##' plot(ans, cutoff1 = 0.2, cutoff2 = 0.8)


plot.sgSEMp2 <- function(x, ..., 
                         cutoff1 = 0.5,
                         cutoff2 = 0.75,
                         width = NULL,
                         height = NULL,
                         filename = NULL){
  
  rtp2 <- x$res.print
  rtp2[, 3] <- gsub("[~]", "<-", rtp2[, 3])
  rtp2[, -c(1:3, 8)] <- round(rtp2[, -c(1:3, 8)], 2)
  rtp2.a <- rtp2[rtp2[, "adj-R-Sqr"] <= cutoff1, ]
  rtp2.b <- rtp2[(rtp2[, "adj-R-Sqr"] > cutoff1) & (rtp2[, "adj-R-Sqr"] < cutoff2), ] 
  rtp2.c <- rtp2[rtp2[, "adj-R-Sqr"] >= cutoff2, ]
  
  # This generates syntax for connections between variables and responses. 
  # <br/> is for linebreak between AIC values.
  
  # node styling options:
  # [] for rectanguler, () for rounded edges in rectangle, (( )) for circle, {} for rhombus
  # Details in "http://knsv.github.io/mermaid/flowchart.html"
  
  
  if(dim(rtp2.a)[1] > 0) {  
    conp2.a <- sapply(1:nrow(rtp2.a) , function(i){  
      paste0(rtp2.a[i,2], "(", rtp2.a[i,2], ")", "-.->|", 
             paste0(colnames(rtp2.a[,3:10]),
                    " : ", rtp2.a[i,3:10],
                    collapse="<br/>"),"|", rtp2.a[i,1], "(", rtp2.a[i,1], ")")
    }
    )
  }
  
  if(dim(rtp2.b)[1] > 0) {
    conp2.b <- sapply(1:nrow(rtp2.b) , function(i){  
      paste0(rtp2.b[i,2], "(", rtp2.b[i,2], ")", "-->|", 
             paste0(colnames(rtp2.b[,3:10]),
                    " : ", rtp2.b[i,3:10],
                    collapse="<br/>"),"|", rtp2.b[i,1], "(", rtp2.b[i,1], ")")
    }
    ) 
  }
  
  
  if(dim(rtp2.c)[1] > 0) {
    conp2.c <- sapply(1:nrow(rtp2.c) , function(i){  
      paste0(rtp2.c[i,2], "(", rtp2.c[i,2], ")", "==>|", 
             paste0(colnames(rtp2.c[,3:10]),
                    " : ", rtp2.c[i,3:10],
                    collapse="<br/>"),"|", rtp2.c[i,1], "(", rtp2.c[i,1], ")")
    }
    ) 
  }
  
  
  
  ## This generates syntax to run "mermaid" for plotting using above syntax
  ## "LR" is left to right flow
  ## For "fill" and "stroke", CSS style coloring can be used. 
  
  
  if(exists("conp2.a")==TRUE & exists("conp2.b")==TRUE & exists("conp2.c")==TRUE) {
    conp2.plot <- paste0(
      "graph LR;", "\n", 
      paste(conp2.a, collapse = "\n"), "\n", paste(conp2.b, collapse="\n"), "\n", paste(conp2.c, collapse="\n"), "\n", 
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
  }
  
  if(exists("conp2.a")==FALSE & exists("conp2.b")==TRUE & exists("conp2.c")==TRUE) { 
    conp2.plot <- paste0(
      "graph LR;", "\n",
      paste(conp2.b, collapse="\n"), "\n",  paste(conp2.c, collapse="\n"), "\n",
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
  }
  
  if(exists("conp2.b")==FALSE & exists("conp2.a")==TRUE & exists("conp2.c")==TRUE) { 
    conp2.plot <- paste0(
      "graph LR;", "\n",
      paste(conp2.a, collapse="\n"), "\n", paste(conp2.c, collapse="\n"), "\n",  
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
  }
  
  if(exists("conp2.c")==FALSE & exists("conp2.a")==TRUE & exists("conp2.b")==TRUE) { 
    conp2.plot <- paste0(
      "graph LR;", "\n",
      paste(conp2.a, collapse="\n"), "\n", paste(conp2.b, collapse="\n"), "\n",  
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
  }
  
  if(exists("conp2.a")==FALSE & exists("conp2.b")==FALSE & exists("conp2.c")==TRUE) {
    conp2.plot <- paste0(
      "graph LR;", "\n", 
      paste(conp2.c, collapse = "\n"), "\n", 
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
    cat("All of the adjusted R-sqr values are higher than the cutoff2: Only thick lines", "\n")
  }
  
  if(exists("conp2.a")==FALSE & exists("conp2.c")==FALSE & exists("conp2.b")==TRUE) {
    conp2.plot <- paste0(
      "graph LR;", "\n", 
      paste(conp2.b, collapse = "\n"), "\n", 
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
    cat("All of the adjusted R-sqr values are between the two cutoff: Only solid lines", "\n")
  }
  
  if(exists("conp2.b")==FALSE & exists("conp2.c")==FALSE & exists("conp2.a")==TRUE) {
    conp2.plot <- paste0(
      "graph LR;", "\n", 
      paste(conp2.a, collapse = "\n"), "\n", 
      "classDef default fill:#FFFF99, stroke:#000000, stroke-width:3px;")
    cat("All of the adjusted R-sqr values are lower than the cutoff1: Only dotted lines", "\n")
  }
  
  
  
  
  
  ## stylep2.b <- vector()
  ## for (i in 1:nrow(rtp2.b)){
  ##  stylep2.b <- c(stylep2.b, paste("style",rtp2.b[i,2], "font-size:larger, fill:#f9f, stroke:#333, stroke-width:4px;", sep=" "))}  
  ## if(exists("conp2.a")==FALSE) { 
  ##   conp2.plot <- paste0(
  ##     "graph LR;", "\n",
  ##     paste(conp2.b, collapse="\n"), "\n",
  ##     paste(stylep2.b, collapse="\n"))
  ##   cat("The cutoff value is lower than all of the adjusted R-sqr values: Only solid lines")
  ## } 
  
  
  p2 <- DiagrammeR::mermaid(conp2.plot, width=width, height=height) 
  
  if(!is.null(filename))
    saveWidget(p2, file = filename, selfcontained = TRUE)
  return(p2)
}