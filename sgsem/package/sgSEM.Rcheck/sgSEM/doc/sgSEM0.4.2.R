## ---- eval=FALSE---------------------------------------------------------
#  install.packages("sgSEM_0.4.2.tar.gz", repos = NULL, type = "source")
#  library(sgSEM)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  
#  ## Load the acrylic data set
#  data(acrylic)
#  
#  ## Run semi-gSEM
#  ans = sgSEM(acrylic, stressor= c("IrradTot", *IAD1*, *IAD2*, *IAD2p*, and *IAD3*), response="YI")
#  
#  ## Plot result
#  plot(ans)
#  
#  ## Plot result with different R-sqr cutoff
#  plot(ans, cutoff = 0.2)
#  

