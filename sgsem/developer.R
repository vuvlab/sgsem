library("htmlwidgets", "DiagrammeR")

## -------------------------
## Load all R files
## -------------------------
setwd("/Users/yifanxu/gdrive/aarepos/sgsem/packages/sgsem")
source("loadDir.R")
sourceDir(file.path(".", "package", "R"))
load(file.path(".", "package", "data", "acrylic.RData"))

## -------------------------
## semi-gSEM with nls
## -------------------------
source("simulation.R")

## Run semi-gSEM
ans1 <- sgSEMp1(x, nlsInits = genInit())

genInit()
##plot(ans1, cutoff = 0.8)
plotAll(ans1, dirPath = file.path(".", "testsFigs"))

rands <- genInit(bounds = list(
                     a1 = c(-1, 3),
                     a2 = c(4, 5),
                     a3 = c(0,3)), numGen = 20)

ans1 <- sgSEMp1(x, nlsInits = rands)

## Run semi-gSEM principle one
ans <- sgSEMp1(acrylic, stressor="IrradTot", response="YI")

## See the summary
summary(ans)

## Plot the result
plot.sgSEMp1(ans)

## Plot result with different R-sqr cutoff
plot(ans, cutoff = 0.5)

## Plot all individual fittings
plotAll(ans)

## Print three components of the result
names(ans)
ans$res.print

## Look at a specific fit
summary(ans$res.all[[1,2,2]])
pairs(acrylic)
1

## ----------------------
## get path equations
## ----------------------
ans2 <- sgSEMp1(acrylic, stressor="IrradTot", response="YI", nlsInits = genInit())
path(ans2, "IrradTot", "YI")

## ----------------------
## Principle two
## ----------------------
## help file
?sgSEMp2

## Test
data(acrylic)
summary(acrylic)

## Run semi-gSEM principle one
ans <- sgSEMp2(acrylic, stressor="IrradTot", response="YI")

## See the summary
summary(ans)

## Plot the result
plot(ans)

## Plot result with different R-sqr cutoff
plot(ans, cutoff = 0.5)

## Print three components of the result
ans$res.print
