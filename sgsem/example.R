## Install the package PAY ATTENTION to the version numbers
install.packages("sgSEM_0.4.1.tar.gz", repos = NULL, type = "source")
library(sgSEM)

## ----------------------
## Principle one
## ----------------------
## help file
?sgSEMp1

## Test
data(acrylic)
summary(acrylic)

## Run semi-gSEM principle one
ans <- sgSEMp1(acrylic, stressor="IrradTot", response="YI")

## See the summary
summary(ans)

## Plot the result
plot(ans)

## Plot result with different R-sqr cutoff
plot(ans, cutoff = 0.5)

## Print three components of the result
names(ans)
ans$res.print

## Look at a specific fit
summary(ans$res.all[[1,2,2]])
pairs(acrylic)

## path
path(ans, from = "IAD1", to = "YI", round = 6)

1
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
