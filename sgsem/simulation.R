mysd <- 0.1
n <- 50
x <- data.frame(time = seq(0.1, 10, length = n), YI = NA,
                A = NA, B = NA, C = NA, D = NA, E = NA)

## Linear
x$A <- 3 + 0.5 * x$time + rnorm(50, sd = mysd)
## Quadratic
x$B <- 0.5 - 1 * x$time + 0.1 * (x$time)^2 + rnorm(50, sd = mysd)
## Simple Quadratic
x$C <- 1 + 0.5 * (x$time)^2 + rnorm(50, sd = 2)
## Exponential
x$D <- 6 + 3 * exp(x$time) + rnorm(50, sd = 20)
## Log
x$E <- 1 + 0.5 * log(x$time) + rnorm(50, sd = mysd)
## Change point
x$F <- 1 + 0.5 * x$time + 2 * ifelse(x$time - 5 > 0, x$time - 5, 0) +
    rnorm(50, sd = mysd)
## nls1
x$G <- 1 - 2.5 * exp(0.3 * (x$time)) + rnorm(50, sd = 2)
## nls2
x$H <- 2 + 0.1 * exp(0.5 * (x$time)) + rnorm(50, sd = 1)
## linear on A
x$YI <- 1 + 0.5 * x$A + rnorm(50, sd = mysd)

types <- c("Linear", "Quad", "Simple Quad", "Exp", "Log", "Change Pt", "nls1", "nls2")
vars <- names(x)[c(-1, -2)]
par(mfrow = c(2, 4), mar = c(2,2,4,2))
for(ivar in 1:length(vars))
    plot(x[["time"]], x[[vars[ivar]]],
         xlab = "time", ylab = vars[ivar], main = types[ivar])




