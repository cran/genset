## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(genset)

## -----------------------------------------------------------------------------
library(genset)

## -----------------------------------------------------------------------------
y <- mtcars$mpg
x1 <- mtcars$hp
x2 <- mtcars$wt

## -----------------------------------------------------------------------------
set1 <- data.frame(y, x1, x2)

## -----------------------------------------------------------------------------
multi.fun <- function(x) {
  c(mean = mean(x), media=median(x), sd=sd(x))
}
round(multi.fun(set1$y), 1)
round(multi.fun(set1$x1), 0)
round(multi.fun(set1$x2), 3)

## -----------------------------------------------------------------------------
summary(lm(y ~ x1, x2, data=set1))

## -----------------------------------------------------------------------------
set.seed(123)
set2 <- genset(y, x1, x2, method=2, option="x1")

## -----------------------------------------------------------------------------
round(multi.fun(set2$y), 1)
round(multi.fun(set2$x1), 0)
round(multi.fun(set2$x2), 3)

## -----------------------------------------------------------------------------
summary(lm(y ~ x1 + x2, data=set2))

## ---- fig.show='hold'---------------------------------------------------------
plot(set1)
plot(set2)

## -----------------------------------------------------------------------------
y <- mtcars$mpg
x1 <- mtcars$wt
x2 <- mtcars$vs

## -----------------------------------------------------------------------------
set3 <- data.frame(y, x1, x2)

## -----------------------------------------------------------------------------
v.shape <- subset(set3, x2==0)
straight <- subset(set3, x2==1) 

## -----------------------------------------------------------------------------
multi.fun <- function(x) {
  c(mean = mean(x), media=median(x), sd=sd(x))
}
round(multi.fun(v.shape$y), 1)
round(multi.fun(v.shape$x1), 3)
round(multi.fun(straight$y), 1)
round(multi.fun(straight$x1), 3)

## -----------------------------------------------------------------------------
summary(lm(y ~ x1 + factor(x2), data=set3))

## -----------------------------------------------------------------------------
set.seed(123)
set4 <- genset(y, x1, factor(x2), method=2, option="x2")

## -----------------------------------------------------------------------------
v.shape <- subset(set4, x2==0)
straight <- subset(set4, x2==1) 

## -----------------------------------------------------------------------------
multi.fun <- function(x) {
  c(mean = mean(x), media=median(x), sd=sd(x))
}
round(multi.fun(v.shape$y), 1)
round(multi.fun(v.shape$x1), 3)
round(multi.fun(straight$y), 1)
round(multi.fun(straight$x1), 3)

## -----------------------------------------------------------------------------
summary(lm(y ~ x1 + factor(x2), data=set4))

## ---- fig.show='hold'---------------------------------------------------------
plot(set3)
plot(set4)

## ---- echo=FALSE, include=FALSE, results='asis'-------------------------------
knitr::kable(head(set1, 10))

