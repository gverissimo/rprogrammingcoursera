
## clear the console
## click on the console and type ctrl-l

## clear the workspace
ls() # show workspace
rm(list=ls()) # clear workspace
ls() # show empty workspace

x <- list(a = 1:5, b = rnorm(10))
x
lapply(x, mean)

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
x
lapply(x, mean)

x<-1:4
x
lapply(x, runif)

x<-1:4
x
lapply(x, runif, min = 0, max = 10)

x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x
lapply(x, function(elt) elt[,1])

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
x
lapply(x, mean)
sapply(x, mean)


x <- matrix(rnorm(200), 20, 10)
x
apply(x, 2, mean)
apply(x, 1, sum)

x <- matrix(rnorm(200), 20, 10)
x
apply(x, 1, quantile, probs = c(0.25, 0.75))

a <- array(rnorm(2 * 2 * 10), c(2, 2, 10))
a
apply(a, c(1, 2), mean)
rowMeans(a, dims = 2)

# want to create: list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))
list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))
mapply(rep, 1:4, 4:1)

noise <- function(n, mean, sd) {
                rnorm(n, mean, sd)
        }
noise(5, 1, 2)
noise(1:5, 1:5, 2)
mapply(noise, 1:5, 1:5, 2)

x <- c(rnorm(10), runif(10), rnorm(10, 1))
x
f<-gl(3,10)
f
tapply(x, f, mean)
tapply(x, f, mean, simplify = FALSE)
tapply(x, f, range)


x <- c(rnorm(10), runif(10), rnorm(10, 1))
x
f<-gl(3,10)
f
split(x, f)
lapply(split(x, f), mean)

library(datasets)
head(airquality)

s <- split(airquality, airquality$Month)
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))


x <- rnorm(10)
x
f1<-gl(2,5)
f1
f2<-gl(5,2)
f2
interaction(f1, f2)
list(f1, f2)
str(split(x, list(f1, f2)))
str(split(x, list(f1, f2), drop = TRUE))


log(-1)

printmessage <- function(x) { 
        if(x > 0) {
                print("x is greater than zero")
        } else {
                print("x is less than or equal to zero")
        }
        invisible(x)
}

printmessage(1)
printmessage(-3)
printmessage(NA)
printmessage(NaN)

printmessage2 <- function(x) {
        if(is.na(x)) {
                print("x is a missing value!") 
        } else if(x > 0) {
                print("x is greater than zero")
        } else {
                print("x is less than or equal to zero")
        }
        invisible(x)
}

printmessage2(1)
printmessage2(-3)
printmessage2(NA)
printmessage2(NaN)

x <- log(-1)
printmessage2(x)

## debugging with traceback...
foo <- function(x) { print(1); bar(2) }
bar <- function(x) { x + a.variable.which.does.not.exist }
## Not run: 
foo(2) # gives a strange error
traceback()
## End(Not run)
## 2: bar(2) at #1
## 1: foo(2)

mean(y)
traceback()
## 1: mean(y)

lm(z~y)
traceback()
## 6: eval(predvars, data, env)
## 5: model.frame.default(formula = z ~ y, drop.unused.levels = TRUE)
## 4: stats::model.frame(formula = z ~ y, drop.unused.levels = TRUE)
## 3: eval(mf, parent.frame())
## 2: eval(mf, parent.frame())
## 1: lm(z ~ y)

debug(lm)
lm(y~x)


options(error = recover)
read.csv("nosuchfile")



#Wk3 Quiz
rm(list=ls())

library(datasets)
data(iris)
?iris
summary(iris)
s <- split(iris, iris$Species)
s
lapply(s, function(x) colMeans(x[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])) 

apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)
?mtcars

# sapply(mtcars, cyl, mean)
# tapply(mtcars$cyl, mtcars$mpg, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
with(mtcars, tapply(mpg, cyl, mean))
#mean(mtcars$mpg, mtcars$cyl)
tapply(mtcars$mpg, mtcars$cyl, mean)
# split(mtcars, mtcars$cyl)
# apply(mtcars, 2, mean)
# lapply(mtcars, mean)


h <- sapply(split(mtcars$hp, mtcars$cyl), mean)
h
h[3]-h[1]

