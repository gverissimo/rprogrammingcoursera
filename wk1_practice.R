x <- 1
print(x)

x

msg <- "hello"
msg

x <- 1:20
x

y <- c(1.7, "a")
y
typeof(y)

y <- c(TRUE,2)
y
typeof(y)

y <- c("a", TRUE)
y
typeof(y)

y <- 0:6
y
typeof(y)
as.numeric(y)
as.logical(y)
as.character(y)

y <- c("a", "b", "c")
y
typeof(y)
as.numeric(y)
as.logical(y)
as.character(y)
as.complex(y)

x <- list(1,"a", TRUE, 1 + 4i)
x

m <- matrix(nrow=2, ncol=3)
m
dim(m)
attributes(m)


m <- matrix(1:6,nrow=2, ncol=3)
m
dim(m)
attributes(m)


m <- 1:10
m

dim(m) <- c(2,5)
m

dim(m) <- c(5,2)
m

x <- 1:3
x
y <- 10:12
y
cbind(x,y)
rbind(x,y)

x <- factor(c("yes","yes","no","yes","no"))
x
table(x)
unclass(x)
table(x)

x <- factor(c("yes","yes","no","yes","no"),levels = c("yes","no"))
x
table(x)
unclass(x)
table(x)

x <- c(1,2,NA,10,3)
x
is.na(x)
is.nan(x)

x <- c(1,2,NaN,NA,3)
x
is.na(x)
is.nan(x)

x <- data.frame(foo=1:4,bar=c(T,T,F,F))
x
dim(x)

x <- 1:3
x
names(x)

names(x) <- c("foo", "bar", "norf")
x
names(x)

x <- list(a=1,b=2,c=3)
x

m <- matrix(1:9, nrow=3, ncol=3)
m
dimnames(m) <- list(c("a","b","c"),c("x","y","z"))
m

con <- file("foo.csv", "r")
dta <- read.csv(con, header = FALSE)
close(con)
dta

dta <- read.csv("foo.csv", header = FALSE)
dta

con <- url("http://www.jhsph.edu", "r")
x <- readLines(con)
close(con)
head(x)

x <- c("a", "b", "c", "c", "d", "a")
x
x[1]
x[2]
x[1:4]

x <- c("a", "b", "c", "c", "d", "a")
x
x[x>"a"]

x <- c("a", "b", "c", "c", "d", "a")
u <- x > "a"
u
x[u]

x <- list(foo = 1:4, bar = 0.6)
x
x[1]
x[[1]]
x$foo
x[["foo"]]
x["foo"]

x <- list(foo = 1:4, bar = 0.6)
x

y=x[2]
y
typeof(y)

y=x[[2]]
y
typeof(y)

y=x$bar
y
typeof(y)

y=x[["bar"]]
y
typeof(y)

y=x["bar"]
y
typeof(y)

x <- list(foo = 1:4, bar = 0.6, baz = "hello")
x
x[c(1,3)]

x <- matrix(1:6,2,3)
x
x[1,2]
x[2,1]

x[1,]
x[,2]

x <- c(1, 2, NA, 4, NA, 5)
x
bad <- is.na(x)
bad
x[!bad]
x[!is.na(x)]

x <- c(1, 2, NA, 4, NA, 5)
y <- c("a", "b", NA, "d", NA, "f")
good <- complete.cases(x,y)
good
x[good]
y[good]

x <- c(1, NA, NA, 4, NA, 5)
y <- c("a", "b", NA, NA, NA, "f")
good <- complete.cases(x,y)
good
x[good]
y[good]


airquality[1:15,]
good <- complete.cases(airquality)
airquality[good, ][1:15, ]

x <- 1:4; y <- 6:9
x;y

x>2
x>=2
y==8

x+y
x*y
x/y

x <- matrix(1:4, 2, 2); y <- matrix(rep(10, 4), 2, 2)
x;y

x*y
x/y
x%*%y ## true matrix multiplication aka 'dot product'

