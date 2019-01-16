
# clear the console
# click on the console and type ctrl-l

# clear the workspace
ls() # show workspace
rm(list=ls()) # clear workspace
ls() # show empty workspace

x <- 5
if(x>3) {
        y <- 10
} else {
        y <- 0
}
x ; y


x <- 5
y <- if(x>3) {10} else {5}
x ; y


for(i in 1:20) {
        print(i)
}


x <- c("a", "b", "c", "d")

for(i in 1:4) {
        print(x[i])
}

for(i in 1:4) print(x[i])

for(i in seq_along(x)) {
        print(x[i])
}

for(letter in x) {
        print(letter)
}


x <- matrix(1:6,2,3)
x

for(i in seq_len(nrow(x))) {
        for(j in seq_len(ncol(x))) {
                print(x[i,j])
        }
}


count <- 0
while(count<10) {
        print(count)
        count <- count+1
}


x<-5
while(x>=3 && x<=10) {
        print(x)
        coin <- rbinom(1,1,0.5)
        if(coin==1) {
                x <- x+1
        } else {
                x <- x-1
        }
}


x0 <- 1
tol <- 1e-8
repeat {
        x1 <- computeEstimate()
        if(abs(x1-x0) < tol) {
                break
        } else {
                x0 <- x1
        }
}


add2 <- function(x,y) {
        x+y
}
add2(3,5)

above10 <- function(x) {
        use <- x>10
        x[use]
}
x <- 1:20
above10(x)

above <- function(x,n) {
        use <- x>n
        x[use]
}
x <- 1:20
above(x,12)

above <- function(x,n=10) {
        use <- x>n
        x[use]
}
x <- 1:20
above(x)
above(x,12)


columnmeans <- function(y) {
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc) {
                means[i] <- mean(y[,i])
        }
        means
}

columnmeans(airquality)

columnmeans2 <- function(y, removena=TRUE) {
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc) {
                means[i] <- mean(y[,i], na.rm=removena)
        }
        means
}

columnmeans2(airquality, TRUE)

mydata <- rnorm(100)
mydata
sd(mydata)
sd(x=mydata, na.rm=FALSE)
sd(na.rm=FALSE, x=mydata)
sd(na.rm=FALSE, mydata)

args(lm)

lm(data=mydata, y~x, model=FALSE, 1:100)
lm(y~x, data=mydata, 1:100, model=FALSE)


make.power <- function(n) {
        pow <- function(x) {
                x^n
        }
        pow
}


cube <- make.power(3)
square <- make.power(2)

cube(3)
square(3)

ls(environment(cube))
ls(environment(square))

get("n", environment(cube))
get("n", environment(square))


y <- 10

f <- function(x) {
        y <- 2
        y^2 + g(x)
}

g <- function(x) {
        x*y
}

f(3)


make.NegLogLik <- function(data, fixed=c(FALSE,FALSE)) { 
        params <- fixed
        function(p) { 
                params[!fixed] <- p
                mu <- params[1]
                sigma <- params[2]
                a <- -0.5*length(data)*log(2*pi*sigma^2)
                b <- -0.5*sum((data-mu)^2) / (sigma^2)
                -(a+b)
        }
}

set.seed(1)
normals <- rnorm(100,1,2)

nLL <- make.NegLogLik(normals)
nLL

ls(environment(nLL))

optim(c(mu=0, sigma=1), nLL)$par

#fixing μ=2
nLL <- make.NegLogLik(normals, c(FALSE,2))
nLL
optimize(nLL,c(-1,3))$minimum

#fixing σ=1
nLL <- make.NegLogLik(normals, c(1,FALSE))
nLL
optimize(nLL,c(1e-6,10))$minimum

make.NegLogLik <- function(data, fixed=c(FALSE,FALSE)) { 
        params <- fixed
        function(p) { 
                params[!fixed] <- p
                mu <- params[1]
                sigma <- params[2]
                a <- -0.5*length(data)*log(2*pi*sigma^2)
                b <- -0.5*sum((data-mu)^2) / (sigma^2)
                -(a+b)
        }
}

set.seed(1)
normals <- rnorm(100,1,2)

nLL <- make.NegLogLik(normals, c(1,FALSE))
x <- seq(1.7, 1.9, len=100)
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type="l")

nLL <- make.NegLogLik(normals, c(FALSE, 2))
x <- seq(0.5, 1.5, len=100)
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type="l")


x <- as.Date("1970-01-01")
x

unclass(x)
unclass(as.Date("1970-01-02"))


x <- Sys.time()
x

p <- as.POSIXlt(x)
p
names(unclass(p))
p$sec
p$min


datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
datestring
x <- strptime(datestring, "%B %d, %Y %H:%M")
x
class(x)
?strptime

x <- as.Date("2012-01-01")
x
y <- strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S")
y

x-y

x <- as.POSIXlt(x)
x-y


x <- as.Date("2012-03-01") 
y <- as.Date("2012-02-28") 
x-y #note: 2012 was a leap year so difference = 2

x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 06:00:00", tz = "GMT") 
y-x #note that the difference takes into account EST vs. GMT


cube <- function(x, n) {
        x^3
}
cube(3)

x <- 1:10
if(x > 5) {
        x <- 0
}

f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}
z <- 10
f(3)

x <- 5
y <- if(x < 3) {
        NA
} else {
        10
}
y