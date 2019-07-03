# Example 1
# library(modreg)
y18 <- c(1:3,5,4,7:3,2*(2:5),rep(10,4))
xx  <- seq(1,length(y18), len=201)
(s2  <- smooth.spline(y18)) # GCV
(s02 <- smooth.spline(y18, spar = 0.2))
plot(y18, main=deparse(s2$call), col.main=2)  
lines(s02, col = "orange"); 
lines(predict(s2, xx), col = 2)
lines(predict(s02, xx), col = 3); 
mtext(deparse(s02$call), col = 3)


# Example 2
data(cars)  ## N=50, n (# of distinct x) =19
attach(cars)
plot(speed, dist, main = "data(cars)  &  smoothing splines")
cars.spl <- smooth.spline(speed, dist)
cars.spl2 <- smooth.spline(speed, dist, df=10)

lines(cars.spl, col = "blue")
lines(cars.spl2, lty=2, col = "red")
lines(smooth.spline(cars, spar=0.1))  
## spar: smoothing parameter (alpha) in (0,1]
detach()

summary(smooth.spline(cars, spar=0.1))
s1=smooth.spline(cars, spar=0.1)
