# 
# 
# install.packages("astsa")
require(astsa)

options(digits=2)

zardoz <- ts(rnorm(48), start=c(2293,6), frequency=12)
oz <- window(zardoz, start=2293, end=c(2295,12))


time(jj)
cycle(jj)

plot(jj)

k = c(.5,1,1,1,.5)            # k is the vector of weights
k = k/sum(k)       

fjj = filter(jj, sides=2, k)  # ?filter for help [but you knew that already]
plot(jj)
lines(fjj, col="red")         # adds a line to the existing plot
lines(lowess(jj), col="blue", lty="dashed")


dljj = diff(log(jj))        # difference the logged data
plot(dljj)                  # plot it (not shown)
shapiro.test(dljj)  

par(mfrow=c(2,1))        # set up the graphics 
hist(dljj, prob=TRUE, 12)   # histogram    
lines(density(dljj))     # smooth it - ?density for details 
qqnorm(dljj)             # normal Q-Q plot  
qqline(dljj)             # add a line   





lag.plot(dljj, 9, do.lines=FALSE)  
lag1.plot(dljj, 9)  # if you have astsa loaded (not shown) 
# why the do.lines=FALSE? Because you get a phase plane if it's TRUE 
#   a little phase plane aside - try this on your own
x = cos(2*pi*1:100/4) + .2*rnorm(100)
plot.ts(x)
dev.new()
lag.plot(x, 4)