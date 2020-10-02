#############################################################
# Random Everything - Testing What is Really Important
# Date Started:  09/28/2020
# jake.stoetzner@gmail.com
# Goal:  Is it possible to simply manage trades with profit targets, trailing stops and stop losses?
#############################################################

#<---------------- Coin Flip Stock -------------------->
set.seed(123)

# - normal distribution of positive or negative
p.sample.signal <- c(-1,1)
p.prob <- 0.5 #percent of times
p.periods <- 200 #number of periods

coin.flips <- sample(p.sample.signal,
                size = p.periods,
                replace = TRUE,
                prob = c(p.prob, 1 - p.prob))

# - lognormal distribution of returns
p.mean.log <- 0
p.sd.log <- .005
coin.returns <- rlnorm(n=p.periods,
                  meanlog = p.mean.log,
                  sdlog = p.sd.log)

# - position
coin.position <- (coin.returns-1)*coin.flips

#<---------------- Test 1: Random Entry/Random Exit for Coin-Flip Stock -------------------->
fn.coin.flip <- function(
                  p.prob = 0.5,
                  p.periods = 200,
                  p.mean.log = 0,
                  p.sd.log = .005)
    {
    a <- sample(c(-1,1),p.periods,replace = TRUE, prob = c(p.prob, 1 - p.prob))
    b <- rlnorm(p.periods, p.mean.log, p.sd.log)
    c <- (b-1)*a
    return(c)
  }

fn.final.coin.flip <- function(){
  a <- sum(fn.coin.flip(),na.rm = T)
  return(a)
}

# - find the final value of the coin flip stock 100,000 times
mc.coin.flip <- replicate(100000, fn.final.coin.flip())

# - output a summary of the monte carlo simulation and a histogram
round(summary(mc.coin.flip),5)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -0.30376 -0.04810 -0.00009 -0.00001  0.04829  0.29403

hist(mc.coin.flip,breaks = 100)

#<---------------- Test 2: Random Entry/Stop-Loss or Profit Target Exit for Coin-Flip Stock -------------------->
fn.fixed.stop.fixed.pt <- function(weight, price, tstart, tend, pstop, pprofit) {
  index = tstart : tend
  if(weight > 0){
    temp = price[ index ] < (1 - pstop) * price[ tstart ]

    # profit target
    temp = temp | price[ index ] > (1 + pprofit) * price[ tstart ]

  } else{

    temp = price[ index ] > (1 + pstop) * price[ tstart ]

    # profit target
    temp = temp | price[ index ] < (1 - pprofit) * price[ tstart ]
  }
}

fn.fixed.stop.fixed.pt.coin.flip <- function(
                            start.value = 100,
                            p.periods = 200,
                            positive.percent.stop = .01,
                            p.prob = .5,
                            p.profit = .02)
  {
  a <- fn.coin.flip(p.prob)           # - vector of returns at p.prob
  b <- cumprod(c(start.value, a + 1)) # - vector of stock prices based on those returns
  c <- first(which(fn.fixed.stop.fixed.pt(1, b, 1, p.periods, positive.percent.stop, p.profit))) # - find the index of the first instance where the trail stop fires
  c.1 <- ifelse(is.na(c) == TRUE, p.periods, c)
  d <- b[c.1] # - return the value of the index
  e <- d/start.value-1 # - exit over entry
  return(e)
}

mc.fixed.stop.fixed.pt.coin.flip <- replicate(100000,fn.fixed.stop.fixed.pt.coin.flip())
round(summary(mc.fixed.stop.fixed.pt.coin.flip),5)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -0.02965 -0.01296 -0.01085 -0.00010  0.02122  0.03993
hist(mc.fixed.stop.fixed.pt.coin.flip)

#<---------------- est 3: Random Entry/Trailing Stop (%) Exit for Coin-Flip Stock -------------------->
# ----- trailing stop: exit trade once price falls below % from max price since start of trade
fn.trailing.stop.long <- function(weight = 1, price, tstart, tend, positive.percent.stop) {
    index = tstart : tend
    if(weight > 0)
        price[ index ] < (1 - positive.percent.stop) * cummax(price[ index ])
    else
        price[ index ] > (1 + positive.percent.stop) * cummin(price[ index ])
}

fn.trail.stop.coin.flip <- function(
                            start.value = 100,
                            p.periods = 200,
                            positive.percent.stop = .01,
                            p.prob = .5,
                            p.mean.log = 0,
                            p.sd.log = .005)
  {
  a <- fn.coin.flip(p.prob) # - vector of returns at p.prob
  b <- cumprod(c(start.value, a + 1)) # - vector of stock prices based on those returns
  c <- first(which(fn.trailing.stop.long(1, b, 1, p.periods,positive.percent.stop ))) # - find the index of the first instance where the trail stop fires
  c.1 <- ifelse(is.na(c) == TRUE, p.periods, c)
  d <- b[c.1] # - return the value of the index
  e <- d/start.value-1 # - exit minus entry
  return(e)
}

mc.trail.stop.coin.flip <- replicate(100000,fn.trail.stop.coin.flip())
round(summary(mc.trail.stop.coin.flip),5)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -0.27392 -0.04873 -0.00243 -0.00015  0.04605  0.40692
hist(mc.trail.stop.coin.flip,breaks = 100)

#- "loose" 5% trailing stop
mc.trail.stop.coin.flip.5 <- replicate(100000,fn.trail.stop.coin.flip(positive.percent.stop = .05))
round(summary(mc.trail.stop.coin.flip.5),5)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -0.27392 -0.04873 -0.00243 -0.00015  0.04605  0.40692
hist(mc.trail.stop.coin.flip.5,breaks = 100)

#<---------------- Test 4: Random Entry/Trailing Stop (%) or Profit-Target(%) Exit for Coin-Flip Stock -------------------->
fn.trailing.stop.profit.target <- function(weight, price, tstart, tend, positive.percent.stop, p.profit) {
		index = tstart : tend
		if(weight > 0) {
			temp = price[ index ] < (1 - positive.percent.stop) * cummax(price[ index ])

			# profit target
			temp = temp | price[ index ] > (1 + p.profit) * price[ tstart ]
		} else {
			temp = price[ index ] > (1 + positive.percent.stop) * cummin(price[ index ])

			# profit target
			temp = temp | price[ index ] < (1 - p.profit) * price[ tstart ]
		}
		return( temp )
	}

fn.trail.stop.prof.targ.coin.flip <- function(
                            start.value = 100,
                            p.periods = 200,
                            positive.percent.stop = .01,
                            p.prob = .5,
                            p.profit = .02)
  {
  a <- fn.coin.flip(p.prob) # - vector of returns at p.prob
  b <- cumprod(c(start.value, a + 1)) # - vector of stock prices based on those returns
  c <- first(which(fn.trailing.stop.profit.target(1, b, 1, p.periods,positive.percent.stop, p.profit))) # - find the index of the first instance where the trail stop fires
  c.1 <- ifelse(is.na(c) == TRUE, p.periods, c)
  d <- b[c.1] # - return the value of the index
  e <- d/start.value-1 # - exit minus entry
  return(e)
}

mc.trail.stop.prof.targ.coin.flip <- replicate(100000,fn.trail.stop.prof.targ.coin.flip())
round(summary(mc.trail.stop.prof.targ.coin.flip),5)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#-0.02863 -0.01087 -0.00501 -0.00001  0.00669  0.04056
hist(mc.trail.stop.prof.targ.coin.flip,breaks = 100)

#<---------------- Skewing the Coin Flip in Your Favor -------------------->
mc.trail.stop.coin.flip.skew <- replicate(100000,fn.trail.stop.coin.flip(p.prob = .47))
round(summary(mc.trail.stop.coin.flip.skew),5)

#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#
-0.26530 -0.04847 -0.00219  0.00035  0.04635  0.33444
sum(unlist(mc.trail.stop.coin.flip.skew))
# [1] 34.67538

#<---------------- Testing on Actual Stock Data -------------------->
library(BatchGetSymbols)

# set dates
first.date <- Sys.Date() - 360
last.date <- Sys.Date()
freq.data <- 'daily'

# set tickers
df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

l.out.grouped <- l.out$df.tickers %>% group_by(ticker)

#<---------------- Test #1: Random Entry/Random Exit for Actual Stock Data -------------------->
fn.rand.exit.actual.data <- function(data = l.out$df.tickers,
                                    max.periods = 100,
                                    same.ticker = TRUE)
                                    {
  a <- round(runif(1, min = 1, max = length(data$price.open))) # - random entry index
  b <- round(runif(1, min = a, max = length(data$price.open))) # - random exit index after entry
  c <- first(which(data$ticker[a:b] != lag(data$ticker[a:b],1))) # - where stock data changes
  d <- min(c,max.periods)
  e <- ifelse(same.ticker == TRUE,
        ifelse(data$ticker[a] == data$ticker[b], # - if ticker is the same for both
          sum(data$ret.closing.prices[a:b], na.rm = T), # - sum of all returns
          sum(data$ret.closing.prices[a:(a+d)], na.rm = T)
        ),
        sum(data$ret.closing.prices[a:b], na.rm = T)
      )
  return(e)
}

mc.rand.exit.actual.data <- replicate(100000,fn.rand.exit.actual.data())

round(summary(mc.rand.exit.actual.data),5)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#-1.64571 -0.09913  0.00626  0.01088  0.13075  1.59938

# - add in a custom function that plots a uniform distribution over the actual histogram
fn.uniform.hist <- function(data,
                    breaks, xlab = "Percent Return", main = "Frequency of Returns")
    {
  h <- hist(data, breaks = breaks, density = breaks, col = "gray", xlab = xlab, main = main)
  xfit <- seq(min(data), max(data), length = breaks)
  yfit <- dnorm(xfit, mean = mean(data), sd = sd(data))
  yfit <- yfit * diff(h$mids[1:2]) * length(data)
  lines(xfit, yfit, col = "black", lwd = 2)
}

mc.rand.exit.actual.data.10 <- replicate(10000,fn.rand.exit.actual.data(max.periods = 10))

round(summary(mc.rand.exit.actual.data.10),5)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -1.09355 -0.02947  0.01046  0.00505  0.05168  0.68247

fn.uniform.hist(mc.rand.exit.actual.data.10, breaks = 200, xlab = "Percent Return", main = "Frequency of Returns - Actual Data | Random Exit | Max Hold 10")

#<---------------- Test #2: Random Entry/ Stop-Loss (%) or Profit-Target (%) Exit for Actual Stock Data -------------------->
fn.fixed.stop.fixed.pt.actual.data <- function(data = l.out$df.tickers,
                                    max.periods = 1000,
                                    same.ticker = TRUE,
                                    positive.percent.stop = .025,
                                    p.profit = .05){
  a <- round(runif(1, min = 1, max = length(data$price.open))) # - random entry index

  b <- length(data$price.open) # - end of the data vector

  c <- first(which(data$ticker[a:b] != lag(data$ticker[a:b],1))) # - where stock ticker changes as measured from the random entry

  d <- min(c,max.periods, na.rm = T) # - return length lesser of stock ticker change OR max hold periods

  a.1 <- first(which(fn.fixed.stop.fixed.pt(1, data$price.open, a, (a+d), positive.percent.stop, p.profit ))) # - return index of where stop loss or profit target fired, as measured from random entry to lesser of stock ticker change OR max hold periods

  a.2 <- min((a + a.1), min((a+d), b),na.rm = T) # - min exit index from entry (trail.stop, change of ticker, max.periods, full length of vector)

  #e <- data$price.open[[a.2]]/data$price.open[[a]]-1

  e <- sum(data$ret.closing.prices[a:a.2], na.rm = T) # - sum of all returns

  return(e)
}

# - mc analysis with max.periods at 1000, stop at 2.5% and profit target at 5%
mc.fixed.stop.fixed.pt.actual.data <- replicate(10000,fn.fixed.stop.fixed.pt.actual.data())

round(summary(mc.trail.stop.actual.data),5)

fn.uniform.hist(mc.fixed.stop.fixed.pt.actual.data, breaks = 200, xlab = "Percent Return", main = "Frequency of Returns - Actual Data | Stop Loss and Profit Target")fn.fixed.stop.fixed.pt.actual.data <- function(data = l.out$df.tickers,
                                    max.periods = 1000,
                                    same.ticker = TRUE,
                                    positive.percent.stop = .025,
                                    p.profit = .05){
  a <- round(runif(1, min = 1, max = length(data$price.open))) # - random entry index

  b <- length(data$price.open) # - end of the data vector

  c <- first(which(data$ticker[a:b] != lag(data$ticker[a:b],1))) # - where stock ticker changes as measured from the random entry

  d <- min(c,max.periods, na.rm = T) # - return length lesser of stock ticker change OR max hold periods

  a.1 <- first(which(fn.fixed.stop.fixed.pt(1, data$price.open, a, (a+d), positive.percent.stop, p.profit ))) # - return index of where stop loss or profit target fired, as measured from random entry to lesser of stock ticker change OR max hold periods

  a.2 <- min((a + a.1), min((a+d), b),na.rm = T) # - min exit index from entry (trail.stop, change of ticker, max.periods, full length of vector)

  #e <- data$price.open[[a.2]]/data$price.open[[a]]-1

  e <- sum(data$ret.closing.prices[a:a.2], na.rm = T) # - sum of all returns

  return(e)
}

# - mc analysis with max.periods at 1000, stop at 2.5% and profit target at 5%
mc.fixed.stop.fixed.pt.actual.data <- replicate(10000,fn.fixed.stop.fixed.pt.actual.data())

round(summary(mc.trail.stop.actual.data),5)

fn.uniform.hist(mc.fixed.stop.fixed.pt.actual.data, breaks = 200, xlab = "Percent Return", main = "Frequency of Returns - Actual Data | Stop Loss and Profit Target")fn.fixed.stop.fixed.pt.actual.data <- function(data = l.out$df.tickers,
                                    max.periods = 1000,
                                    same.ticker = TRUE,
                                    positive.percent.stop = .025,
                                    p.profit = .05){
  a <- round(runif(1, min = 1, max = length(data$price.open))) # - random entry index

  b <- length(data$price.open) # - end of the data vector

  c <- first(which(data$ticker[a:b] != lag(data$ticker[a:b],1))) # - where stock ticker changes as measured from the random entry

  d <- min(c,max.periods, na.rm = T) # - return length lesser of stock ticker change OR max hold periods

  a.1 <- first(which(fn.fixed.stop.fixed.pt(1, data$price.open, a, (a+d), positive.percent.stop, p.profit ))) # - return index of where stop loss or profit target fired, as measured from random entry to lesser of stock ticker change OR max hold periods

  a.2 <- min((a + a.1), min((a+d), b),na.rm = T) # - min exit index from entry (trail.stop, change of ticker, max.periods, full length of vector)

  #e <- data$price.open[[a.2]]/data$price.open[[a]]-1

  e <- sum(data$ret.closing.prices[a:a.2], na.rm = T) # - sum of all returns

  return(e)
}

# - mc analysis with max.periods at 1000, stop at 2.5% and profit target at 5%
mc.fixed.stop.fixed.pt.actual.data <- replicate(10000,fn.fixed.stop.fixed.pt.actual.data())

round(summary(mc.trail.stop.actual.data),5)

fn.uniform.hist(mc.fixed.stop.fixed.pt.actual.data, breaks = 200, xlab = "Percent Return", main = "Frequency of Returns - Actual Data | Stop Loss and Profit Target")

#<---------------- Test #3: Random Entry/Trailing Stop Exit on Actual Stock Data -------------------->
fn.trail.stop.actual.data <- function(data = l.out$df.tickers,
                                    max.periods = 1000,
                                    same.ticker = TRUE,
                                    positive.percent.stop = .1){
  a <- round(runif(1, min = 1, max = length(data$price.open))) # - random entry index

  b <- length(data$price.open) # - end of the vector

  c <- first(which(data$ticker[a:b] != lag(data$ticker[a:b],1))) # - where stock ticker changes as measured from the random entry

  d <- min(c,max.periods, na.rm = T)

  a.1 <- first(which(fn.trailing.stop.long(1, data$price.open, a, (a+d), positive.percent.stop ))) # - find the index of the first instance where the trail stop fires a measured from rand entry to lesser of where stock ticker changes or the max hold period

  a.2 <- min((a + a.1), min((a+d), b),na.rm = T) # - min exit index (trail.stop, change of ticker, max.periods, full length)

  #e <- data$price.open[[a.2]]/data$price.open[[a]]-1

  e <- sum(data$ret.closing.prices[a:a.2], na.rm = T) # - sum of all returns

  return(e)
}

# - mc analysis 100k runs - with max.periods at 100 and positive.percent stop at 5%
# - to compare with mc.random.exit.actual.data
mc.trail.stop.actual.data.comp <- replicate(100000,fn.trail.stop.actual.data(max.periods = 100, positive.percent.stop = .05))

round(summary(mc.trail.stop.actual.data.comp),5)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#-0.90035 -0.05301 -0.01184 -0.00114  0.04277  0.88775

#<---------------- Test #4: Random Entry/Trailing Stop (%) or Profit-Target (%) Exit on Actual Stock Data -------------------->
fn.trail.stop.profit.target.actual.data <- function(data = l.out$df.tickers,
                                    max.periods = 1000,
                                    same.ticker = TRUE,
                                    positive.percent.stop = .025,
                                    p.profit = .05){
  a <- round(runif(1, min = 1, max = length(data$price.open))) # - random entry index

  b <- length(data$price.open) # - end of the vector

  c <- first(which(data$ticker[a:b] != lag(data$ticker[a:b],1))) # - where stock ticker changes as measured from the random entry

  d <- min(c,max.periods, na.rm = T)

  a.1 <- first(which(fn.trailing.stop.profit.target(1, data$price.open, a, (a+d), positive.percent.stop, p.stop ))) # - find the index of the first instance where the trail stop fires a measured from rand entry to lesser of where stock ticker changes or the max hold period

  a.2 <- min((a + a.1), min((a+d), b),na.rm = T) # - min exit index (trail.stop, change of ticker, max.periods, full length)

  #e <- data$price.open[[a.2]]/data$price.open[[a]]-1

  e <- sum(data$ret.closing.prices[a:a.2], na.rm = T) # - sum of all returns

  return(e)
}

# - mc analysis 100k runs - with max.periods at 100, positive.percent stop at 2.5% and profit target at 5%
# - to compare with mc.random.exit.actual.data
mc.trail.stop.profit.target.actual.data <- replicate(100000,fn.trail.stop.profit.target.actual.data(max.periods = 100))

# round(summary(mc.trail.stop.profit.target.actual.data),5)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#-0.80554 -0.02529  0.00801  0.00185  0.03080  0.82386

#<---------------- Momentum, Mean Reversion, Runs and Random-Walk Theory -------------------->
# - set values for an up day equal to 1, down day to -1 and a flat day to 0
a <- ifelse(l.out$df.tickers$ret.closing.prices > 0, 1, ifelse(l.out$df.tickers$ret.closing.prices < 0, -1, 0))

# - replace all NA values with 0
a[is.na(a)] <- 0

# - count the streaks of up days, down days and zero days for all of the data
b <- rle(a)

# - return a summary of the runs of up days
summary(b$lengths[b$values == 1])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  1.000   1.000   1.000   2.009   3.000  17.000

# - return a summary of the runs of down days
summary(b$lengths[b$values == -1])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  1.000   1.000   1.000   1.835   2.000  12.000

# - total runs - positive
length(b$lengths[b$values == 1])
# [1] 31835
length(b$lengths[b$values == -1])
# [1] 31818

pos.runs <- unlist(lapply(1:17, function(x)(length(b$lengths[b$values == 1 & b$lengths == x])/length(b$lengths))))

neg.runs <- unlist(lapply(1:17, function(x)(length(b$lengths[b$values == -1 & b$lengths == x])/length(b$lengths))))

tot.runs <- unlist(lapply(1:17, function(x)(length(b$lengths[b$lengths == x])/length(b$lengths))))

run.table <- cbind(
  "Run Length" = c(1:17),
  "Pos Runs - Count as % of All Runs" = round(pos.runs*100,2),
  "Neg Runs - Count as % of All Runs" = round(neg.runs*100,2),
  "Both Pos & Neg Runs - Count as % of All Runs" = round(tot.runs*100,2)
  )

library(knitr)
kable(run.table)

# - find actual number of observations (removing 0 values)
n <- length(a[a != 0])

# - expected runs from random walk
exp.runs <- n/2

# - actual runs (removing 0 values)
actual.runs <- length(b$lengths[b$value != 0])

# - sd of runs and critical value
sd.exp.runs <- sqrt(n)/2
critical.value <- 1.64*sd.exp.runs

# - solve for the critical value lower limit
lower.value <- exp.runs - critical.value

> lower.value
[1] 60893.66
> actual.runs
[1] 63653

#<---------------- Test #1: Random Entry/Random Exit for Actual Stock Data -------------------->
mc.random.exit.skew <- replicate(100000, sum(fn.coin.flip(p.mean.log = 0.01), na.rm = T))
summary(mc.random.exit.skew)
      Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
-0.7214696 -0.1073840 -0.0003747 -0.0004084  0.1074432  0.7032947

mc.trail.stop.coin.flip.skew.2 <- replicate(100000, fn.trail.stop.coin.flip(p.mean.log = .01, positive.percent.stop = .1))
summary(mc.trail.stop.coin.flip.skew.2)
      Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
-0.1132679 -0.0552802 -0.0035014  0.0003675  0.0467034  0.3221581
#<---------------- Test #1: Random Entry/Random Exit for Actual Stock Data -------------------->
