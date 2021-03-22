################################################################
#<--------------- 1. Set Parameters ---------------->
################################################################
p.from =      "2021-01-01"        #first day to get data
p.to =        "2021-01-31"        #last day to get data
p.period =    '30min'             #candle period - daily for BatchGetSymbols() or tick, 1min, 5min, 10min, 15min, 30min, hour, day, week, month for IQFeed data
p.sma.period    <-  20
p.sd.period     <-  20
# - upper and lower limits for z-score
p.lower.zscore  <-  -2
p.upper.zscore  <-  2
# - lookforward periods for performance Measure
p.look.forward <- c(1,2,3,4,5,10,20,50)

ticker <- c("IWM") #fn.list.SP500.symbols(500) #c("SPY", "QQQ", "IWM", "AAPL", "GOOG", "AMZN", "CSCO")#

################################################################
#<--------------- 2. Download All Data ---------------->
################################################################
ifelse(
  p.period == 'daily',
    all.sym <- fn.my.batchgetsymbols(ticker, p.from, p.to),
    all.sym <- lapply(ticker, function(x)(get_iqfeed_data(x, from = p.from, to = p.to, period = p.period)))
)

# - get rid of all the tickers that don't have any data
ifelse(
  p.period == 'daily',
  ticker <- unique(all.sym$ticker),
  ticker <- ticker
)
# - Counts the number of symbols
num.symbols   <- length(unique(ticker))

# - Counts number of periods total
num.days        <- length(unique(all.sym[[2]]))

# - Separate the data frame into lists of individual symbols
ifelse(
  p.period == 'daily',
  ind.sym <- lapply(ticker,function(x)(all.sym %>% filter(ticker == x))),
  ind.sym <- lapply(seq_along(ticker),
              function(x)(
                cbind(
                  "ticker" = ticker[[x]],
                  all.sym[[x]],
                  "adjusted.close" = all.sym[[x]]$close,
                  "period.return" = all.sym[[x]]$close/lag(all.sym[[x]]$close,1,default = 0)-1
                )
              )
            )
)

# - Counts number of periods total for each symbol
num.days.ind.sym  <-  lapply(ind.sym,function(x)(length(x[[2]])))

# - Extract close from each symbol
ifelse(
  p.period == 'daily',
  adj.close <- lapply(ind.sym,function(a)(a[[6]])),
  adj.close <- lapply(ind.sym,function(a)(a[[6]]))
)

# - Extract high from each symbol
ifelse(
  p.period == 'daily',
  high <- lapply(ind.sym,function(a)(a$high)),
  high <- lapply(ind.sym,function(a)(a$high))
)

# - Extract low from each symbol
ifelse(
  p.period == 'daily',
  low <- lapply(ind.sym,function(a)(a$low)),
  low <- lapply(ind.sym,function(a)(a$low))
)

################################################################
#<--------------- 3. Add Indicator(s)  ---------------->
################################################################
#- solve for simple moving average of close
df.sma <- lapply(ind.sym, function(x)(sma(x$close,p.sma.period)))
#- solve for standard deviation of close
df.sd <- lapply(ind.sym, function(x)(roll_sd(x$close,p.sd.period)))
# - number of standard deviations that price above or below the SMA
df.zscore <- mapply(function(x,y,z)((x-y$close)/z),df.sma,ind.sym,df.sd,SIMPLIFY=F)

# - location/index where z-score exceeded lower and upper z-score bounds
df.lower.zscore <- lapply(df.zscore, function(x)(which(x <= p.lower.zscore)))
df.upper.zscore <- lapply(df.zscore, function(x)(which(x >= p.upper.zscore)))

################################################################
#<--------------- 4. Add lookforward columns ---------------->
################################################################
# - add cumulative percentage returns for look-forward periods
# - lookforward "lf"- measures from the period after the current period until the cumulative total periods indicated
# - lookforward "lf" - for example, at time 1, lf.10 would measure the return from time 2 to time 11 (because the signal con only trigger the period after it fires)
# - lookback "lb"- measures from the period before the current period backward until the cumulative total periods indicated
# - lookback "lb" - for example, at time 20, lb.10 would measure the return from time 10 to time 19

df.all.sym <-
  lapply(ind.sym, #9 columns
    function(x)(
      x %>%
        mutate(
          lf.1 = lead(close,n=2)/lead(close,n=1)-1,     #10
          lf.2 = lead(close,n=3)/lead(close,n=1)-1,     #11
          lf.3 = lead(close,n=4)/lead(close,n=1)-1,     #12
          lf.4 = lead(close,n=5)/lead(close,n=1)-1,     #13
          lf.5 = lead(close,n=6)/lead(close,n=1)-1,     #14
          lf.10 = lead(close,n=11)/lead(close,n=1)-1,   #15
          lf.20 = lead(close,n=21)/lead(close,n=1)-1,   #16
          lf.50 = lead(close,n=51)/lead(close,n=1)-1,   #17
          lb.1 = lag(close,n=2)/lag(close,n=1)-1,       #18
          lb.2 = lag(close,n=3)/lag(close,n=1)-1,       #19
          lb.3 = lag(close,n=4)/lag(close,n=1)-1,       #20
          lb.4 = lag(close,n=5)/lag(close,n=1)-1,       #21
          lb.5 = lag(close,n=6)/lag(close,n=1)-1,       #22
          lb.10 = lag(close,n=11)/lag(close,n=1)-1,     #23
          lb.20 = lag(close,n=21)/lag(close,n=1)-1,     #24
          lb.50 = lag(close,n=51)/lag(close,n=1)-1,     #25
          my.sma = sma(close,p.sma.period),             #26
          my.sd = roll_sd(close,p.sd.period),           #27
          z.score = (my.sma-close)/my.sd                #28
        )
      )
    )

df.all.sym.bind <- rbindlist(df.all.sym)

# - return all the locations of df.all.sym where z-score was less than lower level
df.lower.forward <-
  lapply(seq_along(ticker),
    function(x)(
      df.all.sym[[x]][df.lower.zscore[[x]],]
    )
  )
all.lower <- rbindlist(df.lower.forward)

# - return all the locations of df.all.sym where z-score was greater than upper level
df.upper.forward <-
  lapply(seq_along(ticker),
    function(x)(
      df.all.sym[[x]][df.upper.zscore[[x]],]
    )
  )
all.upper <- rbindlist(df.upper.forward)

# - summrize the lookforward columns
summary(all.lower[,10:17])
summary(all.upper[,10:17])

# - baseline - summarize ALL lookforward columns
summary(df.all.sym.bind[,10:17])

# - average all look forwards
rep.1 <-
  cbind(
    "lower" = round(apply(all.lower[,10:17],2,function(x)(mean(x*100,na.rm=T))),5),
    "upper" = round(apply(all.upper[,10:17],2,function(x)(mean(x*100,na.rm=T))),5),
    "all" = round(apply(df.all.sym.bind[,10:17],2,function(x)(mean(x*100,na.rm=T))),5)
  )

# - conclusion: taking all signals does "worse" than average returns over same period IF we are assuming a reversion to the mean system.  conversely, taking all signals assuming a "momentum" system does better than average
# - if you consider the

################################################################
#<--------------- 5. Where was z-score x periods before it increased y percent ---------------->
################################################################
# - split each symbol's look.forward return columns into deciles
df.all.sym.decile.returns <-
  lapply(df.all.sym,
    function(x)(
      apply(
        x[,10:17],
        2,
        function(y)(
          quantile(y, prob = seq(0, 1, length = 11), type = 5,na.rm = T)
        )
      )
    )
  )

# - where was price in relationship to zscore before top 80 percent decile return for the lookforward 5,20,50 etc?
# - df.all.sym.decile.returns[[1]][,5][[9]] - for the first symbol, return column 5 (lookforward 5), the 9th percent return (which is the 80th percentile)
# - return each row of df.all.sym where above is true
df.all.sym.top80.5period <-
  lapply(seq_along(df.all.sym),
    function(x)(
      df.all.sym[[x]][which(df.all.sym[[x]]$lf.5 >= df.all.sym.decile.returns[[x]][,5][[9]]),]
    )
  )
df.all.sym.top80.20period <-
  lapply(seq_along(df.all.sym),
    function(x)(
      df.all.sym[[x]][which(df.all.sym[[x]]$lf.20 >= df.all.sym.decile.returns[[x]][,7][[9]]),]
    )
  )
df.all.sym.top80.50period <-
  lapply(seq_along(df.all.sym),
    function(x)(
      df.all.sym[[x]][which(df.all.sym[[x]]$lf.50 >= df.all.sym.decile.returns[[x]][,8][[9]]),]
    )
  )

# - solve for deciles of z-scores
df.all.sym.decile.zscore <-
  lapply(df.all.sym,
    function(x)(
      quantile(x$z.score, prob = seq(0, 1, length = 11), type = 5,na.rm = T)
    )
  )

# - calculate returns for bottom 20th and top 80th percentile zscores
df.all.sym.bottom20.zscore <-
  lapply(seq_along(df.all.sym),
    function(x)(
      df.all.sym[[x]][which(df.all.sym[[x]]$z.score <= df.all.sym.decile.zscore[[x]][[3]]),]
    )
  )
# find the mean return for all lookforward windows that have a bottom 20 zscore
mean.bottom20.zscore <-
  lapply(df.all.sym.bottom20.zscore,
    function(x)(
      apply(
        x[,10:17],
        2,
        function(y)(
          mean(y, na.rm = T)
        )
      )
    )
  )

df.all.sym.top80.zscore <-
  lapply(seq_along(df.all.sym),
    function(x)(
      df.all.sym[[x]][which(df.all.sym[[x]]$z.score >= df.all.sym.decile.zscore[[x]][[9]]),]
    )
  )
# find the mean return for all lookforward windows that have a bottom 20 zscore
mean.top80.zscore <-
  lapply(df.all.sym.top80.zscore,
    function(x)(
      apply(
        x[,10:17],
        2,
        function(y)(
          mean(y, na.rm = T)
        )
      )
    )
  )

# find the mean return for ALL zscores
mean.all.zscore <-
  lapply(df.all.sym,
    function(x)(
      apply(
        x[,10:17],
        2,
        function(y)(
          mean(y, na.rm = T)
        )
      )
    )
  )

rep.2 <-
  lapply(seq_along(ticker),
    function(x)(
      cbind.data.frame(
        "bottom.20" = round(mean.bottom20.zscore[[x]]*100,4),
        "top.80"= round(mean.top80.zscore[[x]]*100,4),
        "all.zscore"= round(mean.all.zscore[[x]]*100,4)
      )
    )
  )

rep.3 <-
  lapply(seq_along(ticker),
    function(x)(
      rep.2[[x]] %>%
        mutate(
          diff.bottom  =
            ifelse((all.zscore > 0 & bottom.20 > 0) | (all.zscore > 0 & bottom.20 < 0),
              bottom.20/all.zscore-1,
              -1*bottom.20/all.zscore-1
            ),
          diff.top = top.80/all.zscore-1
        )
      )
    )

##### - create HTML reports for rep.3
# name of each file to export
file.names <- c("Z-Score Decile Analysis")

file.names.2 <- paste0(today()," - ",p.from," - ",p.to," - ",file.names,".html")

# creating a table with the kable() command

# - limit.period for minus and plus
k.files.rep.3 <-
        lapply(seq_along(rep.3), function(x)(
          kable_styling(
              kable_input = kable(
                              rep.3[[x]],
                              "html",
                              align = "c",
                              caption = paste0("Decile Z-Score Analysis for ",ticker[[x]]),
                            ),
              bootstrap_options = "hover",
              full_width = TRUE
            )
          ))

# creating headings for the page
head <- c("<h1> Z-Score Decile Analysis </h1>")

head2 <- paste0("<h2> For the time period: ",p.from," - ",p.to,"</h2>","<br> <h2> Date report run: ",today(),"</h2>")

jumbotron <-
  paste0(
  "<div class=\"jumbotron\">
  <h1 class=\"display-4\">",
  head,
  "</h1>
  <hr class=\"my-4\">",
  head2,
  "</div>"
)

html.basic <-
paste0(
  "<!doctype html>
  <html lang=\"en\">
  <head>
    <!-- Required meta tags -->
    <meta charset=\"utf-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">

    <!-- Bootstrap CSS -->
    <link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css\" crossorigin=\"anonymous\">

    <title></title>
  </head>",
  "<body>",
  jumbotron,
  "<br>",
  k.files.rep.3,
  "<br>",
  "</body>",
  "</html>"
)

write(html.basic, file.names.2[[1]])


################################################################
#<--------------- 6.Using Linear Regression and In-Sample & Out-of-Sample Testing ---------------->
################################################################

# - identify the in sample portion of the data (80%)
in.samp = lapply(df.all.sym,
            function(x)(
              x[1:(round(length(x[[2]])*.8)),]
            )
          )
# - ID the out-of-sample portion of the data (20%)
out.samp = lapply(df.all.sym,
            function(x)(
              x[(round(length(x[[1]])*.8)):(length(x[[1]])),]
            )
          )

# - graphing the correlations on the in-sample data
# - simple - period.return vs z-score
plot(in.samp[[1]]$z.score, in.samp[[1]]$period.return, main="Scatterplot - Period Return vs Z-Score", xlab = "Z-Score", ylab = "Period Return (%)",pch=19)
# Add fit lines
abline(lm(in.samp[[1]]$period.return~in.samp[[1]]$z.score), col="red") # regression line (y~x)

# - graph all correlations
pairs(~period.return+lb.1+lb.2+lb.3+lb.4+lb.5+lb.10+lb.20+lb.50+z.score,data=in.samp[[1]],
   main="Simple Scatterplot Matrix")

# - applying multiple linear regression to the in-sample data, use the returns over the past 10, 20 and 50 periods and the z.score to model the look-forward return over the future 20 periods)
mod.1 = lapply(in.samp,
          function(x)(
            lm(period.return~lb.1+lb.2+lb.3+lb.4+lb.5+lb.10+lb.20+lb.50+z.score, x) #
          )
        )
# - apply the model to the out-of sample data and return the fit and upper and lower prediction intervals (NOT confidence intervals)
pred.1 = lapply(seq_along(mod.1),
          function(x)(
            predict(mod.1[[x]], newdata = out.samp[[x]], interval = "prediction")
          )
        )
# - evaluate the accuracy of the model (the lower the mae, rmse, the better)
myaccuracy.1 = lapply(seq_along(pred.1),
                  function(x)(
                    accuracy(pred.1[[x]][,1],out.samp[[x]]$period.return)
                  )
                )

# - graph the predicted return, actual return and upper and lower limits for
mydata = cbind(out.samp[[1]][1:120,],pred.1[[1]][1:120,])
ggplot(mydata, aes(x = time)) +
    geom_line(aes(y = period.return, colour = "period.return")) +
    geom_line(aes(y = fit, colour = "fit")) +
    geom_line(aes(y = upr, colour = "upr")) +
    geom_line(aes(y = lwr, colour = "lwr"))

# - graph the cumulative return of actual vs predicted
mydata2 = data.frame(
  "time"=seq_along(pred.1[[1]][,1]),
  "pred"=cumsum(pred.1[[1]][,1]),
  "actual"=cumsum(out.samp[[1]]$period.return))
ggplot(mydata2, aes(x=time)) + geom_line(aes(y=actual), colour="green") + geom_line(aes(y=pred), colour="red")  # second layer
