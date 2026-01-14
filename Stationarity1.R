# # Stationary series examples
library(forecast)

set.seed(1)

##ARIMA(1,0,0)
St1 <- ts(replicate(n = 1, 
                 arima.sim(model = list(order = c(1, 0, 0), ar=0.7), n = 100)))

matplot(St1, 
        type = "l", 
        col = c("steelblue"), 
        lty = 1, 
        lwd = 2,
        main = "AR(1)",
        xlab = "Time",
        ylab = "Value")

tsdisplay(St1)


##ARIMA(0,0,1)
St2 <- ts(replicate(n = 1, 
                    arima.sim(model = list(order = c(0, 0, 1), ma=0.9, n = 100))))

matplot(St2, 
        type = "l", 
        col = c("steelblue"), 
        lty = 1, 
        lwd = 2,
        main = "MA(1)",
        xlab = "Time",
        ylab = "Value")

tsdisplay(St2)


##ARIMA(1,0,1)
St3 <- ts(replicate(n = 1, 
                    arima.sim(model = list(order = c(1, 0, 1), ar=0.5, ma=0.5), n = 100)))

matplot(St3, 
        type = "l", 
        col = c("steelblue"), 
        lty = 1, 
        lwd = 2,
        main = "ARMA(1,1)",
        xlab = "Time",
        ylab = "Value")

tsdisplay(St3)


##ARIMA(0,0,2)
St4 <- ts(arima.sim(model = list(order = c(0,0,2), ma = c(0.9, 0.7)), n = 100))

matplot(St4,
        type = "l",
        col = "steelblue",
        lty = 1,
        lwd = 2,
        main = "MA(2)",
        xlab = "Time",
        ylab = "Value")

tsdisplay(St4)


##ARIMA(3,0,0)
St5 <- ts(arima.sim(model = list(order = c(3,0,0), ar = c(0.6, -0.3, 0.2)), n = 100))

matplot(St5,
        type = "l",
        col = "darkgreen",
        lty = 1,
        lwd = 2,
        main = "AR(3)",
        xlab = "Time",
        ylab = "Value")

tsdisplay(St5)


##ARIMA(2,0,1)
St6 <- ts(arima.sim(model = list(order = c(2,0,1), ar = c(0.5, -0.2), ma = -0.4), n = 100))

matplot(St6,
        type = "l",
        col = "purple",
        lty = 1,
        lwd = 2,
        main = "ARIMA(2,0,1)",
        xlab = "Time",
        ylab = "Value")

tsdisplay(St6)



# Nonstationary series examples
set.seed(1)

RWs1 <- ts(replicate(n = 1, 
                    arima.sim(model = list(order = c(0, 1 ,0)), n = 100)))

matplot(RWs1, 
        type = "l", 
        col = c("steelblue", "darkred"), 
        lty = 1, 
        lwd = 2,
        main = "Random Walk",
        xlab = "Time",
        ylab = "Value")

tsdisplay(RWs1)


RWs2 <- ts(replicate(n = 1, 
                     arima.sim(model = list(order = c(1, 1 , 1), ar=0.5, ma=0.5), n = 100)))

matplot(RWs2, 
        type = "l", 
        col = c("steelblue", "darkred"), 
        lty = 1, 
        lwd = 2,
        main = "Random Walk",
        xlab = "Time",
        ylab = "Value")

tsdisplay(RWs2)
