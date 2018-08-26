# Hybrid ARIMA - ANN

install.packages("tseries")
library(tseries)  

install.packages("forecast")
library(forecast)

install.packages("neuralnet")
library(neuralnet)

dollar_euro<-read.csv("C:/Users/admin/Desktop/dollar-euro-lek.csv", sep=";", dec=",", header=T)
str(dollar_euro)
dollar_lek<-ts(dollar_euro[,2], start=c(2000,1), frequency=12)
plot(dollar_lek, main="EVOLUTION OF THE DOLLAR/LEK RATE", col="blue", font.main=4, col.main="blue",cex.main=1.5)

decomp_dollar<- stl(dollar_lek, t.window=15, s.window="periodic", robust=TRUE)
plot(decomp_dollar)


ADF_dollar<-adf.test(dollar_lek, alternative = "stationary",k = trunc((length(dollar_lek)-1)^(1/3)))
ADF_diff_dollar<-adf.test(diff(dollar_lek), alternative = "stationary",k = trunc((length(dollar_lek)-1)^(1/3)))

tsdisplay(diff(dollar_lek,12))
auto.arima(dollar_lek, d = 1, D=1,max.p = 5, max.q = 5, max.P = 2,
  max.Q = 2, max.order = 5,  max.D = 1, start.p = 2,
  start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
  seasonal = TRUE, ic = c("aicc", "aic", "bic"), stepwise = TRUE,
  trace = TRUE)

fit_dollar<- Arima(dollar_lek, order=c(2,1,1), seasonal=c(2,1,1))
fit_dollar
fit_arima_coef<-coef(fit_dollar)
fit_arima_coef
fit_arima_varcoef<-fit_dollar$var.coef
fit_arima_varcoef
fit_arima_sigma2<-fit_dollar$sigma2
fit_arima_sigma2

(1-pnorm(abs(fit_arima_coef)/sqrt(diag(fit_arima_varcoef))))*2

res_dollar <- residuals(fit_dollar)
tsdisplay(res_dollar)
Box.test(res_dollar, lag=16, fitdf=12, type="Ljung")
jarque.bera.test(res_dollar)
plot(fit_dollar)
plot(forecast(fit_dollar ,h=12))
residuals_dollar<-data.frame(res_dollar)

ts.plot(fit_dollar$fitted, dollar_lek,  gpars = list(col = c("red", "blue"), ylab = "dollar_lek",
 main = "SARIMA vs. Original data"))
legend(2007, 140, legend=c("SARIMA", "Original DATA"),
       col=c("red", "blue"), lty=1:2, cex=1)

accuracy(fit_dollar$fitted, dollar_lek)


fit_nar_dollar <- nnetar(dollar_lek, p=2, P = 2, size=2)
plot(forecast(fit_nar_dollar,h=20))
summary(fit_nar_dollar)
fitted_nar<-fit_nar_dollar$fitted

ts.plot(fitted_nar, dollar_lek,  gpars = list(col = c("red", "blue"), ylab = "dollar_lek",
 main = "NAR vs. Original data"))
legend(2007, 140, legend=c("NAR","Original DATA"),
       col=c("red", "blue"), lty=1:2, cex=1)

accuracy(fitted_nar, dollar_lek)

dollar_lek<-data.frame(dollar_lek)
dollar_lek1<-data.frame(dollar_lek)
dollar_lek1<-window(dollar_lek[1:192,1], start=40)
dollar_lek_lag1<-window(dollar_lek[2:192,1], start=39)
dollar_lek_lag2<-window(dollar_lek[3:192,1], start=38)
dollar_lek_lag12<-window(dollar_lek[13:192,1], start=28)
dollar_lek_lag24<-window(dollar_lek[25:192,1], start=16)

data_nar_dollar<-data.frame(dollar_lek1,dollar_lek_lag1,dollar_lek_lag2,dollar_lek_lag12,dollar_lek_lag24)

data_nar_dollar<-scale(data_nar_dollar)

formula1<-dollar_lek1~dollar_lek_lag1+dollar_lek_lag2+dollar_lek_lag12+dollar_lek_lag24

Architecture_NAR_dollar<-plot(neuralnet(formula1, data_nar_dollar,hidden = 3, threshold = 0.01,
stepmax = 1e+05, rep = 1, startweights = NULL,
learningrate.limit = NULL,
learningrate.factor = list(minus = 0.5, plus = 1.2),
learningrate=NULL, lifesign = "none",
lifesign.step = 1000, algorithm = "rprop+",
err.fct = "sse", act.fct = "logistic",
linear.output = TRUE, exclude = NULL,
constant.weights = NULL, likelihood = FALSE))



dollar_lek<-data.frame(dollar_lek)
dollar_lek_lag1<-dollar_lek[2:192,1]
dollar_lek_lag2<-dollar_lek[3:192,1]
dollar_lek_lag3<-dollar_lek[4:192,1]
dollar_lek_lag12<-dollar_lek[13:192,1]
dollar_lek_lag13<-dollar_lek[14:192,1]
dollar_lek_lag14<-dollar_lek[15:192,1]
dollar_lek_lag15<-dollar_lek[16:192,1]
dollar_lek_lag24<-dollar_lek[25:192,1]
dollar_lek_lag25<-dollar_lek[26:192,1]
dollar_lek_lag26<-dollar_lek[27:192,1]	
dollar_lek_lag27<-dollar_lek[28:192,1]
dollar_lek_lag28<-dollar_lek[29:192,1]
dollar_lek_lag36<-dollar_lek[37:192,1]
dollar_lek_lag37<-dollar_lek[38:192,1]
dollar_lek_lag38<-dollar_lek[39:192,1]
dollar_lek_lag39<-dollar_lek[40:192,1]
residuals_lek_lag1<-residuals_dollar[2:192,1]
residuals_lek_lag12<-residuals_dollar[13:192,1]
residuals_lek_lag13<-residuals_dollar[14:192,1]


dollar_lek1<-data.frame(dollar_lek)
dollar_lek1<-window(dollar_lek[1:192,1], start=40)
dollar_lek_lag1<-window(dollar_lek[2:192,1], start=39)
dollar_lek_lag2<-window(dollar_lek[3:192,1], start=38)
dollar_lek_lag3<-window(dollar_lek[4:192,1], start=37)
dollar_lek_lag12<-window(dollar_lek[13:192,1], start=28)
dollar_lek_lag13<-window(dollar_lek[14:192,1], start=27)
dollar_lek_lag14<-window(dollar_lek[15:192,1], start=26)
dollar_lek_lag15<-window(dollar_lek[16:192,1], start=25)
dollar_lek_lag24<-window(dollar_lek[25:192,1], start=16)
dollar_lek_lag25<-window(dollar_lek[26:192,1], start=15)
dollar_lek_lag26<-window(dollar_lek[27:192,1], start=14)
dollar_lek_lag27<-window(dollar_lek[28:192,1], start=13)
dollar_lek_lag28<-window(dollar_lek[29:192,1], start=12)
dollar_lek_lag36<-window(dollar_lek[37:192,1], start=4)
dollar_lek_lag37<-window(dollar_lek[38:192,1], start=3)
dollar_lek_lag38<-window(dollar_lek[39:192,1], start=2)
dollar_lek_lag39<-window(dollar_lek[40:192,1], start=1)
residuals_lek_lag1<-window(residuals_dollar[2:192,1], start=39)
residuals_lek_lag12<-window(residuals_dollar[13:192,1], start=28)
residuals_lek_lag13<-window(residuals_dollar[14:192,1], start=27)


data_hybrid_dollar<-data.frame(dollar_lek1,dollar_lek_lag1,dollar_lek_lag2,dollar_lek_lag3,dollar_lek_lag12,
dollar_lek_lag13,dollar_lek_lag14,dollar_lek_lag15,dollar_lek_lag24,dollar_lek_lag25,dollar_lek_lag26,dollar_lek_lag27
,dollar_lek_lag28,dollar_lek_lag36,dollar_lek_lag37,dollar_lek_lag38,dollar_lek_lag39,residuals_lek_lag1,residuals_lek_lag12,residuals_lek_lag13)

data_hybrid_dollar<-scale(data_hybrid_dollar)

formula<-dollar_lek1~dollar_lek_lag1+dollar_lek_lag2+dollar_lek_lag3+dollar_lek_lag12+dollar_lek_lag13+dollar_lek_lag14+
dollar_lek_lag15+dollar_lek_lag24+dollar_lek_lag25+dollar_lek_lag26+dollar_lek_lag27+dollar_lek_lag28+dollar_lek_lag36+
dollar_lek_lag37+dollar_lek_lag38+dollar_lek_lag39+residuals_lek_lag1+residuals_lek_lag12+residuals_lek_lag13


hybrid_ARIMA_ANN_dollar<-neuralnet(formula, data_hybrid_dollar,hidden = 3, threshold = 0.01,
stepmax = 1e+05, rep = 1, startweights = NULL,
learningrate.limit = NULL,
learningrate.factor = list(minus = 0.5, plus = 1.2),
learningrate=NULL, lifesign = "none",
lifesign.step = 1000, algorithm = "rprop+",
err.fct = "sse", act.fct = "logistic",
linear.output = TRUE, exclude = NULL,
constant.weights = NULL, likelihood = FALSE)

err.fct<-hybrid_ARIMA_ANN_dollar$err.fct
act.fct<-hybrid_ARIMA_ANN_dollar$act.fct
weights<-hybrid_ARIMA_ANN_dollar$weights

plot(hybrid_ARIMA_ANN_dollar)

stand_prediction_hybrid<-hybrid_ARIMA_ANN_dollar$net.result
stand_prediction_hybrid
stand_prediction_hybrid<-data.frame(stand_prediction_hybrid)
mean<-mean(dollar_lek1)
sd<-sd(dollar_lek1)
prediction_hybrid<-stand_prediction_hybrid*sd+mean
prediction_hybrid<-ts(prediction_hybrid, start=c(3,2003), frequency=12)
dollar_lek1<-ts(dollar_lek1, start=c(3,2003), frequency=12)

ts.plot(prediction_hybrid,dollar_lek1,  gpars = list(col = c("red", "blue"), ylab = "dollar_lek",
 main = "Hybrid ARIMA_ANN s. original data"))
legend(2007, 140, legend=c("hybrid ARIMA_ANN", "original data"),col=c("red", "blue"), lty=1:2, cex=1)

accuracy(prediction_hybrid,dollar_lek1)

dollar_euro<-read.csv("C:/Users/OUTMAN/Desktop/dollar-euro-lek.csv", sep=";", dec=",", header=T)
str(dollar_euro)
euro_lek<-ts(dollar_euro[,3], start=c(2000,1), frequency=12)
plot(euro_lek, main="EVOLUTION OF THE EURO/LEK RATE", col="green", font.main=4, col.main="green",cex.main=1.5)

decomp_euro<- stl(euro_lek, t.window=15, s.window="periodic", robust=TRUE)
plot(decomp_euro)


ADF_euro<-adf.test(euro_lek, alternative = "stationary",k = trunc((length(euro_lek)-1)^(1/3)))
ADF_diff_euro<-adf.test(diff(euro_lek), alternative = "stationary",k = trunc((length(euro_lek)-1)^(1/3)))
ADF_euro
ADF_diff_euro

tsdisplay(diff(euro_lek,12))
auto.arima(euro_lek, d = 1, D=1, max.p = 5, max.q = 5, max.P = 2,
  max.Q = 2, max.order = 5,  max.D = 1, start.p = 2,
  start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
  seasonal = TRUE, ic = c("aicc", "aic", "bic"), stepwise = TRUE,
  trace = TRUE)


fit_euro<- Arima(euro_lek, order=c(0,1,2), seasonal=c(2,1,0))
fit_euro
fit_arima_euro_coef<-coef(fit_euro)
fit_arima_euro_coef
fit_arima_euro_varcoef<-fit_euro$var.coef
fit_arima_euro_varcoef
fit_arima_euro_sigma2<-fit_euro$sigma2
fit_arima_euro_sigma2

(1-pnorm(abs(fit_arima_euro_coef)/sqrt(diag(fit_arima_euro_varcoef))))*2

res_euro<- residuals(fit_euro)
tsdisplay(res_euro)
Box.test(res_euro, lag=16, fitdf=12, type="Ljung")
jarque.bera.test(res_euro)
plot(fit_euro)
plot(forecast(fit_euro ,h=12))
residuals_euro<-data.frame(res_euro)

ts.plot(fit_euro$fitted, euro_lek,  gpars = list(col = c("red", "blue"), ylab = "euro_lek",
 main = "SARIMA vs. Original data"))
legend(2007, 140, legend=c("SARIMA", "Original DATA"),
       col=c("red", "blue"), lty=1:2, cex=1)

accuracy(fit_euro$fitted, euro_lek)

fit_euro <- Arima(euro_lek, order=c(0,1,2), seasonal=c(2,1,0))
res_euro <- residuals(fit_euro)
tsdisplay(res_euro)
Box.test(res_euro, lag=16, fitdf=12, type="Ljung")
plot(fit_euro)
plot(forecast(fit_euro,h=12))

fit_nar_euro <- nnetar(euro_lek, p=0, P = 2, lambda=TRUE)
fit_nar_euro 
plot(forecast(fit_nar_euro,h=9))
summary(fit_nar_euro)
fitted_nar_euro<-fit_nar_euro$fitted

ts.plot(fitted_nar_euro, euro_lek,  gpars = list(col = c("red", "blue"), ylab = "euro_lek",
 main = "NAR vs. Original data"))
legend(2007, 140, legend=c("NAR","Original DATA"),
       col=c("red", "blue"), lty=1:2, cex=1)

accuracy(fitted_nar_euro, euro_lek)

euro_lek<-data.frame(euro_lek)
euro_lek1<-data.frame(euro_lek)
euro_lek1<-window(euro_lek[1:192,1], start=40)
euro_lek_lag1<-window(euro_lek[2:192,1], start=39)
euro_lek_lag12<-window(euro_lek[13:192,1], start=28)
euro_lek_lag13<-window(euro_lek[14:192,1], start=27)
euro_lek_lag24<-window(euro_lek[25:192,1], start=16)
euro_lek_lag25<-window(euro_lek[26:192,1], start=15)
euro_lek_lag36<-window(euro_lek[37:192,1], start=4)
euro_lek_lag37<-window(euro_lek[38:192,1], start=3)
residuals_lek_lag1<-window(residuals_euro[2:192,1], start=39)
residuals_lek_lag2<-window(residuals_euro[3:192,1], start=38)


data_hybrid_euro<-data.frame(euro_lek1,euro_lek_lag1, euro_lek_lag12, euro_lek_lag13, euro_lek_lag24,euro_lek_lag25,  euro_lek_lag36,euro_lek_lag37, residuals_lek_lag1, residuals_lek_lag2)

data_hybrid_euro<-scale(data_hybrid_euro)

formula<-euro_lek1~euro_lek_lag1+ euro_lek_lag12+euro_lek_lag13+euro_lek_lag24+euro_lek_lag25+euro_lek_lag36+ euro_lek_lag37 +residuals_lek_lag1+ residuals_lek_lag2


hybrid_ARIMA_ANN_euro<-neuralnet(formula, data_hybrid_euro,hidden = 3, threshold = 0.01,
stepmax = 1e+05, rep = 1, startweights = NULL,
learningrate.limit = NULL,
learningrate.factor = list(minus = 0.5, plus = 1.2),
learningrate=NULL, lifesign = "none",
lifesign.step = 1000, algorithm = "rprop+",
err.fct = "sse", act.fct = "logistic",
linear.output = TRUE, exclude = NULL,
constant.weights = NULL, likelihood = FALSE)

err.fct<-hybrid_ARIMA_ANN_euro$err.fct
act.fct<-hybrid_ARIMA_ANN_euro$act.fct
weights<-hybrid_ARIMA_ANN_euro$weights

plot(hybrid_ARIMA_ANN_euro)

stand_prediction_hybrid<-hybrid_ARIMA_ANN_euro$net.resul
stand_prediction_hybrid
stand_prediction_hybrid<-data.frame(stand_prediction_hybrid)
mean<-mean(euro_lek1)
sd<-sd(euro_lek1)
prediction_hybrid<-stand_prediction_hybrid*sd+mean
prediction_hybrid<-ts(prediction_hybrid, start=c(2003,3), frequency=12)
euro_lek1<-ts(euro_lek1, start=c(2003,3), frequency=12)

ts.plot(prediction_hybrid,euro_lek1,  gpars = list(col = c("red", "blue"), ylab = "euro_lek",
 main = "Hybrid ARIMA_ANN s. original data"))
legend(2009, 130, legend=c("hybrid ARIMA_ANN", "original data") ,col=c("red", "blue"), lty=1:2, cex=1)

accuracy(prediction_hybrid,euro_lek1)

IR<-read.csv("C:/Users/OUTMAN/Desktop/IR.csv", sep=";", dec=",", header=T)
str(IR)
IR<-ts(IR[,2], start=c(1995,8), frequency=12)
plot(IR, main="EVOLUTION OF THE INTEREST RATE", col="red", font.main=4, col.main="red",cex.main=1.5)

decomp_IR<- stl(IR, t.window=15, s.window="periodic", robust=TRUE)
plot(decomp_IR)

ADF_IR<-adf.test(IR, alternative = "stationary",k = trunc((length(IR)-1)^(1/3)))
ADF_diff_IR<-adf.test(diff(IR), alternative = "stationary",k = trunc((length(IR)-1)^(1/3)))
ADF_IR
ADF_diff_IR


tsdisplay(diff(IR,1))
tsdisplay(diff(IR,13))
auto.arima(IR, d = 1,D=1,max.p = 2, max.q = 5, max.P = 1,
  max.Q =1, max.order = 5,  max.D = 1, start.p = 2,
  start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
  seasonal = TRUE, ic = c("aicc", "aic", "bic"), stepwise = TRUE,
  trace = TRUE)


fit_IR<- Arima(IR, order=c(2,1,1), seasonal=c(1,1,0))
fit_IR
fit_arima_IR_coef<-coef(fit_IR)
fit_arima_IR_coef
fit_arima_IR_varcoef<-fit_IR$var.coef
fit_arima_IR_varcoef
fit_arima_IR_sigma2<-fit_IR$sigma2
fit_arima_IR_sigma2

(1-pnorm(abs(fit_arima_IR_coef)/sqrt(diag(fit_arima_IR_varcoef))))*2

res_IR<- residuals(fit_IR)
tsdisplay(res_IR)
Box.test(res_IR, lag=9, fitdf=3, type="Ljung")
jarque.bera.test(res_IR)
plot(fit_IR)
plot(forecast(fit_IR ,h=20))
residuals_IR<-data.frame(res_IR)

ts.plot(fit_IR$fitted, IR,  gpars = list(col = c("red", "blue"), ylab = "IR",
 main = "SARIMA vs. Original data"))
legend(2010, 25, legend=c("SARIMA", "Original DATA"),
       col=c("red", "blue"), lty=1:2, cex=1)

accuracy(fit_IR$fitted, IR)

fit_nar_IR <- nnetar(IR, p=2, P = 1, lambda=TRUE)
fit_nar_IR
plot(forecast(fit_nar_IR,h=20))
summary(fit_nar_IR)
fitted_nar_IR<-fit_nar_IR$fitted

ts.plot(fitted_nar_IR, IR,  gpars = list(col = c("red", "blue"), ylab = "IR",
 main = "NAR vs. Original data"))
legend(2010, 25, legend=c("NAR","Original DATA"),
       col=c("red", "blue"), lty=1:2, cex=1)

IR<-data.frame(IR)
IR2<-window(IR[1:231,1], start=13)
IR_lag1_NAR<-window(IR[2:231,1], start=12)
IR_lag2_NAR<-window(IR[3:231,1], start=11)
IR_lag12_NAR<-window(IR[13:231,1], start=1)

data_NAR_IR<-data.frame(IR2,IR_lag1_NAR,IR_lag2_NAR ,IR_lag12_NAR)

data_NAR_IR<-scale(data_NAR_IR)



formula_NAR<-IR2~IR_lag1_NAR+IR_lag2_NAR+ IR_lag12_NAR

archtecture_NAR_IR<-plot(neuralnet(formula_NAR, data_NAR_IR,hidden = 2, threshold = 0.01,
stepmax = 1e+05, rep = 1, startweights = NULL,
learningrate.limit = NULL,
learningrate.factor = list(minus = 0.5, plus = 1.2),
learningrate=NULL, lifesign = "none",
lifesign.step = 1000, algorithm = "rprop+",
err.fct = "sse", act.fct = "logistic",
linear.output = TRUE, exclude = NULL,
constant.weights = NULL, likelihood = FALSE))








IR<-data.frame(IR)
IR1<-window(IR[1:231,1], start=27)
IR_lag1<-window(IR[2:231,1], start=26)
IR_lag2<-window(IR[3:231,1], start=25)
IR_lag12<-window(IR[13:231,1], start=15)
IR_lag13<-window(IR[14:231,1], start=14)
IR_lag14<-window(IR[15:231,1], start=13)
IR_lag24<-window(IR[25:231,1], start=3)
IR_lag25<-window(IR[26:231,1], start=2)
IR_lag26<-window(IR[27:231,1], start=1)
residuals_IR1<-window(residuals_IR[1:231,1], start=27)
residuals_IR_lag1<-window(residuals_IR[2:231,1], start=26)

data_hybrid_IR<-data.frame(IR1,IR_lag1,IR_lag2 ,IR_lag12,
IR_lag13,IR_lag14,IR_lag24,IR_lag25,IR_lag26,
residuals_IR1,residuals_IR_lag1)

data_hybrid_IR<-scale(data_hybrid_IR)

formula<-IR1~IR_lag1+IR_lag2+ IR_lag12+IR_lag13+IR_lag14+
IR_lag24+IR_lag25+IR_lag26+residuals_IR_lag1+ residuals_IR1

hybrid_ARIMA_ANN_IR<-neuralnet(formula, data_hybrid_IR,hidden = 3, threshold = 0.01,
stepmax = 1e+05, rep = 1, startweights = NULL,
learningrate.limit = NULL,
learningrate.factor = list(minus = 0.5, plus = 1.2),
learningrate=NULL, lifesign = "none",
lifesign.step = 1000, algorithm = "rprop+",
err.fct = "sse", act.fct = "logistic",
linear.output = TRUE, exclude = NULL,
constant.weights = NULL, likelihood = FALSE)

err.fct<-hybrid_ARIMA_ANN_IR$err.fct
act.fct<-hybrid_ARIMA_ANN_IR$act.fct
weights<-hybrid_ARIMA_ANN_IR$weights

plot(hybrid_ARIMA_ANN_IR)

stand_prediction_hybrid<-hybrid_ARIMA_ANN_IR$net.result
stand_prediction_hybrid
stand_prediction_hybrid<-data.frame(stand_prediction_hybrid)
mean<-mean(IR1)
sd<-sd(IR1)
prediction_hybrid<-stand_prediction_hybrid*sd+mean
prediction_hybrid<-ts(prediction_hybrid, start=c(1997,9), frequency=12)
IR1<-ts(IR1, start=c(1997,9), frequency=12)

ts.plot(prediction_hybrid,IR1,  gpars = list(col = c("red", "blue"), ylab = "Interest Rate",
 main = "Hybrid ARIMA_ANN s. original data"))
legend(2000, 26, legend=c("hybrid ARIMA_ANN", "original data"),col=c("red", "blue"), lty=1:2, cex=1)

accuracy(prediction_hybrid,IR1)


fertility<-read.csv("C:/Users/OUTMAN/Desktop/fertility.csv", sep=";", dec=",", header=T)
str(fertility)
training_fertility<-fertility[1:230,2]
test_fertility<-fertility[231:288,2]
fertility<-ts(training_fertility, start=c(1990,1), frequency=12)
test_fertility<-ts(test_fertility, start=c(2009,3), frequency=12)
plot(fertility, main="EVOLUTION OF THE TOTAL FERTILITY RATE", col="red", font.main=4, col.main="red",cex.main=1.5)


decomp_fertility<- stl(fertility, t.window=15, s.window="periodic", robust=TRUE)
plot(decomp_fertility)



ADF_fertility<-adf.test(fertility, alternative = "stationary",k = trunc((length(fertility)-1)^(1/3)))
ADF_fertility



tsdisplay(fertility)
tsdisplay(diff(fertility,12))
auto.arima(fertility, d = 0, D=1,max.p = 5, max.q = 5, max.P = 2,
  max.Q =2, max.order = 5,  max.D = 1, start.p = 2,
  start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
  seasonal = TRUE, ic = c("aicc", "aic", "bic"), stepwise = TRUE,
  trace = TRUE, lambda=T, allowdrift=F)




fit_fertility<- Arima(fertility, order=c(1,0,2), seasonal=c(1,1,1))
fit_fertility
fit_arima_fertility_coef<-coef(fit_fertility)
fit_arima_fertility_coef
fit_arima_fertility_varcoef<-fit_fertility$var.coef
fit_arima_fertility_varcoef
fit_arima_fertility_sigma2<-fit_fertility$sigma2
fit_arima_fertility_sigma2

(1-pnorm(abs(fit_arima_fertility_coef)/sqrt(diag(fit_arima_fertility_varcoef))))*2

res_fertility<- residuals(fit_fertility)
tsdisplay(res_fertility)
Box.test(res_fertility, lag=8, fitdf=1, type="Ljung")
plot(fit_fertility)
plot(forecast(fit_fertility ,h=58, ))
forecast_tfr<-data.frame(forecast(fit_fertility, h=58))
forecast_tfr_arima<-ts(forecast_tfr[,1], start=c(2009,3), frequency=12)

ts.plot(forecast_tfr_arima, test_fertility,gpars = list(col = c("red", "blue"), ylab = "Total Fertility Rate",
 main = "SARIMA vs. Original data"))
legend(2013.3, 2500, legend=c("SARIMA", "Original DATA"), col=c("red", "blue"), lty=1:2, cex=1)

res_fertility<- residuals(fit_fertility)
residual_tfr<-data.frame(res_fertility)
test_fertility<-ts(test_fertility, start=c(2009,3), frequency=12)
accuracy(forecast_tfr_arima, test_fertility)

fit_nar_fertility <- nnetar(fertility, p=3, P = 1, size=4, lambda=TRUE)
fit_nar_fertility
forecast_tfr_NAR<-forecast(fit_nar_fertility,h=58)
plot(forecast_tfr_NAR)
summary(fit_nar_fertility)
fitted_nar_fertility<-fit_nar_fertility$fitted

accuracy(forecast_tfr_NAR, test_fertility)

fertility<-data.frame(fertility)
fertility1<-window(fertility[1:230,1], start=26)
tfr_lag1<-window(fertility[2:230,1], start=25)
tfr_lag2<-window(fertility[3:230,1], start=24)
tfr_lag3<-window(fertility[4:230,1], start=23)
tfr_lag12<-window(fertility[13:230,1], start=14)

data_NAR_tfr<-data.frame(fertility1,tfr_lag1,tfr_lag2,tfr_lag3,tfr_lag12)

formula<-fertility1~tfr_lag1+tfr_lag2+tfr_lag3+tfr_lag12

architecture_NAR_tfr<-neuralnet(formula, data_NAR_tfr,hidden = 2, threshold = 0.01,
stepmax = 1e+05, rep = 1, startweights = NULL,
learningrate.limit = NULL,
learningrate.factor = list(minus = 0.5, plus = 1.2),
learningrate=NULL, lifesign = "none",
lifesign.step = 1000, algorithm = "rprop+",
err.fct = "sse", act.fct = "logistic",
linear.output = TRUE, exclude = NULL,
constant.weights = NULL, likelihood = FALSE)

ts.plot(forecast_tfr_NAR$mean, test_fertility,gpars = list(col = c("red", "blue"), ylab = "Total Fertility Rate",
 main = "NAR vs. Original data"))
legend(2013.3, 2500, legend=c("SARIMA", "Original DATA"), col=c("red", "blue"), lty=1:2, cex=1)

fertility<-data.frame(fertility)

fertility1<-window(fertility[1:230,1], start=26)
tfr_lag1<-window(fertility[2:230,1], start=25)
tfr_lag12<-window(fertility[13:230,1], start=14)
tfr_lag13<-window(fertility[14:230,1], start=13)
tfr_lag24<-window(fertility[25:230,1], start=2)
tfr_lag25<-window(fertility[26:230,1], start=1)
resi_tfr_lag1<-window(residual_tfr[2:230,1], start=25)
resi_tfr_lag2<-window(residual_tfr[3:230,1], start=24)
resi_tfr_lag12<-window(residual_tfr[13:230,1], start=14)
resi_tfr_lag13<-window(residual_tfr[14:230,1], start=13)
resi_tfr_lag14<-window(residual_tfr[15:230,1], start=12)

test_fertility<-data.frame(test_fertility)

test_tfr1<-window(test_fertility[1:58,1], start=26)
test_tfr_lag1<-window(test_fertility[2:58,1], start=25)
test_tfr_lag12<-window(test_fertility[13:58,1], start=14)
test_tfr_lag13<-window(test_fertility[14:58,1], start=13)
test_tfr_lag24<-window(test_fertility[25:58,1], start=2)
test_tfr_lag25<-window(test_fertility[26:58,1], start=1)
res_tfr_lag1<-window(residual_tfr[2:230,1], start=197)
res_tfr_lag2<-window(residual_tfr[3:230,1], start=196)
res_tfr_lag12<-window(residual_tfr[13:230,1], start=186)
res_tfr_lag13<-window(residual_tfr[14:230,1], start=185)
res_tfr_lag14<-window(residual_tfr[15:230,1], start=184)

cov<-scale(data.frame(test_tfr_lag1,test_tfr_lag12,test_tfr_lag13,test_tfr_lag24,test_tfr_lag25,res_tfr_lag1,res_tfr_lag2,res_tfr_lag12,res_tfr_lag13,res_tfr_lag14))

data_hybrid_tfr<-data.frame(fertility1,tfr_lag1,tfr_lag12,tfr_lag13,tfr_lag24,tfr_lag25,resi_tfr_lag1,resi_tfr_lag2,resi_tfr_lag12,resi_tfr_lag13,resi_tfr_lag14)

data_hybrid_tfr<-scale(data_hybrid_tfr)

formula<-fertility1~tfr_lag1+tfr_lag12+tfr_lag13+tfr_lag24+tfr_lag25+resi_tfr_lag1+resi_tfr_lag2+resi_tfr_lag12+resi_tfr_lag13+resi_tfr_lag14


hybrid_ARIMA_ANN_tfr<-neuralnet(formula, data_hybrid_tfr,hidden = 3, threshold = 0.01,
stepmax = 1e+05, rep = 1, startweights = NULL,
learningrate.limit = NULL,
learningrate.factor = list(minus = 0.5, plus = 1.2),
learningrate=NULL, lifesign = "none",
lifesign.step = 1000, algorithm = "rprop+",
err.fct = "sse", act.fct = "logistic",
linear.output = TRUE, exclude = NULL,
constant.weights = NULL, likelihood = FALSE)


plot(hybrid_ARIMA_ANN_tfr)

err.fct<-hybrid_ARIMA_ANN_dollar$err.fct
act.fct<-hybrid_ARIMA_ANN_dollar$act.fct
weights<-hybrid_ARIMA_ANN_dollar$weights

pr.nn <- compute(hybrid_ARIMA_ANN_tfr,cov)
pred<-pr.nn$net.result
hybrid_fcast<-ts(pred*sd(test_tfr1)+mean(test_tfr1), start=c(2011,4), frequency=12)

test_tfr1<-ts(test_tfr1, start=c(2011,4), frequency=12)
fcast_tfr<-ts(pred*sd(test_tfr1)+mean(test_tfr1), start=c(2011,4), frequency=12)


ts.plot(hybrid_fcast, test_tfr1,gpars = list(col = c("red", "blue"), ylab = "Total Fertility Rate",
 main = "hybrid ARIMA ANN vs. Original data"))
legend(2013.3, 2600, legend=c("Hybrid", "Original DATA"), col=c("red", "blue"), lty=1:2, cex=1)

accuracy(fcast_tfr,start=c(2011,4), frequency=12, test_tfr1)











