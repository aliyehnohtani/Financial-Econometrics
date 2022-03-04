# Aliyeh Nohtani
# 2021-Fall
# Financial Time Series
# Lecture 2 - Box-Jenkins Analysis


rm(list=ls(all=TRUE))
ls()

# load forecast package 
library(forecast)

# Set directory
# Check you have the right file path!!
filepath <- 
setwd(filepath)

# Export figures? 0 for NO, 1 for YES
exp.plt <- 0

# Set seed
set.seed(123)


# Simulated AR(1) process
y <- arima.sim(n=250,list(ar=.9,ma=0))

# plot time series
layout(1)
plot(y)
abline(h=0)
if(exp.plt==1){
	pdf("Figures/fig1.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	abline(h=0)
	dev.off()	
}
# plot sample ACF and PACF 
par(mfrow=c(1,2))
Acf(y,lag.max=24)
Pacf(y,lag.max=24)
if(exp.plt==1){
	pdf("Figures/fig2.pdf",height=3,width=7)
	par(mfrow=c(1,2),omi=c(0,0,0,0),mai=c(.6,.8,.2,.1),ps=10)
	Acf(y,lag.max=24)
	Pacf(y,lag.max=24)
	dev.off()	
}

# Simulated AR(2) process
y <- arima.sim(n=250,list(ar=c(1.31,-0.35),ma=0))

# plot time series
layout(1)
plot(y)
abline(h=0)
if(exp.plt==1){
	pdf("Figures/fig3.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	abline(h=0)
	dev.off()	
}
# plot sample ACF and PACF 
par(mfrow=c(1,2))
Acf(y,lag.max=24)
Pacf(y,lag.max=24)
if(exp.plt==1){
	pdf("Figures/fig4.pdf",height=3,width=7)
	par(mfrow=c(1,2),omi=c(0,0,0,0),mai=c(.6,.8,.2,.1),ps=10)
	Acf(y,lag.max=24)
	Pacf(y,lag.max=24)
	dev.off()	
}


# Simulated MA(1) process
y <- arima.sim(n=250,list(ar=0,ma=.9))

# plot time series
layout(1)
plot(y)
abline(h=0)
if(exp.plt==1){
	pdf("Figures/fig5.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	abline(h=0)
	dev.off()	
}
# plot sample ACF and PACF 
par(mfrow=c(1,2))
Acf(y,lag.max=24)
Pacf(y,lag.max=24)
if(exp.plt==1){
	pdf("Figures/fig6.pdf",height=3,width=7)
	par(mfrow=c(1,2),omi=c(0,0,0,0),mai=c(.6,.8,.2,.1),ps=10)
	Acf(y,lag.max=24)
	Pacf(y,lag.max=24)
	dev.off()	
}

# Simulated MA(2) process
y <- arima.sim(n=250,list(ar=0,ma=c(.5,.4)))

# plot time series
layout(1)
plot(y)
abline(h=0)
if(exp.plt==1){
	pdf("Figures/fig7.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	abline(h=0)
	dev.off()	
}
# plot sample ACF and PACF 
par(mfrow=c(1,2))
Acf(y,lag.max=24)
Pacf(y,lag.max=24)
if(exp.plt==1){
	pdf("Figures/fig8.pdf",height=3,width=7)
	par(mfrow=c(1,2),omi=c(0,0,0,0),mai=c(.6,.8,.2,.1),ps=10)
	Acf(y,lag.max=24)
	Pacf(y,lag.max=24)
	dev.off()	
}


# Simulated ARMA(1,1) process
y <- arima.sim(n=250,list(ar=.5,ma=.9))

# plot time series
layout(1)
plot(y)
abline(h=0)
if(exp.plt==1){
	pdf("Figures/fig9.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	abline(h=0)
	dev.off()	
}
# plot sample ACF and PACF 
par(mfrow=c(1,2))
Acf(y,lag.max=24)
Pacf(y,lag.max=24)
if(exp.plt==1){
	pdf("Figures/fig10.pdf",height=3,width=7)
	par(mfrow=c(1,2),omi=c(0,0,0,0),mai=c(.6,.8,.2,.1),ps=10)
	Acf(y,lag.max=24)
	Pacf(y,lag.max=24)
	dev.off()	
}


# Fitting an ARMA Model

# function that computes AIC
aic <- function(model){
	n <- length(model$residuals)
	k <- length(model$coef)
	s2 <- model$sigma2
	return(log(s2)+(2*k/n))
	}

# function that computes BIC
bic <- function(model){
	n <- length(model$residuals)
	k <- length(model$coef)
	s2 <- model$sigma2
	return(log(s2)+(log(n)*k/n))
	}

# Simulated AR(2) process
y <- arima.sim(n=250,list(ar=c(1.31,-0.35),ma=0))

# plot time series
layout(1)
plot(y)
abline(h=0)
if(exp.plt==1){
	pdf("Figures/fig11.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	abline(h=0)
	dev.off()	
}
# plot sample ACF and PACF 
par(mfrow=c(1,2))
Acf(y,lag.max=24)
Pacf(y,lag.max=24)
if(exp.plt==1){
	pdf("Figures/fig12.pdf",height=3,width=7)
	par(mfrow=c(1,2),omi=c(0,0,0,0),mai=c(.6,.8,.2,.1),ps=10)
	Acf(y,lag.max=24)
	Pacf(y,lag.max=24)
	dev.off()	
}

# max lag orders allowed
p.max <- 3
q.max <- 3

# Storage matrices
aic.results <- matrix(rep(0,(p.max+1)*(q.max+1)),nrow=(q.max+1),ncol=(p.max+1))
bic.results <- matrix(rep(0,(p.max+1)*(q.max+1)),nrow=(q.max+1),ncol=(p.max+1))

for(i in 0:q.max){
	for(j in 0:p.max){
		model <- Arima(y,order=c(j,0,i),method="ML")
		aic.results[i+1,j+1] <- aic(model)
		bic.results[i+1,j+1] <- bic(model)
		}
	}

# information criteria selects ARMA(2,0)
model20 <- Arima(y,order=c(2,0,0),method="ML")
model20

# find eigenvalues
polyroot(c(-model20$coef[2],-model20$coef[1],1))

# test for autocorrelation in the residuals
Box.test(model20$resid,lag=5,type="Ljung-Box",fitdf=3)
Box.test(model20$resid,lag=10,type="Ljung-Box",fitdf=3)


# estimate misspecified model: ARMA(0,1)
model01 <- Arima(y,order=c(0,0,1),method="ML")
model01

# find eigenvalues
polyroot(c(-model01$coef[1],1))

# test for autocorrelation in the residuals
Box.test(model01$resid,lag=5,type="Ljung-Box",fitdf=2)
Box.test(model01$resid,lag=10,type="Ljung-Box",fitdf=2)

