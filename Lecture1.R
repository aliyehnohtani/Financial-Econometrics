# Aliyeh Nohtani
#University of Tehran
# 2021-Fall
# Financial Time Series
# Lecture 1 - Univariate Stationary Processes


rm(list=ls(all=TRUE))
ls()

# load forecast package 
library(forecast)

# Set directory
# Check you have the right file path!!
filepath
setwd(filepath)

# Export figures? 0 for NO, 1 for YES
exp.plt <- 0

# Set seed
set.seed(123)


# simulate gaussian white noise process
y <- as.ts(rnorm(250))

# plot process
layout(1)
plot(y)
abline(h=0)
# create pdf
if(exp.plt==1){
	pdf("Figures/fig1.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	abline(h=0)
	dev.off()	
}

# get theoretical IRF
acf0 <- ARMAacf(ar=c(0),ma=c(0),lag.max=10,pacf=FALSE)

# plot theoretical IRF
plot(acf0,type="b",ylim=c(0,1),axes=FALSE,ylab="ACF",xlab="lags")
axis(1,1:11,labels=0:10)
axis(2,seq(0,1,by=.25),labels=seq(0,1,by=.25))
box()
# create pdf
if(exp.plt==1){
	pdf("Figures/fig2.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(acf0,type="b",ylim=c(0,1),axes=FALSE)
	axis(1,1:11,labels=0:10)
	axis(2,seq(0,1,by=.5),labels=seq(0,1,by=.5))
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("ACF",side=2,line=2,at=300,cex=0.9)
	box()
	dev.off()	
}


# simulate trend process
e <- rnorm(250)
beta0 <- 0; beta1 <- .1
t <- seq(1,250)
y <- as.ts(beta0+beta1*t+e)

# plot process
plot(y)
# create pdf
if(exp.plt==1){
	pdf("Figures/fig3.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	dev.off()	
}


# simulate random walk process
e <- rnorm(250)
y <- as.ts(rep(0,250))
y[1] <- e[1]
for(i in 2:250){ y[i] <- y[i-1]+e[i] }

# plot process
plot(y)
abline(h=0)
# create pdf
if(exp.plt==1){
	pdf("Figures/fig4.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	abline(h=0)
	dev.off()	
}


# AR(1) with phi = 0.75
# get theoretical IRF
acf1 <- ARMAacf(ar=c(0.75),ma=c(0),lag.max=10,pacf=FALSE)
# IRF
plot(acf1,type="b",ylim=c(0,1),axes=FALSE,ylab="IRF",xlab="lags")
axis(1,1:11,labels=0:10)
axis(2,seq(0,1,by=.25),labels=seq(0,1,by=.25))
box()
# CIRF
plot(cumsum(acf1),type="b",ylim=c(0,4),axes=FALSE,ylab="Cumulative IRF",xlab="lags")
axis(1,1:11,labels=0:10)
axis(2,seq(0,4,by=1),labels=seq(0,4,by=1))
box()
# create pdfs
if(exp.plt==1){
	pdf("Figures/fig5.pdf",height=3,width=7)
	par(mfrow=c(1,2),omi=c(0,0,0,0),mai=c(.6,.8,.2,.1),ps=10)
	plot(acf1,type="b",ylim=c(0,1),axes=FALSE,ylab="")
	axis(1,1:11,labels=0:10)
	axis(2,seq(0,1,by=.5),labels=seq(0,1,by=.5))
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("IRF",side=2,line=2,at=.5,cex=0.9)
	box()
	plot(cumsum(acf1),type="b",ylim=c(0,4),axes=FALSE,ylab="")
	axis(1,1:11,labels=0:10)
	axis(2,seq(0,4,by=1),labels=seq(0,4,by=1))
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("Cumulative IRF",side=2,line=2,at=2,cex=0.9)
	box()
	dev.off()	
}

# AR(1) with phi = -0.75
# get theoretical IRF
acf2 <- ARMAacf(ar=c(-0.75),ma=c(0),lag.max=10,pacf=FALSE)
# IRF
plot(acf2,type="b",ylim=c(-1,1),axes=FALSE,ylab="ACF",xlab="lags")
abline(h=0)
axis(1,1:11,labels=0:10)
axis(2,seq(-1,1,by=.5),labels=seq(-1,1,by=.5))
box()
# CIRF
plot(cumsum(acf2),type="b",ylim=c(0,1),axes=FALSE,ylab="ACF",xlab="lags")
axis(1,1:11,labels=0:10)
axis(2,seq(0,1,by=.25),labels=seq(0,1,by=.25))
box()
# create pdfs
if(exp.plt==1){
	pdf("Figures/fig6.pdf",height=3,width=7)
	par(mfrow=c(1,2),omi=c(0,0,0,0),mai=c(.6,.8,.2,.1),ps=10)
	plot(acf2,type="b",ylim=c(-1,1),axes=FALSE,ylab="")
	axis(1,1:11,labels=0:10)
	axis(2,seq(-1,1,by=.5),labels=seq(-1,1,by=.5))
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("IRF",side=2,line=2,at=0,cex=0.9)
	abline(h=0)
	box()
	plot(cumsum(acf2),type="b",ylim=c(0,1),axes=FALSE,ylab="")
	axis(1,1:11,labels=0:10)
	axis(2,seq(0,1,by=.5),labels=seq(0,1,by=.5))
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("Cumulative IRF",side=2,line=2,at=.5,cex=0.9)
	box()
	dev.off()	
}


# Simulated AR(1) processes
y1 <- arima.sim(n=250,list(ar=.99,ma=0))
y2 <- arima.sim(n=250,list(ar=.90,ma=0))
y3 <- arima.sim(n=250,list(ar=.75,ma=0))
y4 <- arima.sim(n=250,list(ar=.50,ma=0))

# plot time series
layout(matrix(c(1,3,2,4),2,2))
plot(y1,ann=FALSE)
abline(h=0)
mtext("f = 0.99",side=3,line=0,at=125,cex=0.9,font=5)
plot(y2,ann=FALSE)
abline(h=0)
mtext("f = 0.90",side=3,line=0,at=125,cex=0.9,font=5)
plot(y3,ann=FALSE)
abline(h=0)
mtext("f = 0.75",side=3,line=0,at=125,cex=0.9,font=5)
plot(y4,ann=FALSE)
abline(h=0)
mtext("f = 0.50",side=3,line=0,at=125,cex=0.9,font=5)
if(exp.plt==1){
	pdf("Figures/fig7.pdf",height=5,width=7)
	layout(matrix(c(1,3,2,4),2,2))
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y1,ann=FALSE)
	abline(h=0)
	mtext("f = 0.99",side=3,line=0,at=125,cex=0.9,font=5)
	plot(y2,ann=FALSE)
	abline(h=0)
	mtext("f = 0.90",side=3,line=0,at=125,cex=0.9,font=5)
	plot(y3,ann=FALSE)
	abline(h=0)	
	mtext("f = 0.75",side=3,line=0,at=125,cex=0.9,font=5)
	plot(y4,ann=FALSE)
	abline(h=0)
	mtext("f = 0.50",side=3,line=0,at=125,cex=0.9,font=5)
	dev.off()	
}

# plot acfs
layout(matrix(c(1,3,2,4),2,2))
Acf(y1,lag.max=10,main="")
mtext("f = 0.99",side=3,line=0,at=5.5,cex=0.9,font=5)
Acf(y2,lag.max=10,main="")
mtext("f = 0.90",side=3,line=0,at=5.5,cex=0.9,font=5)
Acf(y3,lag.max=10,main="")
mtext("f = 0.75",side=3,line=0,at=5.5,cex=0.9,font=5)
Acf(y4,lag.max=10,main="")
mtext("f = 0.50",side=3,line=0,at=5.5,cex=0.9,font=5)
if(exp.plt==1){
	pdf("Figures/fig8.pdf",height=5,width=7)
	layout(matrix(c(1,3,2,4),2,2))
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	# series y1
	Acf(y1,lag.max=10,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("f = 0.99",side=3,line=0,at=5.5,cex=0.9,font=5)
	# series y2
	Acf(y2,lag.max=10,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("f = 0.90",side=3,line=0,at=5.5,cex=0.9,font=5)
	# series y3
	Acf(y3,lag.max=10,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("f = 0.75",side=3,line=0,at=5.5,cex=0.9,font=5)
	# series y4
	Acf(y4,lag.max=10,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("f = 0.50",side=3,line=0,at=5.5,cex=0.9,font=5)
	dev.off()	
}


# Simulated AR(2) processes

# phi1=.6, phi2=.2
y <- arima.sim(n=250,list(ar=c(.6,.2),ma=0))

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

# plot ACF
Acf(y,lag.max=20,main="")
if(exp.plt==1){
	pdf("Figures/fig10.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	Acf(y,lag.max=20,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=18,cex=0.9)
	mtext("ACF",side=2,line=2,at=.5,cex=0.9)
	dev.off()	
}

# phi1=.5, phi2=-.8
y <- arima.sim(n=250,list(ar=c(.5,-.8),ma=0))

# plot time series
plot(y)
abline(h=0)
if(exp.plt==1){
	pdf("Figures/fig11.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y,ann=FALSE)
	abline(h=0)
	dev.off()	
}

# plot ACF
Acf(y,lag.max=20,main="")
if(exp.plt==1){
	pdf("Figures/fig12.pdf",height=3,width=7)
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	Acf(y,lag.max=20,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=18,cex=0.9)
	mtext("ACF",side=2,line=2,at=0,cex=0.9)
	dev.off()	
}


# Simulated MA(1) processes
y1 <- arima.sim(n=250,list(ar=0,ma=.95))
y2 <- arima.sim(n=250,list(ar=0,ma=.4))
y3 <- arima.sim(n=250,list(ar=0,ma=-.4))
y4 <- arima.sim(n=250,list(ar=0,ma=-.95))

# plot time series
layout(matrix(c(1,3,2,4),2,2))
plot(y1)
abline(h=0)
mtext("q = 0.95",side=3,line=0,at=125,cex=0.9,font=5)
plot(y2)
abline(h=0)
mtext("q = 0.40",side=3,line=0,at=125,cex=0.9,font=5)
plot(y3)
abline(h=0)
mtext("q = -0.95",side=3,line=0,at=125,cex=0.9,font=5)
plot(y4)
abline(h=0)
mtext("q = -0.40",side=3,line=0,at=125,cex=0.9,font=5)
if(exp.plt==1){
	pdf("Figures/fig13.pdf",height=5,width=7)
	layout(matrix(c(1,3,2,4),2,2))
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	plot(y1,ann=FALSE)
	abline(h=0)
	mtext("q = 0.95",side=3,line=0,at=125,cex=0.9,font=5)
	plot(y2,ann=FALSE)
	abline(h=0)
	mtext("q = 0.40",side=3,line=0,at=125,cex=0.9,font=5)
	plot(y3,ann=FALSE)
	abline(h=0)	
	mtext("q = -0.95",side=3,line=0,at=125,cex=0.9,font=5)
	plot(y4,ann=FALSE)
	abline(h=0)
	mtext("q = -0.40",side=3,line=0,at=125,cex=0.9,font=5)
	dev.off()	
}

# plot acfs
layout(matrix(c(1,3,2,4),2,2))
Acf(y1,lag.max=10,main="")
mtext("q = 0.95",side=3,line=0,at=5.5,cex=0.9,font=5)
Acf(y2,lag.max=10,main="")
mtext("q = 0.40",side=3,line=0,at=5.5,cex=0.9,font=5)
Acf(y3,lag.max=10,main="")
mtext("q = -0.95",side=3,line=0,at=5.5,cex=0.9,font=5)
Acf(y4,lag.max=10,main="")
mtext("q = -0.40",side=3,line=0,at=5.5,cex=0.9,font=5)
if(exp.plt==1){
	pdf("Figures/fig14.pdf",height=5,width=7)
	layout(matrix(c(1,3,2,4),2,2))
	par(omi=c(0,0,0,0),mai=c(.6,0.6,0.2,0.1),ps=10)
	# series y1
	Acf(y1,lag.max=10,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("ACF",side=2,line=2,at=0,cex=0.9)
	mtext("q = 0.95",side=3,line=0,at=5.5,cex=0.9,font=5)
	# series y2
	Acf(y2,lag.max=10,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("ACF",side=2,line=2,at=0,cex=0.9)
	mtext("q = 0.40",side=3,line=0,at=5.5,cex=0.9,font=5)
	# series y3
	Acf(y3,lag.max=10,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("ACF",side=2,line=2,at=0,cex=0.9)
	mtext("q = -0.95",side=3,line=0,at=5.5,cex=0.9,font=5)
	# series y4
	Acf(y4,lag.max=10,main="",ann=FALSE)
	mtext("lags",side=1,line=2,at=9.5,cex=0.9)
	mtext("ACF",side=2,line=2,at=0,cex=0.9)
	mtext("q = -0.40",side=3,line=0,at=5.5,cex=0.9,font=5)
	dev.off()	
}

