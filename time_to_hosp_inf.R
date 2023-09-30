#rel pro 2

library(KMsurv)
library(dplyr)
data("pneumon")

#summary(as.factor(pneumon$chldage))
#summary(as.factor(pneumon$agepn))
pneu_nocero <- pneumon[pneumon$agepn !=0,]
#summary(as.factor(pneu_nocero$hospital))
#summary(as.factor(pneumon$hospital))
#summary(pneu_nocero)
#histogram
hist(pneu_nocero$chldage,breaks=seq(0,12,by=1),freq=T,xlab="Child Age in Month Getting Pneumonia",
     ylab="Frequency",main="Histogram for Child Pneumonia Diagnosis",col="orange2",
     xlim=c(0,12),labels=T,ylim=c(0,3000))
axis(1,at=seq(0,12,by=1),labels=seq(0,12,by=1))
axis(2) 

hist(pneu_nocero$agepn,breaks=seq(0,12,by=1),freq=T,xlab="Months to Hospital Visit for Pneumonia",
     ylab="Frequency",main="Histogram for Hospital visit for Pneumonia",col="yellow3",
     xlim=c(0,12),labels=T,ylim=c(0,3000))
axis(1,at=seq(0,12,by=1),labels=seq(0,12,by=1))
axis(2) 
#lines(d, col = "red")

#summary(as.factor(pneu_nocero$chldage))
#summary(as.factor(pneu_nocero$agepn))

# density

d <- density(pneu_nocero$agepn)
plot(d, lwd = 2, main = "Kernel Density Plot")
#hist(pneu_nocero$agepn,frequency(pneu_nocero$agepn))


hist(pneu_nocero$agepn,breaks=seq(0,12,by=1),freq=T,xlab="Months to Hospital Visit for Pneumonia",
     ylab="Frequency",main="Histogram for Hospital visit for Pneumonia",col="yellow3",
     xlim=c(0,12),labels=T,ylim=c(0,3000))
axis(1,at=seq(0,12,by=1),labels=seq(0,12,by=1))
axis(2) 
lines(d, col = "purple")

library(ggplot2)


# EDA correlation of continuous variables
cor_pnue_nocero <- pneu_nocero[,c(1, 3, 11:15)]
as.matrix(cor(cor_pnue_nocero))
plot(cor_pnue_nocero)

#w sold

pneu_nocero_wm0 <- pneu_nocero[pneu_nocero$wmonth == 0,]
summary(as.factor(pneu_nocero_wm0$sfmonth))
pneu_nocero_sm0 <- pneu_nocero[pneu_nocero$sfmonth == 0,]
summary(as.factor(pneu_nocero_sm0$wmonth))


        
#survival coxph etc models
library(survival)
fit1=coxph(Surv(pneu_nocero$agepn, pneu_nocero$hospital)~ factor(pneu_nocero$wmonth > 0),ties="breslow",data=pneu_nocero)
summary(fit1)
summary(as.factor(pneu_nocero$wmonth))
AIC(fit1)

fit2=coxph(Surv(pneu_nocero$agepn, pneu_nocero$hospital)~ pneu_nocero$smoke * factor(pneu_nocero$wmonth > 0),ties="breslow",data=pneu_nocero)
summary(fit2)
AIC(fit2)

#survfit plot

fit.km=survfit(Surv(pneu_nocero$agepn,pneu_nocero$hospital)~factor(pneu_nocero$wmonth >0),data=pneu_nocero)

plot(fit.km,fun="cloglog",col=c("blue","red"),xlab="Time",ylab="log(-log(Survival))")

fit.km_2=survfit(Surv(pneu_nocero$agepn,pneu_nocero$hospital)~factor(pneu_nocero$wmonth >0),data=pneu_nocero)
summary(fit.km_2)
plot(fit.km_2,col=c("blue","red"),xlab="Time",ylab="Survival Function", ylim = c(.94, 1), main = "Kaplan-Meier Weaned Month", lwd = c(2,2))
legend("bottomleft",bty="n",legend=c("Breastfed","Never Breastfed"),col=c("red","blue"),lty=c(1,1))


fit.km_21 <- survfit(Surv(pneu_nocero$agepn, pneu_nocero$hospital)~factor(pneu_nocero$race), data = pneu_nocero)
plot(fit.km_21, col = c("blue", "red", "green"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Race", lwd = c(2,2,2))
legend("bottomleft",bty="n",legend=c("White","Black", "Other"),col=c("blue","red", "green"),lty=c(1,1,1))

summary(fit.km_21)

par(mfrow = c(1,2))

plot(fit.km_2,col=c("blue","red"),xlab="Time",ylab="Survival Function", ylim = c(.94, 1), main = "Kaplan-Meier Weaned Month", lwd = c(2,2))
legend("bottomleft",bty="n",legend=c("BF","Never BF"),col=c("red","blue"),lty=c(1,1))

plot(fit.km_21, col = c("blue", "red", "green"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Race", lwd = c(2,2,2))
legend("bottomleft",bty="n",legend=c("White","Black", "Other"),col=c("blue","red", "green"),lty=c(1,1,1))

# the 2x2 KM smoke alc region pov
fit.km_22 <- survfit(Surv(pneu_nocero$agepn, pneu_nocero$hospital)~factor(pneu_nocero$smoke > 0), data = pneu_nocero)
plot(fit.km_22, col = c("blue", "red"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Smoke", lwd = c(2,2))
legend("bottomleft",bty="n",legend=c("Did not Smoke", "Smoke"),col=c("blue","red"),lty=c(1,1))
summary(fit.km_22)


fit.km_23 <- survfit(Surv(pneu_nocero$agepn, pneu_nocero$hospital)~factor(pneu_nocero$alcohol > 0), data = pneu_nocero)
plot(fit.km_23, col = c("green", "gold"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Alcohol", lwd = c(2,2))
legend("bottomleft",bty="n",legend=c("No Alcohol", "Alcohol"),col=c("green","gold"),lty=c(1,1))
summary(fit.km_23)

fit.km_24 <- survfit(Surv(pneu_nocero$agepn, pneu_nocero$hospital)~factor(pneu_nocero$poverty), data = pneu_nocero)
plot(fit.km_24, col = c("purple", "orange"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Poverty", lwd = c(2,2))
legend("bottomleft",bty="n",legend=c("Not in Poverty", "Poverty"),col=c("purple","orange"),lty=c(1,1))
summary(fit.km_24)

fit.km_25 <- survfit(Surv(pneu_nocero$agepn, pneu_nocero$hospital)~factor(pneu_nocero$region), data = pneu_nocero)
plot(fit.km_25, col = c("blue", "red", "green", "violet"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Region", lwd = c(2,2,2,2))
legend("bottomleft",bty="n",legend=c("NE","NC", "S", "W"),col=c("blue","red", "green", "violet"),lty=c(1,1,1,1))
summary(fit.km_25)

par(mfrow = c(2,2))

plot(fit.km_22, col = c("blue", "red"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Smoke", lwd = c(2,2))
legend("bottomleft",bty="n",legend=c("Did not Smoke", "Smoke"),col=c("blue","red"),lty=c(1,1))

plot(fit.km_23, col = c("green", "gold"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Alcohol", lwd = c(2,2))
legend("bottomleft",bty="n",legend=c("No Alcohol", "Alcohol"),col=c("green","gold"),lty=c(1,1))

plot(fit.km_24, col = c("purple", "orange"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Poverty", lwd = c(2,2))
legend("bottomleft",bty="n",legend=c("Not in Poverty", "Poverty"),col=c("purple","orange"),lty=c(1,1))

plot(fit.km_25, col = c("blue", "red", "green", "violet"), xlab = "Time", ylab = "Survival Function",
     ylim = c(.94, 1), main = "KM Region", lwd = c(2,2,2,2))
legend("bottomleft",bty="n",legend=c("NE","NC", "S", "W"),col=c("blue","red", "green", "violet"),lty=c(1,1,1,1))


# check for each variable to see if it would work for the coxph

fit.km_pov =survfit(Surv(pneu_nocero$agepn,pneu_nocero$hospital)~factor(pneu_nocero$poverty),data=pneu_nocero)

plot(fit.km_pov,fun="cloglog",col=c("blue","red"),xlab="Time",ylab="log(-log(Survival))", main = "Log-log KM for Poverty")

pha <- cox.zph(fit = fit1)
pha

fit3=coxph(Surv(pneu_nocero$agepn, pneu_nocero$hospital)~ factor(pneu_nocero$poverty),ties="breslow",data=pneu_nocero)
summary(fit3)
pha_pov <- cox.zph(fit = fit3)
pha_pov

