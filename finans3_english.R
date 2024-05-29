## 
## ################################
## ## Set the working directory
## 
## ## In RStudio use conveniently the menu "Session->Set Working
## ## Directory->To Source File Location"
## ## In R use only "/" for separating in paths (i.e. no backslash)
## setwd("Replace with path to where the project files are")

## 
## ################################
## ## Import the data
## 
## ## Read the finans1_data.csv file containing the data
## wr <- read.table("finans1_data.csv", header=TRUE, sep=";", as.is=TRUE)

## 
## #############################
## ## Overview of the data
## 
## ## Dimension of HE (number of rows and columns)
## dim(wr)
## ## Column names
## names(wr)
## ## The first rows
## head(wr)
## ## The last rows
## tail(wr)
## ## Default summary
## summary(wr)
## ## Another summary function also including the data type
## str(wr)


#setwd("~/Kurser/introstat/trunk/projects/finans3/")
wr <- read.table("../finans1_data.csv",header=TRUE,sep=";",as.is=TRUE)
dim(wr)
head(wr[ ,1:5],n=2)
wr[c(1,454) ,1]
sum(is.na(wr))
names(wr)[1:4]
summary(wr)[ ,1:3]


############################
## Descriptive analysis of selected variables
## b)
sum(!is.na(wr$AGG))
mean(wr$AGG)
sd(wr$AGG)
## ...

## Alternatively, to run a "function" on the selected columns you
## can use the "apply"-command or wrap it in a for-loop. 
## For futher info see ?apply.


## define new data frame with the EFT's we will work with 
wr2 <- wr[ ,c("AGG","VAW","IWN","SPY")]
## The full table
round(cbind(No.obs=apply(wr2,2,function(x){sum(!is.na(x))}),
      avg=apply(wr2,2,mean),
      var=apply(wr2,2,var),sd=apply(wr2,2,sd),
      t(apply(wr2,2,quantile,prob=c(0.25,0.5,0.75)))),digits=5)

   par(mfrow=c(2,2))
   hist(wr2[ ,"AGG"],freq=FALSE)
   hist(wr2[ ,"VAW"],freq=FALSE)
   hist(wr2[ ,"IWN"],freq=FALSE)
   hist(wr2[ ,"SPY"],freq=FALSE)

par(mfrow=c(1,1))
boxplot(list(wr2[ ,"AGG"],wr2[ ,"VAW"],wr2[ ,"IWN"],wr2[ ,"SPY"]), 
        names=c("AGG","VAW","IWN","SPY"))

apply(wr2,2,range)


###########################
## d)
## Determination of the correlation between ETFs 
## and determination of portfolio
cov(wr[ ,c("AGG","VAW","IWN","SPY","EWG","EWW")])


round(cov(wr[ ,c("AGG","VAW","IWN","SPY","EWG","EWW")]),digits=6)





(beta2<-var(wr[ ,c("EWG")])+var(wr[ ,c("EWW")])-
2*cov(wr[ ,c("EWG")],wr[ ,c("EWW")]))
(beta1 <- 2*(-var(wr[ ,c("EWW")])+cov(wr[ ,c("EWG")],wr[ ,c("EWW")])))
(beta0 <- var(wr[ ,c("EWW")]))

alpha <- seq(0,1,by=0.01)
plot(alpha,beta2*alpha^2+beta1*alpha+beta0,type="l",ylab="Variance")

 (alpham <- -beta1/(2*beta2))

 port <- function(EFT1,EFT2){
   beta2<-var(wr[ ,EFT1])+var(wr[ ,EFT2])-
   2*cov(wr[ ,c(EFT1)],wr[ ,EFT2])
   beta1 <- 2*(-var(wr[ ,EFT2])+cov(wr[ ,EFT1],wr[ ,EFT2]))
   beta0 <- var(wr[ ,c(EFT2)])
   alpham <- -beta1/(2*beta2)
   vm <- beta2*alpham^2+beta1*alpham+beta0
   retr <- alpham * mean(wr[ ,EFT1]) + 
           (1 - alpham) * mean(wr[ ,EFT2])
   c(alpham = alpham, vm = vm, retr = retr)
 }
 rbind(EWGEWW=port("EWG","EWW"),AGGSPY=port("AGG","SPY"),
       VAWIWN=port("VAW","IWN"),VAWEWG=port("VAW","EWG"),
       VAWEWW=port("VAW","EWW"),IWNEWG=port("IWN","EWG"))



  
###########################
## Model validation
## f)
## Validation of a model for AGG
qqnorm(wr$AGG, main='Validation of normal distribution assumption for AGG',
       xlab='z-scores', ylab='Weekly returns')
qqline(wr$AGG)
## Do the same for the other ETFs


tab <- cbind(m = apply(wr2,2,mean), var = apply(wr2,2,var), 
             sd = apply(wr2,2,sd))
rownames(tab) <- c("AGG","VAW","IWN","SPY")  
tab

par(mfrow=c(2,2))
qqnorm(wr2[ ,"AGG"])
qqline(wr2[ ,"AGG"])
qqnorm(wr2[ ,"VAW"])
qqline(wr2[ ,"VAW"])
qqnorm(wr2[ ,"IWN"])
qqline(wr2[ ,"IWN"])
qqnorm(wr2[ ,"SPY"])

  
###########################
## Calculations of the 95% confidence intervals
## g)
## t-quantile for the confidence interval for the mean of AGG, 
## since the degrees of freedom for the mean of AGG are 453 
qt(0.975, 453)

## Determination of the confidence interval for the mean parameter in a
## normally distributed random sample

## The 95% confidence interval for AGG
t.test(wr$AGG, conf.level=0.95)$conf.int
## Do the same for the other ETFs


(CImAGG <- mean(wr[ ,"AGG"]) + c(-1, 1) * qt(0.975, df = 453) * 
sd(wr[ ,"AGG"])/sqrt(454))
(CImVAW <- mean(wr[ ,"VAW"]) + c(-1, 1) * qt(0.975, df = 453) * 
sd(wr[ ,"VAW"])/sqrt(454))
(CImIWN <- mean(wr[ ,"IWN"]) + c(-1, 1) * qt(0.975, df = 453) *
sd(wr[ ,"IWN"])/sqrt(454))
(CImSPY <- mean(wr[ ,"SPY"]) + c(-1, 1) * qt(0.975, df = 453) *
sd(wr[ ,"SPY"])/sqrt(454))

(CIsAGG <- var(wr[ ,"AGG"]) * 453 / c(qchisq(c(0.975,0.025), df=453)))
(CIsVAW <- var(wr[ ,"VAW"]) * 453 / c(qchisq(c(0.975,0.025), df=453)))
(CIsIWN <- var(wr[ ,"IWN"]) * 453 / c(qchisq(c(0.975,0.025), df=453)))
(CIsSPY <- var(wr[ ,"SPY"]) * 453 / c(qchisq(c(0.975,0.025), df=453)))

tab <- cbind(rbind(CImAGG, CImVAW, CImIWN, CImSPY), 
      rbind(CIsAGG, CIsVAW, CIsIWN, CIsSPY))
rownames(tab) <- c("AGG","VAW","IWN","SPY")
colnames(tab) <- c("lower CI.m","upper CI.m","lower CI.s^2",
"upper CI.s^2")
tab

set.seed(3285913)
 
 k <- 10000
 simsamplesAGG<-replicate(k,sample(wr[ ,"AGG"], replace=TRUE))
 simmeansAGG<-apply(simsamplesAGG,2,mean)
 simvarsAGG<-apply(simsamplesAGG,2,var)
 (CImAGGboot <- quantile(simmeansAGG, c(0.025,0.975)))
 (CIsAGGboot <- quantile(simvarsAGG, c(0.025,0.975)))
 
 simsamplesVAW<-replicate(k,sample(wr[ ,"VAW"], replace=TRUE))
 simmeansVAW<-apply(simsamplesVAW,2,mean)
 simvarsVAW<-apply(simsamplesVAW,2,var)
 CImVAWboot <- quantile(simmeansVAW, c(0.025,0.975))
 CIsVAWboot <- quantile(simvarsVAW, c(0.025,0.975))
 
 simsamplesIWN<-replicate(k,sample(wr[ ,"IWN"], replace=TRUE))
 simmeansIWN<-apply(simsamplesIWN,2,mean)
 simvarsIWN <-apply(simsamplesIWN,2,var)
 CImIWNboot <- quantile(simmeansIWN, c(0.025,0.975))
 CIsIWNboot <- quantile(simvarsIWN, c(0.025,0.975))
 
 simsamplesSPY<-replicate(k,sample(wr[ ,"SPY"], replace=TRUE))
 simmeansSPY<-apply(simsamplesSPY,2,mean)
 simvarsSPY<-apply(simsamplesSPY,2,var)
 CImSPYboot <- quantile(simmeansSPY, c(0.025,0.975)) 
 CIsSPYboot <- quantile(simvarsSPY, c(0.025,0.975)) 

tab <- cbind(rbind(CImAGGboot, CImVAWboot, CImIWNboot, CImSPYboot), 
      rbind(CIsAGGboot, CIsVAWboot, CIsIWNboot, CIsSPYboot))
rownames(tab) <- c("AGG","VAW","IWN","SPY")
colnames(tab) <- c("lower CI.m","upper CI.m","lower CI.s^2",
"upper CI.s^2")
tab

   (t.obs <- mean(wr[ ,"AGG"]) / (sd(wr[ ,"AGG"])/sqrt(454) ))

 2*(1-pt(abs(t.obs),df=453))

t.test(wr[ ,"AGG"])

qt(0.975,df=453)

t <- apply(wr[ ,c("AGG","VAW","IWN","SPY")],2,mean)/
apply(wr[ ,c("AGG","VAW","IWN","SPY")],2,sd)*sqrt(453)
df <- rep(453,4)
p <- 2*(1-pt(abs(t),df=453))
tab <- cbind(t,df,p)
rownames(tab) <- c("AGG","VAW","IWN","SPY")
colnames(tab) <- c("t.obs","df","P(T>|t.obs|)")
tab

 t.test(wr[ ,"AGG"],wr[ ,"VAW"])

  
################################
## 
## Import data finans2_data.csv
etfSum <- read.table("finans2_data.csv",header=TRUE, sep=";")
str(etfSum)

## 
## ################################
## ## j)
## ## Determine the empirical correlation for the selected variables and
## ## examine the dependencies
## cor(etfSum_analyse[,2:7], use="everything", method="pearson")
## 
## ## First trim the square around the plot. See more on ?par
## par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
## par(mfrow=c(1,1))
## plot(etfSum_analyse$Volatility, etfSum_analyse$CVaR, pch=16, cex=0.7,
##      xlab="Volatility [Weekly Pct.]",
##      ylab="Conditional Value at Risk [Weekly Pct.]",  cex.lab=0.8,
##      main="Relation between Volatility and CVaR", cex.main=0.8)

## 
## ## For calculations of the correlation between Geo.mean and maxTuW
## ## k)
## cov(etfSum_analyse$Geo.mean, etfSum_analyse$maxTuW)
## var(etfSum_analyse$Geo.mean)
## var(etfSum_analyse$maxTuW)


etfSum_analyse <- read.table("../finans2_data.csv",header=TRUE, sep=";")
attach(etfSum_analyse)
par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
par(mfrow=c(2,2))
plot(Volatility, CVaR, pch=16, cex=0.7,cex.main=0.8)
plot(Geo.mean, maxTuW, pch=16, cex=0.7,cex.main=0.8)
plot(Volatility, maxDD, pch=16, cex=0.7,cex.main=0.8)
plot(maxTuW, Volatility, pch=16, cex=0.7,cex.main=0.8)

round(cor(etfSum_analyse[,-1]),digits=2)

c(cov(etfSum_analyse[,"Geo.mean"],etfSum_analyse[,"maxTuW"]), 
sd(etfSum_analyse[,"Geo.mean"]), sd(etfSum_analyse[,"maxTuW"]))



