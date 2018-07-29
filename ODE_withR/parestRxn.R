#
# Computational Appendix of Book
# Chemical Reactor Analysis and Design Fundamentals - Rawlings and Ekerdt
#
# Example A.5: Estimating rate constants for A->B->C from concentration vs time data
# 
# This was orginally written for a blog post
# https://notesofdabbler.wordpress.com/2013/06/30/learning-r-parameter-fitting-for-models-involving-differential-equations/
#
# This is an update/modification to also show how to fit initial concentration
# in addition to rate constants
#
#


# load libraries
library(ggplot2) #library for plotting
library(dplyr)  # data munging library
library(tidyr)  # data munging library
library(deSolve) # library for solving differential equations
library(minpack.lm) # library for least squares fit using levenberg-marquart algorithm

#load concentration data
# This is simulated data from A->B->C with k1 = 2, k2 = 1 with added noise
df=read.table("ABC_data.txt")
names(df)=c("time","ca","cb","cc")
head(df)
dim(df)

# plot data
dfg = df %>% gather(species, conc, -time)
ggplot(data=dfg, aes(x=time, y=conc, color=species)) + geom_point(size=3)


# rate function
# Given rate constants and concentration, this function returns rate of change of each species
# This function is needed for the ode solver function in deSolve package
rxnrate=function(t,c,mdlparms){
  
  # rate constant passed through a list called mdlparms
  k1 = mdlparms$k1
  k2 = mdlparms$k2
  
  # c is the concentration of species
  
  # derivatives dc/dt are computed below
  r=rep(0,length(c))
  r[1]=-k1*c["A"]  #dcA/dt
  r[2]=k1*c["A"]-k2*c["B"] #dcB/dt
  r[3]=k2*c["B"] #dcC/dt
  
  # the computed derivatives are returned as a list
  # order of derivatives needs to be the same as the order of species in c
  return(list(r))
  
}

# predicted concentration for a given set of initial conditions and rate constants (model parameters)
cinit=c(A=1,B=0,C=0)
t=df$time
mdlparms=list(k1=2,k2=1)
out=ode(y=cinit,times=t,func=rxnrate,parms=mdlparms)
head(out)

#plot of predicted concentration
outdf=data.frame(out)
outdfg = outdf %>% gather(species, conc, -time)
ggplot(data=outdfg, aes(x = time,y = conc, color = species))+geom_line()

# For parameter estimation, we need a function whose input are parameters to be estimated
# and output is a vector of residuals for the set of measured data
#' ---
#' title: "Parameter Estimation of Reaction Model using R"
#' author: ""
#' date: "July 28, 2018"
#' ---

#
# In this case, we want to estimate both the initial concentrations of A, B, C and 
# the rate constants k1, k2
# df - dataframe containing experimental data
# fitparams - parameters to be estimated, in this case it is a 5 length vector with first
# 2 corresponding to k1, k2 and the remaining 3 corresponding to initial concentrations of 
# A, B, and C
#
ssq=function(fitparms, df){
  
  # inital concentration
  cinit = fitparms[3:5]
  names(cinit) = c("A", "B", "C")
  #cinit=c(A=1,B=0,C=0)
  
  # time points for which conc is reported
  # include the points where data is available
  t=c(seq(0,5,0.1),df$time)
  t=sort(unique(t))
  # parameters from the parameter estimation routine
  k1=fitparms[1]
  k2=fitparms[2]
  # solve ODE for a given set of parameters
  out=ode(y=cinit,times=t,func=rxnrate,parms=list(k1=k1,k2=k2))
  
  # Filter data that contains time points where data is available
  outdf=data.frame(out)
  outdf=outdf[outdf$time %in% df$time,]
  # Evaluate predicted vs experimental residual
  preddf = outdf %>% gather(species, conc, -time)
  expdf = df %>% gather(species, conc, -time)
  ssqres=preddf$conc-expdf$conc
  
  # return predicted vs experimental residual
  return(ssqres)
  
}

# parameter fitting using levenberg marquart algorithm
# initial guess for parameters
fitparms=c(k1=0.5, k2=0.5, A = 0.5, B = 0, C = 0)
# fitting
# lower bound of 0 imposed for fitting parameters since rate constants and concentrations
# are nonnegative
fitval=nls.lm(par=fitparms,fn=ssq, lower = rep(0, 5), control = nls.lm.control(nprint = 1), df = df)

# Summary of fit
summary(fitval)
# Estimated parameter
parest=as.list(coef(fitval))
parest
# degrees of freedom: # data points - # parameters
dof=3*nrow(df)-length(fitparms)
dof
# mean error
ms=sqrt(deviance(fitval)/dof)
ms
# variance Covariance Matrix
S=vcov(fitval)
S

# plot of predicted vs experimental data

# simulated predicted profile at estimated parameter values
cinit=unlist(parest[3:5])
t=seq(0,5,0.2)
mdlparms=parest[1:2]
out=ode(y=cinit,times=t,func=rxnrate,parms=mdlparms)
outdf=data.frame(out)
names(outdf)=c("time","ca_pred","cb_pred","cc_pred")

# Overlay predicted profile with experimental data
tmppred = outdf %>% gather(species, conc, -time)
tmpexp = df %>% gather(species, conc, -time)
p=ggplot(data=tmppred,aes(x=time,y=conc,color=species,linetype=species))+geom_line()
p=p+geom_line(data=tmpexp,aes(x=time,y=conc,color=species,linetype=species))
p=p+geom_point(data=tmpexp,aes(x=time,y=conc,color=species))
p=p+scale_linetype_manual(values=c(0,1,0,1,0,1))
p=p+scale_color_manual(values=rep(c("red","blue","green"),each=2))+theme_bw()
print(p)

# Get the 95% confidence region for k1 and k2

# Inverse of covariance matrix
Sinv=solve(S[1:2, 1:2])

# draw the confidence region
# get points for a circle with radius r
r=sqrt(qf(0.95,2,58)*2)
theta=seq(0,2*pi,length.out=100)
z=cbind(r*cos(theta),r*sin(theta))
# transform points of circle into points of ellipse using 
# svd of inverse covariance matrix
Sinv_svd=svd(Sinv)      # inverse of covariance matrix
xt=t(Sinv_svd$v)%*%diag(1/sqrt(Sinv_svd$d))%*%t(z) # transform from circle to ellispse
x=t(xt)
# translate the ellipse so that center is the estimated parameter value
x=x+matrix(rep(as.numeric(parest[1:2]),100),nrow=100,byrow=T)

plot(x[,1],x[,2],type="l",xlab="k1",ylab="k2",lwd=2)
points(parest$k1,parest$k2,pch=20,col="blue",cex=2)


# Simulation based estimation of uncertainty

# store original experimental data in a separate dataframe
dforig=df

# conc profile based on estimated initial concentrations, k1 and k2
cinit=unlist(parest[3:5])
t=dforig$time
parms=parest[1:2]
out=ode(y=cinit,times=t,func=rxnrate,parms=parms)

outsim=matrix(0,nrow=nrow(dforig),ncol=4)
outsim[,1]=out[,1]

# number of simulations
nsim=1000

parsim=matrix(0,nrow=nsim,ncol=5)
colnames(parsim)=c("k1","k2", "A", "B", "C")

for (i in 1:nsim){
  
  if(i %% 50 == 0) {
    print(i)
  }
  
  # Simulate data set by adding normal random variable with mean 0 and stdev from fit
  
  outsim[,2:4]=out[,2:4]+matrix(rnorm(3*nrow(dforig)),nrow=nrow(dforig),ncol=3)*ms
  df=data.frame(outsim)
  names(df)=c("time","ca","cb","cc")
  
  # get parameter estimate for the simulated dataset
  fitparms=as.numeric(parest)
  fitsim=nls.lm(par=fitparms,fn=ssq, control = nls.lm.control(nprint = 0), df = df)
  # store estimated parameters in the ith row
  parsim[i,]=coef(fitsim)
  
  
}

# plot the parameter estimates from the 1000 simulations
plot(parsim[,1],parsim[,2],xlab="k1",ylab="k2")
# overlay the 95% ellipse computed previously
lines(x[,1],x[,2],col="blue",lwd=2)

# percentage of parameters from simulation within the 95% ellipse
tmp=rep(0,length.out=nsim)
for(i in 1:nsim){
  tmp[i]=(parsim[i, 1:2]-as.numeric(parest[1:2]))%*%Sinv%*%(parsim[i, 1:2]-as.numeric(parest[1:2]))
}
sum(tmp <= qf(0.95,2,58)*2)/nsim

# session Info
sessionInfo()