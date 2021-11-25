library("MSBVAR")
library("Matrix")
library("MASS")

estimation<-function(x,y,n,lam,varth,gam,al,ph,alphatrue)
{
  dx=ncol(x)
  #muini=y
  #betaini=seq(0,0,length=dx)
  Inx=cbind(1,x)
  thetain=solve(t(Inx)%*%Inx)%*%t(Inx)%*%y
  #muini=rep(1,length=n)*thetain[1]
  betaini=thetain[2:length(thetain)]
  muini=y-x%*%betaini
  
  etaini=seq(0,0,length=(n*(n-1)/2))
  #etatrue=seq(0,0,length=(n*(n-1)/2))
  delta=matrix(0,nrow=n,ncol=(n*(n-1)/2))
  e=diag(n)
  for (i in 1:(n-1)){
    ind=(i-1)*n-i*(i-1)/2
    etaini[(ind+1):(ind+n-i)]=muini[i]-muini[(i+1):n]
    #etatrue[(ind+1):(ind+n-i)]=mu[i]-mu[(i+1):n]
    delta[,(ind+1):(ind+n-i)]=e[,i]-e[,(i+1):n]
  }
  delta=t(delta)
  upsini=seq(0,0,length=(n*(n-1)/2))
  weight=exp(-ph*etaini^2)
  #weight=1/abs(etaini)^2
  
  muold=muini
  betaold=betaini
  etaold=etaini
  upsold=upsini
  varthold=varth
  roold=0
  Px=solve(t(x)%*%x)%*%t(x)
  Qx=x%*%Px
  epsilon=10^(-3)
  step=0
  rnew=2
  snew=2
  rr=NULL
  ss=NULL
  mud=NULL
  while(rnew>epsilon)
  {
    
    step=step+1
    rold=delta%*%muold-etaold
    rold=sqrt(t(rold)%*%rold)
    #Iinv=(1+n*varthold)*diag(n)-varthold*rep(1,length=n)%*%t(rep(1,length=n))-Qx
    Iinv=diag(n)+varthold*t(delta)%*%delta-Qx
    Iinv=solve(Iinv)
    II=(diag(n)-Qx)%*%y+varthold*t(delta)%*%(etaold-(1/varthold)*upsold)
    munew=Iinv%*%II
    betanew=Px%*%(y-munew)
    del=delta%*%munew+(1/varthold)*upsold
    lam1=lam/varthold
    eta=abs(del)-lam1
    eta=eta*(eta>0)
    eta=sign(del)*eta
    
    if(al==1){
      lamL=lam1*weight
      etaL=abs(del)-lamL
      etaL=etaL*(etaL>0)
      etanew=sign(del)*etaL
      #etanew=eta
    }else if(al==2){
      etanew=(eta/(1-(varthold*gam)^(-1)))*(abs(del)<=(gam*lam))+del*(abs(del)>(gam*lam)) 
    }else if(al==3){
      eta1=eta*(abs(del)<=(lam+lam1))
      eta2=del*(abs(del)>(gam*lam))
      lam2=(lam/varthold)*(gam/(gam-1))
      etat=abs(del)-lam2
      etat=etat*(etat>0)
      etat=sign(del)*etat 
      eta3=(etat/(1-((gam-1)*varthold)^(-1)))*(abs(del)<=(gam*lam))*(abs(del)>(lam+lam1))
      etanew=eta1+eta2+eta3  #SCAD
    }else{
      etaL=abs(del)-lam1
      etaL=etaL*(etaL>0)
      dell=etaL*(del/abs(del))
      etanew=del*(abs(etaold)>=tau)+dell*(abs(etaold)<tau)
    }
    
    upsnew=upsold+varthold*(delta%*%munew-etanew)
    varthnew=(1*varthold)*(rold>(0.25*roold))+varthold*(rold<=(0.25*roold))
    varthnew=varthnew[1,1]
    #varthnew=varthold
    
    roold=rold
    rnew=delta%*%munew-etanew
    rnew=sqrt(t(rnew)%*%rnew)
    rold=rnew
    snew=t(delta)%*%(etanew-etaold)
    snew=sqrt(t(snew)%*%snew)
    mudiff=sqrt(t(muold-munew)%*%(muold-munew))
    
    muold=munew
    betaold=betanew
    etaold=etanew
    upsold=upsnew
    varthold=varthnew
    rr=c(rr,rnew)
    ss=c(ss,snew)
    mud=c(mud,mudiff)
  }
  
  seq=1:n
  group=matrix(0,nrow=n,ncol=n)
  alphaold=seq(0,0,length=n)
  zhat=matrix(0,nrow=n,ncol=n)
  K=1
  
  while(length(seq)>0)
  {
    i=seq[1]
    ind=(i-1)*n-i*(i-1)/2
    id=which(etaold[(ind+1):(ind+n-i)]==0)
    id=c(i,i+id)
    group[1:length(id),K]=id
    alphaold[K]=mean(muold[id])
    zhat[id,K]=1
    seq=seq [! seq %in% id]
    K=K+1
  }
  
  K=K-1
  alphaold=alphaold[1:K]
  ind=seq(1:length(alphaold))
  zhat=zhat[,1:K]
  zhat=as.matrix(zhat)
  iz=seq(0,0,length=length(alphatrue))
  if(length(alphaold)>1){
    for(ii in 1:length(alphatrue)){
      diff=abs(alphaold-alphatrue[ii])
      com=cbind(diff,ind)
      com=com[sort.list(com[,1]), ]	
      iz[ii]=com[1,2]
    }	
    alphaold1=alphaold[iz]
    alphaold2=alphaold[-iz]
    alphaold=c(alphaold1,alphaold2)
    zhat1=zhat[,iz]
    zhat2=zhat[,-iz]
    zhat=cbind(zhat1,zhat2)
  }
  #sig=sqrt(t(y-zhat%*%alphaold-x%*%betaold)%*%(y-zhat%*%alphaold-x%*%betaold)/(n-K-dx))
  
  
  return(list(K,muold,betaold,etaold,group,delta,alphaold,zhat))
}


lamoptimal<-function(x,y,n,varth,gam,al,ph)
{
  lamU=0.2
  lamL=1
  lams=seq(lamL,lamU,length=20)
  ss=length(lams)
  BIC1=seq(-1,-1,length=ss)
  BIC2=seq(-1,-1,length=ss)
  GCV=seq(-1,-1,length=ss)
  deg=seq(-1,-1,length=ss)
  Qnn=seq(-1,-1,length=ss)
  for(i in 1:ss){
    lam=lams[i]
    result=estimation(x,y,n,lam,varth,gam,al,ph,alphatrue)
    muhat=result[[2]]
    K=result[[1]]
    betahat=result[[3]]
    etahat=result[[4]]
    group=result[[5]]
    delta=result[[6]]
    df=K+ncol(x)
    # dff=degree(x,y,n,lam,varth,gam,al,ph,K,muhat,betahat,etahat,group,delta)
    # df=dff[[1]]
    deg[i]=df
    # Phat=matrix(0,nrow=n,ncol=K)
    # group=group[,1:K]
    #group=as.matrix(group)
    # for(i in 1:K){
    # gi=group[,i]
    # gi=gi[which(gi!=0)]
    #  Phat[gi,i]=1
    #   }
    # Pr=cbind(Phat,x)
    #  thetahat=solve(t(Pr)%*%Pr)%*%t(Pr)%*%y
    #  muhat=thetahat[1:ncol(Phat)]
    #  betahat=thetahat[(1+ncol(Phat)):ncol(Pr)]
    #  muhat=Phat%*%muhat
    Qn=t(y-muhat-x%*%betahat)%*%(y-muhat-x%*%betahat)/n
    Qn=Qn[1,1]
    Qnn[i]=Qn
    yhat=muhat+x%*%betahat
    BIC1[i]=log(Qn)+10*log(log(n+ncol(x)))*log(n)*df/n
    BIC2[i]=log(Qn)+log(n)*df/n
    GCV[i]=Qn/(1-df/n)^2
    
  }
  
  lam=lams[min(which(BIC1==min(BIC1)))]
  
  #lam=lams[min(which(GCV==min(GCV)))]
  
  return(list(lam,BIC))
}



nsim=100
n=100
dx=5

mux<-rep(0,dx)
rho=0.3
######exchangeable covariance matrix for x
sigx=rho*matrix(1,nrow=dx,ncol=dx)+(1-rho)*diag(dx)
sigx=sigx

Ksim=seq(0,0,length=nsim)
MSEmu=seq(0,0,length=nsim)
MSEbeta=seq(0,0,length=nsim)
MSEmuor=seq(0,0,length=nsim)
MSEbetaor=seq(0,0,length=nsim)
betasim=matrix(0,nrow=nsim,ncol=dx)
alphasim=matrix(0,nrow=nsim,ncol=n)
alphasig=matrix(0,nrow=nsim,ncol=n)
betasig=matrix(0,nrow=nsim,ncol=dx)
pvaluesim=matrix(NA,nrow=nsim,ncol=n)
alphasigor=matrix(0,nrow=nsim,ncol=2)
alphasimor=matrix(0,nrow=nsim,ncol=2)
betasigor=matrix(0,nrow=nsim,ncol=dx)
set.seed(1)


for(i in 1:nsim){
e=rnorm(n,mean=0,sd=0.5)

x=rmultinom(n, mux, sigx)
beta=runif(dx,min=0.5,max=1)
u=runif(n)
alp=2
#mu=(u<(1/4))*2*(-alp)+(u>=(1/4))*(u<(1/2))*(-alp)+(u>=(1/2))*(u<(3/4))*alp+(u>=(3/4))*2*alp

#mu=(u<(1/3))*alp+(u>(2/3))*(-alp)

mu=(u<=(1/2))*alp+(u>(1/2))*(-alp)
#z1=as.numeric(u<=(1/2))
#z2=as.numeric(u>(1/2))
#z=cbind(z1,z2)

#mu=2
z=matrix(1,nrow=n,ncol=1)

alphatrue=c(-alp,0,alp)
#alphatrue=mu
K=length(alphatrue)
#mu=2

y=mu+crossprod(t(x), beta)+e
#y=mu+x%*%beta+e

#Q=cbind(as.numeric(mu==alp),as.numeric(mu!=alp))
#QX=cbind(Q,x)
#theta=solve(t(QX)%*%QX)%*%t(QX)%*%y
#muhat=theta[1:2]
#muhat=Q%*%muhat
##betahat=theta[3:length(theta)]
#MSEmu[i]=sqrt(t(muhat-mu)%*%(muhat-mu)/n)
#MSEbeta[i]=sqrt(t(betahat-beta)%*%(betahat-beta)/ncol(x))

al=2 #2 MCP 1 LASSO 3 SCAD 4 truncated L1
varth=1
gam=3
ph=1
tau=0.5 # truncated parameter for truncated L1

lamopt=lamoptimal(x,y,n,varth,gam,al,ph)
lam=lamopt[[1]]
result=estimation(x,y,n,lam,varth,gam,al,ph,alphatrue)
muhat=result[[2]]
betahat=result[[3]]
Khat=result[[1]]
Ksim[i]=Khat
MSEmu[i]=sqrt(t(muhat-mu)%*%(muhat-mu)/n)
MSEbeta[i]=sqrt(t(betahat-beta)%*%(betahat-beta)/ncol(x))
alphahat=result[[7]]
zhat=result[[8]]
sighat=result[[9]]
betasim[i,]=betahat
alphasim[i,1:Khat]=alphahat
des=cbind(zhat,x)
deINV=solve(t(des)%*%des)
sign=sighat*sqrt(diag(deINV))
alphasig[i,1:Khat]=sign[1:Khat]
betasig[i,]=sign[(Khat+1):length(sign)]
an=seq(0,0,length=length(sign))

if(Khat>1){
for(j in 2:Khat){
an[1]=1
an[j]=-1
sigj=sighat*sqrt(t(an)%*%deINV%*%an)
zcrit=abs(alphahat[1]-alphahat[j])/sigj
zcrit=zcrit[1]
pvaluesim[i,(j-1)]=2*(1-pnorm(zcrit))
}
}
MOD=lm(y~z+x-1)
ORcoef=MOD$coef
alphaor=ORcoef[1:ncol(z)]
alphasimor[i,]=alphaor
muor=z%*%alphaor
betaor=ORcoef[(ncol(z)+1):(ncol(z)+ncol(x))]
MSEmuor[i]=sqrt(t(muor-mu)%*%(muor-mu)/n)
MSEbetaor[i]=sqrt(t(betaor-beta)%*%(betaor-beta)/ncol(x))
sigor=sqrt(t(y-z%*%alphaor-x%*%betaor)%*%(y-z%*%alphaor-x%*%betaor)/(n-K-dx))
des=cbind(z,x)
deINV=solve(t(des)%*%des)
sign=sigor*sqrt(diag(deINV))
alphasigor[i,1:K]=sign[1:K]
betasigor[i,]=sign[(K+1):length(sign)]

}


