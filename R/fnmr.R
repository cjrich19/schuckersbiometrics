fnmr_1_bs_reps=function(i,D,nreps=1000){list.i=as.numeric(names(table(i)))
#list.i is a list of all of the individuals
sample(list.i,length(list.i),replace=T)
pihat=sum(D)/length(D)
#pihat is the mean of the original decisions
pihat.b=rep(0,nreps)
nlist=length(list.i)
#nlist is the number of individuals
#bootstrapping the individuals
for (b in 1:nreps)
{
  list.b=sample(list.i,length(list.i),replace=T)
  #print(list.b)
  D.b=sample(D[i==list.b[1]],length(D[i==list.b[1]]),replace=T)
  #print(D.b)
  #bootstrapping the decisions
  for (bb in 2:nlist)
  {
    D.temp=sample(D[i==list.b[bb]],length(D[i==list.b[bb]]),replace=T)
    D.b=c(D.b,D.temp)
  }
  pihat.b[b]=sum(D.b)/length(D.b)
  #pihat.b is the mean of the bootstrapped decisions
}
e=pihat.b-pihat
#e is the difference between the original mean and bootstrapped mean decisions
#print(e)
hist(e,nclass=20)
endpts=quantile(e,c(0.025,0.975))
return(e)
}
fnmr_1_bs_ci=function(i,D,nreps=1000){list.i=as.numeric(names(table(i)))
#list.i is a list of all of the individuals
sample(list.i,length(list.i),replace=T)
pihat=sum(D)/length(D)
#pihat is the mean of the original decisions
pihat.b=rep(0,nreps)
nlist=length(list.i)
#nlist is the number of individuals
#bootstrapping the individuals
for (b in 1:nreps)
{
  list.b=sample(list.i,length(list.i),replace=T)
  #print(list.b)
  D.b=sample(D[i==list.b[1]],length(D[i==list.b[1]]),replace=T)
  #print(D.b)
  #bootstrapping the decisions
  for (bb in 2:nlist)
  {
    D.temp=sample(D[i==list.b[bb]],length(D[i==list.b[bb]]),replace=T)
    D.b=c(D.b,D.temp)
  }
  pihat.b[b]=sum(D.b)/length(D.b)
  #pihat.b is the mean of the bootstrapped decisions
}
e=pihat.b-pihat
#e is the difference between the original mean and bootstrapped mean decisions
#print(e)
hist(e,nclass=20)
endpts=quantile(e,c(0.025,0.975))
return(pihat-endpts)
}

fnmr_1_bs_pval=function(i,y,nreps=1000,pi0){
  #jk individual
  #boot decisions
  if(length(i)!=length(y)) {print("lengths unequal");break}
  pi.hat=mean(y)
  #pi.se=genuine.se.nomat(i,y,0.5,0.95)
  out2=rep(0,nreps)
  out=vector("list",nreps)
  data=cbind(i,y)
  indiv.i=names(table(i))
  n.i=length(indiv.i)




  out2[1]=(pi.hat)
  for  (t in 2:nreps)
  {

    list.i=sample(indiv.i,replace=T)
    #print(list.i)
    #print(list.j)
    #if(t %%50==0) print(as.numeric(list.i))
    for(t2 in 1:length(list.i))
    {
      temp=data[(i==as.numeric(list.i[t2])),]
      #print(temp)
      #n.temp=nrow(temp)
      #print(n.temp)
      #add1=temp[sample(1:n.temp,n.temp,replace=T),]
      #print("add")
      #print(add1)
      #temp=data[(i==as.numeric(list.j[t3])&j==as.numeric(list.i[t2])),]
      #n.temp=length(temp)
      #add2=temp[sample(1:n.temp,n.temp,replace=T)]

      out[[t]]= rbind(out[[t]],temp)
    }
    out2[t]=(mean(out[[t]][,2]))-(pi.hat-pi0)
  }
  print("p-value")
  print(mean(as.numeric (out2<= out2[[1]]) ) )
}

fnmr_1_ls_ci=function(i,y,threshold,level=0.95){
  N=length(i)
  order.i=order(i)
  i=i[order.i]
  y=y[order.i]
  #print(i)
  #zeros=matrix(rep(0,N*N),ncol=N)
  #rho=zeros
  #pqvar=zeros
  pihat=sum(y>threshold)/length(y)
  y2=as.numeric(y>threshold)-pihat
  #print(y2)
  pq=pihat*(1-pihat)
  rhosum=0
  rhocount=0
  varcount=0
  varsum=0
  for (h in 1:N)
  {
    for (hh in h:N)
    {
      if (h==hh) {
        #
        varcount=varcount+1
        varsum=varsum+y2[h]*y2[hh]
      }
      if (i[h]==i[hh]&h!=hh)
      {#print(i[h]);print(i[hh]);print("*");
        rhosum=rhosum+y2[h]*y2[hh]
        rhocount=rhocount+1
        #print(rhosum)
      }
    }
  }

  #print(rhocount)
  corr.hat=rhosum/rhocount/pq
  total.var= (varsum+max(2*rhosum,0))/varcount/varcount
  #print("rho -hat")
  #print(corr.hat)
  #print(rhocount)
  #print(varsum)
  Neff=pq/total.var
  #print("Neff")
  #print(Neff)
  se=sqrt(total.var)
  CI=c(pihat-qnorm(1-(1-level)/2)*se,pihat+qnorm(1-(1-level)/2)*se)
  CI.binom=c(pihat-2*sqrt(pq/varcount),pihat+2*sqrt(pq/varcount))
  print("CI")
  return(CI)
  #cat(pihat,"+/-",se,"\n")
  #print("CI.binom")
  #cat(pihat,"+/-",sqrt(pq/varcount),"\n")

  return(se)

}

fnmr_1_ls_pval=function(i,y,threshold,pi0){
  N=length(i)
  order.i=order(i)
  i=i[order.i]
  y=y[order.i]
  #print(i)
  #zeros=matrix(rep(0,N*N),ncol=N)
  #rho=zeros
  #pqvar=zeros
  pihat=sum(y>threshold)/length(y)
  y2=as.numeric(y>threshold)-pihat
  #print(y2)
  pq=pihat*(1-pihat)
  rhosum=0
  rhocount=0
  varcount=0
  varsum=0
  for (h in 1:N)
  {
    for (hh in h:N)
    {
      if (h==hh) {
        #
        varcount=varcount+1
        varsum=varsum+y2[h]*y2[hh]
      }
      if (i[h]==i[hh]&h!=hh)
      {#print(i[h]);print(i[hh]);print("*");
        rhosum=rhosum+y2[h]*y2[hh]
        rhocount=rhocount+1
        #print(rhosum)
      }
    }
  }

  #print(rhocount)
  corr.hat=rhosum/rhocount/pq
  total.var= (varsum+max(2*rhosum,0))/varcount/varcount
  ##print("rho -hat")
  ##print(corr.hat)
  #print(rhocount)
  #print(varsum)
  Neff=pq/total.var
  ##print("Neff")
  ##print(Neff)
  ##print("Neff*pi0")
  ##print(Neff*pi0)
  ##print("se")
  se=sqrt(total.var)
  ##print(se)
  #CI=c(pihat-qnorm(1-(1-level)/2)*se,pihat+qnorm(1-(1-level)/2)*se)
  #CI.binom=c(pihat-2*sqrt(pq/varcount),pihat+2*sqrt(pq/varcount))
  ##print("z-test")
  ##cat("(",pihat,"-",pi0,") / ",se,"\n")
  ##print((pihat-pi0)/se)
  print("pvalue")
  pval=pnorm((pihat-pi0)/se)
  cat(pval,"\n")
  #print("CI.binom")
  #cat(pihat,"+/-",sqrt(pq/varcount),"\n")

  ##return(se)

}


fnmr_2i_bs_ci=function(i1,y1,i2,y2,nreps=1000,level){
  #jk individual
  #boot decisions
  #if(length(i)!=length(y)) {print("lengths unequal");break}
  hold1=fnmr_1_bs_reps(i1,y1,nreps)
  hold2=fnmr_1_bs_reps(i2,y2,nreps)
  diff=hold1-hold2-(mean(y1)-mean(y2))
  diff.est=mean(y1)-mean(y2)

  print(diff.est-quantile(diff,c(1-(1-level)/2,(1-level)/2)))
  return(diff)
}

fnmr_2i_bs_pval=function(i1,y1,i2,y2,nreps=1000){
  hold1=fnmr_1_bs_reps(i1,y1,nreps)
  hold2=fnmr_1_bs_reps(i2,y2,nreps)
  diff=hold1-hold2-(mean(y1)-mean(y2))
  diff.est=mean(y1)-mean(y2)
  diff[1]=diff.est



  print("diff.est")
  print(diff.est)

  print("p-value")

  print(mean(as.numeric( diff <= diff[1]) ) )

}


fnmr_2p_bs_ci=function(i1,y1,y2,nreps=1000,level=0.95){
  #jk individual
  #boot decisions
  print("pi1")
  print(mean(y1))
  print("pi2")
  print(mean(y2))
  if(length(y1)!=length(y2)) {print("lengths unequal");break}
  diff=fnmr_1_bs_reps(i1,y1-y2,nreps)
  diff.est=mean(y1)-mean(y2)
  print("diff")
  print(diff.est)
  print(diff.est-quantile(diff-diff.est,c(1-(1-level)/2,(1-level)/2)))
  return(diff)
}


fnmr_2p_bs_pval=function(i1,y1,y2,nreps=1000,level=0.95){
  #jk individual
  #boot decisions
  print("pi1")
  print(mean(y1))
  print("pi2")
  print(mean(y2))
  if(length(y1)!=length(y2)) {print("lengths unequal");break}
  diff=fnmr_1_bs_reps(i1,y1-y2,nreps)-(mean(y1)-mean(y2))
  diff.est=mean(y1)-mean(y2)

  diff[1]=diff.est
  print("diff.est")
  print(diff.est)

  print("p-value")

  print(mean(as.numeric( diff <= diff[1]) ) )

}


fnmr_2i_ls_ci=function(i1,y1,i2,y2,level=0.95){
  pi1hat=mean(y1)
  pi2hat=mean(y2)
  se1=fnmr_1_ls_ci(i1,y1,0.5,level)
  se2=fnmr_1_ls_ci(i2,y2,0.5,level)

  se=sqrt(se1^2+se2^2)

  CI=c(pi1hat-pi2hat-qnorm(1-(1-level)/2)*se,pi1hat-pi2hat+qnorm(1-(1-level)/2)*se)
  print(CI)
  ##return(se)
}


fnmr_2i_ls_pval=function(i1,y1,i2,y2){
  level=0.95
  pi1hat=mean(y1)
  pi2hat=mean(y2)
  se1=fnmr_1_ls_ci(i1,y1,0.5,level)
  se2=fnmr_1_ls_ci(i2,y2,0.5,level)
  #print("diff")
  #print(pi1hat-pi2hat)
  pooled=mean(c(y1,y2))
  print(pooled)
  se.p=sqrt(pooled*(1-pooled)*(se1^2/pi1hat/(1-pi1hat)+se2^2/pi2hat/(1-pi2hat)))
  #se=fnmr.diff2.indep(i1,y1,i2,y2,level)
  zstat=(pi1hat-pi2hat)/se.p
  pval=pnorm(zstat)
  print("Z")
  print(zstat)
  print("p-value")
  print(pval)
  ##return(se.p)
}


fnmr_2p_ls_ci=function(i1,y1,y2,level=0.95){
  pi1hat=mean(y1)
  pi2hat=mean(y2)
  se1=fnmr_1_ls_ci(i1,y1,0.5,level)
  se2=fnmr_1_ls_ci(i1,y2,0.5,level)
  covvar=fnmr_cov_paired(i1,y1,y2)
  print("covvar")
  print(covvar)
  print("diff")
  print(pi1hat-pi2hat)
  se=sqrt(se1^2+se2^2-2*covvar)
  print("CI")
  CI=c(pi1hat-pi2hat-qnorm(1-(1-level)/2)*se,pi1hat-pi2hat+qnorm(1-(1-level)/2)*se)
  print(CI)
  return(se)
}


fnmr_2p_ls_pval=function(i1,y1,y2){
  level=0.95
  pi1hat=mean(y1)
  pi2hat=mean(y2)
  pooled=mean(c(y1,y2))
  #ci1=fnmr_1_ls_ci(i1,y1,0.5,level)
  #se1=(ci1[2]-ci1[1])/2/qnorm(1-(1-level)/2)
  se1= fnmr_se_hyp(i1,y1,pi_hyp=pooled)
  #ci2=fnmr_1_ls_ci(i1,y2,0.5,level)
  #se2=(ci2[2]-ci2[1])/2/qnorm(1-(1-level)/2)
  se2=fnmr_se_hyp(i1,y2,pi_hyp=pooled)
  #print(se1)
  #print(se2)
  #print("pooled")
  #print(pooled)
  x1=y1-pooled
  x2=y2-pooled
  covvar=sum(x1*x2)/length(i1)/length(i1)

  #print("covvar")
  #print(covvar)
  #print("diff")
  #print(pi1hat-pi2hat)
  #print(pooled*(1-pooled)*(se1^2/pi1hat/(1-pi1hat)+se2^2/pi2hat/(1-pi2hat)))
  se.p=sqrt(pooled*(1-pooled)*(se1^2/pi1hat/(1-pi1hat)+se2^2/pi2hat/(1-pi2hat)) -2*covvar)
  #print(se.p)
  #se=fnmr.diff2.paired(i1,y1,y2,0.95)
  #print("CI")
  zstat=(pi1hat-pi2hat)/se.p
  pval=pnorm(zstat)
  #print("Z")
  #print(zstat)
  print("p-value")
  #print(pval)
  return(pval)
}

fnmr_se=
  function (i, y, threshold = 0.5, level = 0.95)
  {
    N = length(i)
    order.i = order(i)
    i = i[order.i]
    y = y[order.i]
    pihat = sum(y > threshold)/length(y)
    y2 = as.numeric(y > threshold) - pihat
    pq = pihat * (1 - pihat)
    rhosum = 0
    rhocount = 0
    varcount = 0
    varsum = 0
    for (h in 1:N) {
      for (hh in h:N) {
        if (h == hh) {
          varcount = varcount + 1
          varsum = varsum + y2[h] * y2[hh]
        }
        if (i[h] == i[hh] & h != hh) {
          rhosum = rhosum + y2[h] * y2[hh]
          rhocount = rhocount + 1
        }
      }
    }
    corr.hat = rhosum/rhocount/pq
    total.var = (varsum + max(2 * rhosum, 0))/varcount/varcount
    #print("rho -hat")
    #print(corr.hat)
    Neff = pq/total.var
    #print("Neff")
    #print(Neff)
    se = sqrt(total.var)
    CI = c(pihat - qnorm(1 - (1 - level)/2) * se, pihat + qnorm(1 -
                                                                  (1 - level)/2) * se)
    CI.binom = c(pihat - 2 * sqrt(pq/varcount), pihat + 2 * sqrt(pq/varcount))
    #print("CI")
    #cat(pihat, "+/-", se, "\n")
    return(se)
  }

fnmr_se_hyp=
  function (i, y,threshold = 0.5, pi_hyp=sum(y>threshold)/length(y),level = 0.95)
  {
    N = length(i)
    order.i = order(i)
    i = i[order.i]
    y = y[order.i]
    #pihat = sum(y > threshold)/length(y)
    y2 = as.numeric(y > threshold) - pi_hyp
    pq = pi_hyp * (1 - pi_hyp)
    rhosum = 0
    rhocount = 0
    varcount = 0
    varsum = 0
    for (h in 1:N) {
      for (hh in h:N) {
        if (h == hh) {
          varcount = varcount + 1
          varsum = varsum + y2[h] * y2[hh]
        }
        if (i[h] == i[hh] & h != hh) {
          rhosum = rhosum + y2[h] * y2[hh]
          rhocount = rhocount + 1
        }
      }
    }
    corr.hat = rhosum/rhocount/pq
    total.var = (varsum + max(2 * rhosum, 0))/varcount/varcount
    #print("rho -hat")
    #print(corr.hat)
    Neff = pq/total.var
    #print("Neff")
    #print(Neff)
    se = sqrt(total.var)
    #print("CI")
    #cat(pihat, "+/-", se, "\n")
    return(se)
  }


fnmr_cov_paired=function(i1,y1,y2){
  pi1hat=mean(y1)
  pi2hat=mean(y2)
  x1=y1-pi1hat
  x2=y2-pi2hat
  covvar=sum(x1*x2)/length(i1)/length(i1)
  return(covvar)
}


fnmr_2i_rand_pval=function(Dlist,nreps=1000){
  diffobs=mean(Dlist[[1]][,2])-mean(Dlist[[2]][,2])
  ###Extract list of individuals from each device
  Ilist=c(as.numeric(names(table(Dlist[[1]][,1]))))
  #Ilist is a list of all of the individuals
  G=2
  n.i=rep(0,G)
  #n.i is the number of individuals
  n.i[1]=length(Ilist)
  ###n.i is the number of individuals in each set of data
  for(h in 2:G)
  {
    Ilist=c(Ilist,as.numeric(names(table(Dlist[[h]][,1]))))
    n.i[h]=length(as.numeric(names(table(Dlist[[h]][,1]))))
  }

  ###Extract the coordinating group numbers of the individuals
  glist=rep(1,n.i[1])
  ###glist contains the group numbers of each individual
  for(i in 2:G)
  {
    glist=c(glist,rep(i,n.i[i]))
  }
  Iglist=cbind(glist,Ilist)
  #Dprime2 is a matrix
  ##of the group number of the specific individual and the individual
  #print(Iglist)
  Dprime2=matrix(Iglist,length(Ilist),2,byrow=FALSE)
  ###Create a D2/randomize the groups
  G=length(Dlist)
  n.g=rep(0,G)
  for(j in 1:G)
  {
    n.g[j]=length(Dlist[[j]][,1])
  }
  #n.g is the length of each group
  k=nrow(Dprime2)
  diffrand=rep(0,nreps)
  diffrand[1]=diffobs
  for (n in 2:nreps)
  {
    D2=Dprime2[sample(1:k,k,replace=F),]
    ###D2 is a randomized matrix of the individuals
    D2a=list(D2[1:n.i[1],])
    for (i in 2:G)
    {
      D2a=c(D2a,list(D2[(sum(n.i[1:(i-1)])+1):sum(n.i[1:i]),]))
    }
    #print("Dprime2")
    #print(Dprime2)
    #print("D2a")
    #print(D2a)
    ###sTEP 5 - Calculate Drand (spliting D2 up into the original
    ##length of the groups (n.i)
    xmat=Dlist[[D2a[[1]][1,1]]]
    xmat=matrix(xmat[xmat[,1]==D2a[[1]][1,2],],ncol=2,byrow=F)
    #print("frand:xmat")
    #print(xmat)
    xlen=NROW(xmat)
    #bs=sample(1:xlen,xlen,replace=TRUE)
    Drand=list(cbind(rep(1,xlen),xmat[,2]))
    for(ii in 2:n.i[1])
    {
      xmat=Dlist[[D2a[[1]][ii,1]]]
      xmat=matrix(xmat[xmat[,1]==D2a[[1]][ii,2],],ncol=2,byrow=F)
      #print("frand:xmat2")
      #print(xmat)
      xlen=NROW(xmat)
      #bs=sample(1:xlen,xlen,replace=TRUE)
      Drand[[1]]=rbind(Drand[[1]],cbind(rep(ii,xlen),xmat[,2]))
    }
    #print("Drand[[1]]")
    #print(Drand)
    for(g in 2:G)
    {
      xmat=Dlist[[D2a[[g]][1,1]]]
      xmat=matrix(xmat[xmat[,1]==D2a[[g]][1,2],],ncol=2,byrow=F)
      #print("frand:xmat3")
      #print(xmat)
      xlen=NROW(xmat)
      #bs=sample(1:xlen,xlen,replace=TRUE)
      Drand=c(Drand,list(cbind(rep(1,xlen),xmat[,2])))
      ###Drand is the partitioned, bootstrap of the randomized
      ##individuals and decisions.
      for(ii in 2:n.i[g])
      {
        xmat=Dlist[[D2a[[g]][ii,1]]]
        xmat=matrix(xmat[xmat[,1]==D2a[[g]][ii,2],],ncol=2,byrow=F)
        #print("frand:xmat4")
        #print(xmat)
        xlen=NROW(xmat)
        #bs=sample(1:xlen,xlen,replace=TRUE)
        Drand[[g]]=rbind(Drand[[g]],cbind(rep(ii,xlen),xmat[,2]))
        ###Drand is a list of the randomized matrices
        ##split back up into the original n.g's			}

      }
      #Step 7 - Find "F" random

      diffrand[n]=mean(Drand[[1]][,2])-mean(Drand[[2]][,2])
    }
    #print("Drand")
    #print(Drand)
  }



  #print(Frand)
  Pvalue=mean(as.numeric(diffrand<=diffobs))
  hist(diffrand,xlab="diff-rand",nclass=40,main=" ")
  abline(v=diffobs,col="red",lwd=4,lty=3)
  #print(Fobs)
  print("Pvalue")
  print(Pvalue)
}


fnmr_2p_rand_pval=function(data,nreps=1000){
  #F-test paired.2+
  pi1hat=mean(data[,2])
  pi2hat=mean(data[,3])
  diffobs=pi1hat-pi2hat
  n=length(data)
  diffrand=rep(0,nreps)
  diffrand[1]=diffobs
  for (i in 2:nreps)

  {
    datatemp=data
    for(j in 1:nrow(data))
    {
      datatemp[j,]=c(data[j,1],data[j,sample(2:3,2,replace=F)])
    }

    diffrand[i]=mean(datatemp[,2])-mean(datatemp[,3])
    #print(Frand)
  }

  Pvalue=mean(as.numeric(diffrand>=diffobs))
  hist(diffrand,nclass=40,xlab="diff",main=" ")
  abline(v=diffobs,col="red",lwd=4,lty=3)
  print("Pvalue")
  print(Pvalue)
}


fnmr_3i_bs_pval=function(Dlist,nreps=1000){

  #calculate F


  G=length(Dlist)
  n.g=rep(0,G)
  pi.g=rep(0,G)
  for (t in 1:G)
  {
    pi.g[t]=mean(Dlist[[t]][,2])
    n.g[t]=nrow(Dlist[[t]])
  }
  pibar=sum(n.g*pi.g)/sum(n.g)
  delta=pi.g-pibar
  ##print(pibar)
  ##print("pi's")
  ##print(pi.g)
  #for (jj in 1:G)
  #{
  #Dlist[[jj]][,2]=Dlist[[jj]][,2]
  #
  #}
  #print("1.0")

  F1=fcalc3(Dlist,rep(0,G))
  ##print("F")
  ##print(F1)
  Fboot=rep(0,nreps)
  Fboot[1]=F1
  ##print(F1)
  #Dprime=vector("list",G)
  #print("2.0")
  for (j in 2:nreps)
  {
    ##if (j%%10==0) print(j)
    #cat(" j ",j,"\n");
    #rm(Dprime);
    #Dprime=vector("list",G)
    Dprime=fnmr_1i_bs_supplementary(Dlist[[1]][,1],Dlist[[1]][,2])
    for (g in 2:G)
    {
      Dprime=c(Dprime,fnmr_1i_bs_supplementary(Dlist[[g]][,1],Dlist[[g]][,2]))
      #print(rboot)
      #Dprime=c(Dprime,rboot)
      #print(Dprime)
    }

    Fboot[j]=fcalc3(Dprime,delta)
  }
  print("pvalue")
  print(mean(Fboot >= F1))

  hist(Fboot,nclass=40,main=" ")
  abline(v=F1,col="red",lty=3,lwd=4)
  ##return(Fboot)
}


fnmr_3p_bs_pval=function(i,ymat,nreps=1000){
  #calculate F
  delta=rep(0,ncol(ymat))
  F1=fcalc3p(cbind(i,ymat),delta)
  ##print("F")
  ##print(F1)
  ##print("pi's")
  ##print(apply(ymat,2,mean))
  pibar=mean(ymat)
  delta=rep(0,ncol(ymat))
  for (jj in 1:ncol(ymat))
  {
    delta[jj]=mean(ymat[,jj])-pibar

  }
  Fboot=rep(0,nreps)
  Fboot[1]=F1


  for (j in 2:nreps)
  {
    ##print(j)
    g=fnmr_1p_bs_supplementary(i,ymat)
    datastar=g[[1]]
    #print(datastar)
    Fboot[j]=fcalc3p(datastar,delta)
  }
  print("pvalue")
  print(mean(Fboot >= F1))

  hist(Fboot,nclass=40,main=" ")
  abline(v=F1,col="red",lty=3,lwd=4)
  ##return(Fboot)
}


fcalc3=function(Dlist,delta){

  G=length(Dlist)
  #print("G");print(G);
  ###G is the number of sets of data
  n.g=rep(0,G)
  ###n.g is the number of data points in each set of data
  pihat.g=rep(0,G)
  ###pihat.g is the mean Decisions of each set of data

  Dprime=c(Dlist[[1]][,2])
  for(k in 2:G)
  {
    Dprime=c(Dprime,Dlist[[k]][,2])
  }
  ###Dprime is a list of all of the individuals
  n=length(Dprime)
  #pibar=mean(Dprime)
  #print(pibar)
  for(i in 1:G)
  {
    #print(Dlist[[i]][1:10,2])
    pihat.g[i]=mean(Dlist[[i]][,2])+1e-15
    n.g[i]=length(Dlist[[i]][,2])
  }
  pibar=sum(n.g*pihat.g)/sum(n.g)
  var.pihat.g=rep(0,G)
  ###var.pihat.g is the variance of each set of data
  for(i in 1:G){
    var.pihat.g[i]=var.pihat(cbind(Dlist[[i]][,1],Dlist[[i]][,2]-delta[i]))
  }

  ###pibar is the mean of all of the decisions
  #print(delta)
  #print(pihat.g)
  pib=pihat.g-delta
  #print(pib)
  var2=var.pihat.g/(pihat.g)/(1-pihat.g)*pib*(1-pib)
  #print(pib)
  F=(sum(n.g*(pib-pibar)^2/(G-1)))/sum((n.g^2*var2)/(n-G))
  if(F<0)F=1e-80
  if (F>10^90) F=10^90
  return(F)
}


fcalc3p=function(Dmat,delta=rep(0,ncol(Dmat)-1)){

  G=ncol(Dmat)-1
  n.g=nrow(Dmat)
  #print("G");print(G);
  ###G is the number of sets of data
  ###n.g is the number of data points in each set of data
  pihat.g=rep(0,G)
  ###pihat.g is the mean Decisions of each set of data

  ###Dprime is a list of all of the individuals
  #pibar=mean(Dprime)
  #print(pibar)
  for(i in 1:G)
  {
    pihat.g[i]=mean(Dmat[,i+1])+1e-15
  }
  pibar=mean(pihat.g)
  var.pihat.g=rep(0,G)
  ###var.pihat.g is the variance of each set of data
  for(i in 1:G){
    ##print(cbind(Dmat[,1],Dmat[,i+1]-delta[i]))
    var.pihat.g[i]=var.pihat(cbind(Dmat[,1],Dmat[,i+1]-delta[i]))
  }

  ###pibar is the mean of all of the decisions
  pib=pihat.g-delta
  var2=var.pihat.g/(pihat.g)/(1-pihat.g)*pib*(1-pib)+1e-10
  #print(pib)
  F=(sum((pib-pibar)^2/(G-1)))/sum((n.g*var2)/(length(Dmat[,2:G])-G))
  if(F>10^90) F=10^90
  return(F)

}




fnmr_3p_rand_pval=function(data,nreps=1000){
  #F-test paired.2+
  Fobs=fcalc3p(data)
  ##print("Fobs");print(Fobs)
  n=length(data)
  Frand=rep(0,nreps)
  G=ncol(data)-1
  Frand[1]=Fobs
  for (i in 2:nreps)

  {
    datatemp=data
    for(j in 1:nrow(data))
    {
      datatemp[j,]=c(data[j,1],data[j,sample(2:(G+1),G,replace=F)])
    }

    Frand[i]=fcalc3p(datatemp)
    #print(Frand)
  }

  Pvalue=mean(as.numeric(Frand>=Fobs))
  hist(Frand,nclass=20,xlab="Frand",main=" ")
  abline(v=Fobs,col="red",lwd=4,lty=3)
  print("P value")
  return(Pvalue)
}


fnmr_3i_rand_pval=function(Dlist,nreps=1000){
  delta=rep(0,length(Dlist))
  Fobs=fcalc3(Dlist,delta)
  ##print("F")
  ##print(Fobs)
  ###Extract list of individuals from each device
  Ilist=c(as.numeric(names(table(Dlist[[1]][,1]))))
  #Ilist is a list of all of the individuals
  G=length(Dlist)
  ##print("G");print(G)
  n.i=rep(0,G)
  #n.i is the number of individuals
  n.i[1]=length(Ilist)
  ###n.i is the number of individuals in each set of data
  for(h in 2:G)
  {
    Ilist=c(Ilist,as.numeric(names(table(Dlist[[h]][,1]))))
    n.i[h]=length(as.numeric(names(table(Dlist[[h]][,1]))))
  }
  #print("Ilist")
  ##print(n.i)
  ###Extract the coordinating group numbers of the individuals
  glist=rep(1,n.i[1])
  ###glist contains the group numbers of each individual
  for(i in 2:G)
  {
    glist=c(glist,rep(i,n.i[i]))
  }
  Iglist=cbind(glist,Ilist)
  #Dprime2 is a matrix of the group number of the specific individual and the individual
  #print(Iglist)
  Dprime2=matrix(Iglist,length(Ilist),2,byrow=FALSE)
  #print(Dprime2)
  ###Create a D2/randomize the groups
  G=length(Dlist)
  n.g=rep(0,G)
  for(j in 1:G)
  {
    n.g[j]=length(Dlist[[j]][,1])
  }
  #n.g is the length of each group
  k=nrow(Dprime2)
  Frand=rep(0,nreps)
  Frand[1]=Fobs
  #print("1.")
  for (n in 2:nreps)
  {
    ##if (n%%10==0) print(n)
    D2=Dprime2[sample(1:k,k,replace=F),]
    ###D2 is a randomized matrix of the individuals
    #print(D2)
    D2a=list(D2[1:n.i[1],])
    #print(D2a)
    for (i in 2:G)
    {
      D2a=c(D2a,list(D2[(sum(n.i[1:(i-1)])+1):sum(n.i[1:i]),]))
    }
    #print("Dprime2")
    #print(Dprime2)
    #print("D2a")
    #print(D2a)
    ###sTEP 5 - Calculate Drand (spliting D2 up into the original length of the groups (n.i)
    xmat=as.matrix(Dlist[[D2a[[1]][1,1]]])
    #print("xmat")
    #print(xmat)
    xmat=xmat[xmat[,1]==D2a[[1]][1,2],]
    #print("frand:xmat")
    #print(xmat)
    xlen=NROW(xmat)
    #bs=sample(1:xlen,xlen,replace=TRUE)
    Drand=list(cbind(rep(1,xlen),xmat[,2]))
    #print("Dr")
    #print(Drand)
    for(ii in 2:n.i[1])
    {
      xmat=as.matrix(Dlist[[D2a[[1]][ii,1]]])
      xmat=xmat[xmat[,1]==D2a[[1]][ii,2],]
      #print("frand:xmat2")
      #print(xmat)
      xlen=NROW(xmat)
      #bs=sample(1:xlen,xlen,replace=TRUE)
      Drand[[1]]=rbind(Drand[[1]],cbind(rep(ii,xlen),xmat[,2]))
    }
    #print("Drand[[1]]")
    #print(Drand)
    for(g in 2:G)
    {
      xmat=as.matrix(Dlist[[D2a[[g]][1,1]]])
      #print(xmat)
      xmat=xmat[xmat[,1]==D2a[[g]][1,2],]
      #print("frand:xmat3")
      #print(xmat)
      xlen=NROW(xmat)
      #bs=sample(1:xlen,xlen,replace=TRUE)
      Drand=c(Drand,list(cbind(rep(1,xlen),xmat[,2])))
      #print(Drand)
      ###Drand is the partitioned, bootstrap of the randomized individuals and decisions.
      for(ii in 2:n.i[g])
      {
        xmat=as.matrix(Dlist[[ D2a[[g]][ii,1] ]])
        xmat=xmat[xmat[,1]==D2a[[g]][ii,2],]
        #print("frand:xmat4")
        #print(xmat)
        xlen=NROW(xmat)
        #bs=sample(1:xlen,xlen,replace=TRUE)
        Drand[[g]]=rbind(Drand[[g]],cbind(rep(ii,xlen),xmat[,2]))
      }
      #print(Drand[[g]])
    }
    ###Drand is a list of the randomized matrices split back up into the original n.g's


    #Step 7 - Find "F" random
    #print("huh")
    #print(Drand)
    Frand[n]=fcalc3(Drand,delta)

    #	print("Drand")
    #print(Drand)
  }



  #print(Frand)
  Pvalue=mean(as.numeric(Frand>=Fobs))
  hist(Frand,xlab="F-rand",nclass=40,main=" ")
  abline(v=Fobs,col="red",lwd=4,lty=3)
  #print(Fobs)
  print("Pvalue")

  print(Pvalue)
}

var.pihat=function (X)
{

  if (sum(X[, 2]) == 0) {
    return(0)
  }
  if (sum(X[, 2]) == nrow(X)) {
    return(0)
  }
  Xtab = table(X[, 1], X[, 2])
  se = fnmr_se_supplementary_no_out(X[, 1], X[, 2], threshold = 0.5)
  return(se^2)
}

fnmr_1i_bs_supplementary=function(i,y)
{
  #jk individual
  #boot decisions
  if(length(i)!=length(y)) {print("lengths unequal");break}
  #out2=rep(0,nreps)
  out=vector("list",1)
  data=cbind(1:length(i),i,y)
  indiv.i=names(table(i))
  n.i=length(indiv.i)
  list.i=rep(0,n.i)

  list.i=sample(indiv.i,replace=T)
  #print(list.i)
  #print(list.j)

  for(t2 in 1:length(list.i))
  {

    #temp=data[(i==as.numeric(list.i[t2])),2:3]
    #print(temp)

    temp=data[(i==as.numeric(list.i[t2])),3]
    n.temp=nrow(temp)
    #print(n.temp)
    #add1=temp[sample(1:n.temp,n.temp,replace=T),]
    #print("add")
    #print(add1)
    #temp=data[(i==as.numeric(list.j[t3])&j==as.numeric(list.i[t2])),]
    #n.temp=length(temp)
    #add2=temp[sample(1:n.temp,n.temp,replace=T)]

    out[[1]]= rbind(out[[1]],cbind(rep(t2,length(temp)),temp))
    #out[[1]]=rbind(out[[1]],temp)
  }


  return(out)
}


fnmr_1p_bs_supplementary=function(i,ymat)
{
  #jk individual
  #boot decisions
  if(length(i)!=nrow(ymat)) {print("lengths unequal");break}
  #out2=rep(0,nreps)
  out=vector("list",1)
  data=cbind(1:length(i),i,ymat)
  G=ncol(data)
  indiv.i=names(table(i))
  n.i=length(indiv.i)
  list.i=rep(0,n.i)

  list.i=sample(indiv.i,replace=T)
  #print(list.i)
  #print(list.j)

  for(t2 in 1:length(list.i))
  {

    #temp=data[(i==as.numeric(list.i[t2])),2:3]
    #print(temp)

    temp=data[(i==as.numeric(list.i[t2])),3:G]
    n.temp=nrow(temp)
    #print(n.temp)
    #add1=temp[sample(1:n.temp,n.temp,replace=T),]
    #print("add")
    #print(add1)
    #temp=data[(i==as.numeric(list.j[t3])&j==as.numeric(list.i[t2])),]
    #n.temp=length(temp)
    #add2=temp[sample(1:n.temp,n.temp,replace=T)]

    out[[1]]= rbind(out[[1]],cbind(rep(t2,nrow(temp)),temp))
    #out[[1]]=rbind(out[[1]],temp)
  }


  return(out)
}


fnmr_se_supplementary_no_out=function(i,y,threshold,level=0.95)
{
  N=length(i)
  order.i=order(i)
  i=i[order.i]
  y=y[order.i]
  #print(i)
  #zeros=matrix(rep(0,N*N),ncol=N)
  #rho=zeros
  #pqvar=zeros
  pihat=sum(y>threshold)/length(y)
  y2=as.numeric(y>threshold)-pihat
  #print(y2)
  pq=pihat*(1-pihat)
  rhosum=0
  rhocount=0
  varcount=0
  varsum=0
  for (h in 1:N)
  {
    for (hh in h:N)
    {
      if (h==hh) {
        #
        varcount=varcount+1
        varsum=varsum+y2[h]*y2[hh]
      }
      if (i[h]==i[hh]&h!=hh)
      {#print(i[h]);print(i[hh]);print("*");
        rhosum=rhosum+y2[h]*y2[hh]
        rhocount=rhocount+1
        #print(rhosum)
      }
    }
  }

  #print(rhocount)
  corr.hat=rhosum/rhocount/pq
  total.var= (varsum+max(2*rhosum,0))/varcount/varcount
  #print("rho -hat")
  #print(corr.hat)
  #print(rhocount)
  #print(varsum)
  Neff=pq/total.var
  #print("Neff")
  #print(Neff)
  se=sqrt(total.var)
  CI=c(pihat-qnorm(1-(1-level)/2)*se,pihat+qnorm(1-(1-level)/2)*se)
  CI.binom=c(pihat-2*sqrt(pq/varcount),pihat+2*sqrt(pq/varcount))
  #print("CI")
  #cat(pihat,"+/-",se,"\n")
  #print("CI.binom")
  #cat(pihat,"+/-",sqrt(pq/varcount),"\n")

  return(se)

}


