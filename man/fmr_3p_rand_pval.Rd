\name{fmr_3p_rand_pval}
\alias{fmr_3p_rand_pval}
\title{fmr_3p_rand_pval
}
\description{3 sample paired randomized method
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fmr_3p_rand_pval(i,k,ymat,nreps=1000)
}
\arguments{
  \item{i}{id
}
  \item{k}{replications
  }
  \item{ymat}{binary indicator of match
  }
  \item{nreps}{number of repetitions
  }
}

\value{p value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
ymat=cbind(BANCA.G.g1.imp[,1:2],as.numeric(BANCA.G.g1.imp[,3]>0.1),as.numeric(BANCA.G.g1.imp[,4]>0),as.numeric(BANCA.G.g1.imp[,5]>0.2))

out_fmr_3<-fmr_3p_rand_pval(ymat[,1],ymat[,2],ymat[,3:5],nreps=1000)


## The function is currently defined as
function(i,k,ymat,nreps=1000)
{
  if(length(i)!=length(k)|length(i)!=length(ymat[,1])) {print("lengths unequal");break}

  G=ncol(ymat)
  Fcalc=rep(0,nreps)
  numean=mean(ymat)
  nuhat=apply(ymat,2,mean)
  Fcalc[1]=sum((nuhat-numean)^2)
  print(nuhat);print(Fcalc[1])
  numb=nrow(ymat)
  ymattemp=matrix(0,nrow=numb,ncol=G)
  for(t1 in 2:nreps)
  {
    ymattemp=matrix(0,nrow=numb,ncol=G)

    for (t2 in 1:nrow(ymat))
    {
      ymattemp[t2,]=sample(ymat[t2,])
    }
    nuhattemp=apply(ymattemp,2,mean)
    Fcalc[t1]=sum((nuhattemp-numean)^2)
  }

  print("pval")
  print(mean(Fcalc >= Fcalc[1]))
  return(Fcalc)
}
}

