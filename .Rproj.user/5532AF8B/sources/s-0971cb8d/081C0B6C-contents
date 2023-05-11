\name{fnmr_2p_rand_pval}
\alias{fnmr_2p_rand_pval}
\title{fnmr_2p_rand_pval
}
\description{2 sample paired randomized method
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_2p_rand_pval(data, nreps=1000)
}
\arguments{
  \item{data}{ set of decisions
}\item{nreps}{ number of repetitions
}

}

\value{Outputs a p value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
hold=cbind(faceG.gen[,1],as.numeric(faceC.gen[,3]<0.5860),as.numeric(faceG.gen[,3]<72.7287))

fnmr_2p_rand_pval(hold,nreps=1000)

## The function is currently defined as
function(data,nreps=1000){
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
}
