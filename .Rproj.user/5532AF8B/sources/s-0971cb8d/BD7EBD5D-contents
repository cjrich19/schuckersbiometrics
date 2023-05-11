\name{fnmr_3p_rand_pval}
\alias{fnmr_3p_rand_pval}
\title{fnmr_3p_rand_pval
}
\description{3+ sample paired randomized method
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_3p_rand_pval(data,nreps=1000)
}
\arguments{
  \item{data}{observations
}
 \item{nreps}{ number of repetitions
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
h=cbind(as.numeric(B.G.g2.gen[,36]< -0.8),as.numeric(B.G.g2.gen[,37]< -0.5),as.numeric(B.G.g2.gen[,38]< -0.2),as.numeric(B.G.g2.gen[,39]< -0.2),
as.numeric(B.G.g2.gen[,40]< 0),as.numeric(B.G.g2.gen[,41] <0), as.numeric(B.G.g2.gen[,42]<0),as.numeric(B.G.g2.gen[,43]<0),as.numeric(B.G.g2.gen[,44]<0),as.numeric(B.G.g2.gen[,45]< 0),as.numeric(B.G.g2.gen[,46] <0))

fnmr_3p_rand_pval(cbind(B.G.g2.gen[,1],h),nreps=1000)

## The function is currently defined as
function(data,nreps=1000){
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
}

