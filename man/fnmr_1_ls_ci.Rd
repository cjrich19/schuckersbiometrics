\name{fnmr_1_ls_ci}
\alias{fnmr_1_ls_ci}
\title{fnmr_1_ls_ci
}
\description{1 sample large sample method
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_1_ls_ci(i,y,threshold,level=0.95)
}
\arguments{
  \item{i}{ data with individuals for sampling
}
\item{y}{replicates where we assume null is true
}
\item{threshold}{threshold of Gaussian distribution used for matching
}
\item{level}{confidence level
}

}

\value{Outputs confidence interval bounds
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
fnmr_1_ls_ci(BANCA.G.all.gen[,1],as.numeric(BANCA.G.all.gen[,31]<0.35),threshold=0.5,level=0.99)

## The function is currently defined as
function(i,y,threshold,level=0.95){
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

  corr.hat=rhosum/rhocount/pq
  total.var= (varsum+max(2*rhosum,0))/varcount/varcount

  Neff=pq/total.var

  se=sqrt(total.var)
  CI=c(pihat-qnorm(1-(1-level)/2)*se,pihat+qnorm(1-(1-level)/2)*se)
  CI.binom=c(pihat-2*sqrt(pq/varcount),pihat+2*sqrt(pq/varcount))
  print("CI")

  return(CI)

  return(se)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
