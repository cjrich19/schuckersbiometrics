\name{fnmr_1_ls_pval}
\alias{fnmr_1_ls_pval}
\title{fnmr_1_ls_pval
}
\description{1 sample large sample
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_1_ls_pval(i,y,threshold,pi0)
}
\arguments{
  \item{i}{ data with individuals for sampling
}
\item{y}{replicates where we assume null is true
}
\item{threshold}{threshold of Gaussian distribution used for matching
}
\item{pi0}{the bound used for testing
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
fnmr_1_ls_pval(lausanne.gen[,1],as.numeric(lausanne.gen[,8]<3.64432),0.5,0.1)


## The function is currently defined as
function(i,y,threshold,pi0){
  N=length(i)
  order.i=order(i)
  i=i[order.i]
  y=y[order.i]

  pihat=sum(y>threshold)/length(y)
  y2=as.numeric(y>threshold)-pihat
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

  print("pvalue")
  pval=pnorm((pihat-pi0)/se)
  cat(pval,"\n")

  ##return(se)

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
