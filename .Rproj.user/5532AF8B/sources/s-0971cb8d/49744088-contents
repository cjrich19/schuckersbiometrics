\name{fnmr_2p_ls_pval}
\alias{fnmr_2p_ls_pval}
\title{fnmr_2p_ls_pval
}
\description{2 sample paired large sample method
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_2p_ls_pval(i1,y1,y2)
}
\arguments{
  \item{i1}{ id 1
}
\item{y1}{ binary for first group  indication of match
}
\item{y2}{ binary for second group indication of match
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
fnmr_2p_ls_pval(lausanne.gen[,1],as.numeric(lausanne.gen[,7]< -0.4),as.numeric(lausanne.gen[,5]< 0.4))

## The function is currently defined as
function(i1,y1,y2){
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
}

