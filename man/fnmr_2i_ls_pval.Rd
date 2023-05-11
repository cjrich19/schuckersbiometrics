\name{fnmr_2i_ls_pval}
\alias{fnmr_2i_ls_pval}
\title{fnmr_2i_ls_pval
}
\description{2 sample independent large sample method hypothesis test
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_2i_ls_pval(i1,y1,i2,y2)
}
\arguments{
   \item{i1}{ id 1
}
\item{y1}{ binary for first sample  indication of match
}
\item{i2}{ id 1 sample 2
}
\item{y2}{ binary for second sample indication of match
}
}

\value{Ouputs a Zscore and a p value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
fnmr_2i_ls_pval(faceG.gen[,1],as.numeric(faceG.gen[,3]<72.5),lausanne.gen[,1],as.numeric(lausanne.gen[,3]<0.72))


## The function is currently defined as
function(i1,y1,i2,y2){
  level=0.95
  pi1hat=mean(y1)
  pi2hat=mean(y2)
  se1=fnmr_1_ls_ci(i1,y1,0.5,level)
  se2=fnmr_1_ls_ci(i2,y2,0.5,level)

  pooled=mean(c(y1,y2))
  print(pooled)
  se.p=sqrt(pooled*(1-pooled)*(se1^2/pi1hat/(1-pi1hat)+se2^2/pi2hat/(1-pi2hat)))

  zstat=(pi1hat-pi2hat)/se.p
  pval=pnorm(zstat)
  print("Z")
  print(zstat)
  print("p-value")
  print(pval)
  ##return(se.p)
}
}
