\name{fmr_cov_paired}
\alias{fmr_cov_paired}
\title{fmr_cov_paired
}
\description{Supplementary function for covariance of FMR paired data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fmr_cov_paired(i1,k1,y1,y2)
}
\arguments{
  \item{i1}{id 1
}
 \item{k1}{ replications for group 1
}
\item{y1}{ binary for first group  indication of match
}
\item{y2}{ binary for second group indication of match
}
}

\value{ covariance
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{


## The function is currently defined as
fmr.cov.paired=function(i1,k1,y1,y2)
{
  nu1hat=mean(y1)
  nu2hat=mean(y2)
  x1=y1-nu1hat
  x2=y2-nu2hat
  covvar=sum(x1*x2)/length(i1)/length(i1)
  return(covvar)
}
}
