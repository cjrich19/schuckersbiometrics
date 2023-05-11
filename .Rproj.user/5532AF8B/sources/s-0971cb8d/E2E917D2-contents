\name{fnmr_2p_ls_ci}
\alias{fnmr_2p_ls_ci}
\title{fnmr_2p_ls_ci
}
\description{2 sample paired large sample
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_2p_ls_ci(i1,y1,y2,level=0.95)
}
\arguments{
  \item{i1}{ id 1
}
\item{y1}{ binary for first group  indication of match
}
\item{y2}{ binary for second group indication of match
}
\item{level}{
  confidence level
}
}

\value{Outputs Covariance, diff between estimates, and a confidence interval
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
fnmr_2p_ls_ci(lausanne.gen[,1],as.numeric(lausanne.gen[,9]<2.0),as.numeric(lausanne.gen[,10]<1.0),level=0.90)

## The function is currently defined as
function(i1,y1,y2,level=0.95){
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
