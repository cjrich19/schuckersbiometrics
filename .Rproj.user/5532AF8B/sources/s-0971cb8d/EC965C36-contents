\name{fnmr_2i_ls_ci}
\alias{fnmr_2i_ls_ci}
\title{fnmr_2i_ls_ci
}
\description{2 sample independent large sample
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_2i_ls_ci(i1,y1,i2,y2,level=0.95)
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
 \item{level}{
  confidence level
}
}

\value{confidence interval
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
fnmr_2i_ls_ci(faceC.gen[,1],as.numeric(faceC.gen[,3]<0.58),lausanne.gen[,1],as.numeric(lausanne.gen[,5]<0.3),0.90)


## The function is currently defined as
function(i1,y1,i2,y2,level=0.95){
  pi1hat=mean(y1)
  pi2hat=mean(y2)
  se1=fnmr_1_ls_ci(i1,y1,0.5,level)
  se2=fnmr_1_ls_ci(i2,y2,0.5,level)

  se=sqrt(se1^2+se2^2)

  CI=c(pi1hat-pi2hat-qnorm(1-(1-level)/2)*se,pi1hat-pi2hat+qnorm(1-(1-level)/2)*se)
  print(CI)
  return(se)
}
}

