\name{fnmr_2i_bs_pval}
\alias{fnmr_2i_bs_pval}
\title{fnmr_2i_bs_pval
}
\description{2 sample independent bootstrap method
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_2i_bs_pval(i1,y1,i2,y2,nreps=1000)
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
 \item{nreps}{
    number of repetitions
}
}

\value{Outputs difference in estimate, and p value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
fnmr_2i_bs_pval(BANCA.G.g1.gen[,1],as.numeric(BANCA.G.g1.gen[,34]<0.4),B.G.g2.gen[,1],as.numeric(B.G.g2.gen[,34]<0.4),nreps=5000)

## The function is currently defined as
function(i1,y1,i2,y2,nreps=1000){
  hold1=fnmr_1_bs_reps(i1,y1,nreps)
  hold2=fnmr_1_bs_reps(i2,y2,nreps)
  diff=hold1-hold2-(mean(y1)-mean(y2))
  diff.est=mean(y1)-mean(y2)
  diff[1]=diff.est



  print("diff.est")
  print(diff.est)

  print("p-value")

  print(mean(as.numeric( diff <= diff[1]) ) )

}
}

