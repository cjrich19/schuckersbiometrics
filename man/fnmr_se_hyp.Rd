\name{fnmr_se_hyp}
\alias{fnmr_se_hyp}
\title{fnmr_se_hyp
}
\description{supplementary function for SE of FNMR data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_se_hyp(i, y, threshold = 0.5, pi_hyp=sum(y>threshold)/length(y), level = 0.95)
}
\arguments{
  \item{i}{ id face
%%     ~~Describe \code{x} here~~
} \item{y}{ decisions
%%     ~~Describe \code{x} here~~
} \item{threshold}{ threshold
}
\item{pi_hyp}{sum(y>threshold)/length(y)
%%     ~~Describe \code{x} here~~
} \item{level}{ confidence level
%%     ~~Describe \code{x} here~~
}
}

\examples{

## The function is currently defined as
function (i, y,threshold = 0.5, pi_hyp=sum(y>threshold)/length(y),level = 0.95)
  {
    N = length(i)
    order.i = order(i)
    i = i[order.i]
    y = y[order.i]
    #pihat = sum(y > threshold)/length(y)
    y2 = as.numeric(y > threshold) - pi_hyp
    pq = pi_hyp * (1 - pi_hyp)
    rhosum = 0
    rhocount = 0
    varcount = 0
    varsum = 0
    for (h in 1:N) {
      for (hh in h:N) {
        if (h == hh) {
          varcount = varcount + 1
          varsum = varsum + y2[h] * y2[hh]
        }
        if (i[h] == i[hh] & h != hh) {
          rhosum = rhosum + y2[h] * y2[hh]
          rhocount = rhocount + 1
        }
      }
    }
    corr.hat = rhosum/rhocount/pq
    total.var = (varsum + max(2 * rhosum, 0))/varcount/varcount
    #print("rho -hat")
    #print(corr.hat)
    Neff = pq/total.var
    #print("Neff")
    #print(Neff)
    se = sqrt(total.var)
    #print("CI")
    #cat(pihat, "+/-", se, "\n")
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
