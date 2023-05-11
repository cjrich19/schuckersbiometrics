\name{fnmr_se}
\alias{fnmr_se}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fnmr_se
%%  ~~function to do ... ~~
}
\description{Supplementary Function for SE
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_se(i, y, threshold = 0.5, level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{ individual
%%     ~~Describe \code{i} here~~
}
  \item{y}{ decisions
%%     ~~Describe \code{y} here~~
}
  \item{threshold}{ threshold
%%     ~~Describe \code{threshold} here~~
}
  \item{level}{ confidence level
%%     ~~Describe \code{level} here~~
}
}

\value{ standard error
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{

## The function is currently defined as
function (i, y, threshold = 0.5, level = 0.95)
{
    N = length(i)
    order.i = order(i)
    i = i[order.i]
    y = y[order.i]
    pihat = sum(y > threshold)/length(y)
    y2 = as.numeric(y > threshold) - pihat
    pq = pihat * (1 - pihat)
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
    Neff = pq/total.var
    se = sqrt(total.var)
    CI = c(pihat - qnorm(1 - (1 - level)/2) * se, pihat + qnorm(1 -
        (1 - level)/2) * se)
    CI.binom = c(pihat - 2 * sqrt(pq/varcount), pihat + 2 * sqrt(pq/varcount))
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
