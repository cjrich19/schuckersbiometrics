\name{fnmr_1_bs_reps}
\alias{fnmr_1_bs_reps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fnmr_1_bs_reps
%%  ~~function to do ... ~~
}
\description{ A supplementary function to be used in fnmr bootstrapping functions
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_1_bs_reps(i, D, nreps = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{ individuals
%%     ~~Describe \code{i} here~~
}
  \item{D}{ decisions
%%     ~~Describe \code{D} here~~
}
  \item{nreps}{number of reps
%%     ~~Describe \code{nreps} here~~
}
}

\value{outputs bootstrapped replications
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\examples{

## The function is currently defined as
function (i, D, nreps = 1000)
{
    list.i = as.numeric(names(table(i)))
    sample(list.i, length(list.i), replace = T)
    pihat = sum(D)/length(D)
    pihat.b = rep(0, nreps)
    nlist = length(list.i)
    for (b in 1:nreps) {
        list.b = sample(list.i, length(list.i), replace = T)
        D.b = sample(D[i == list.b[1]], length(D[i == list.b[1]]),
            replace = T)
        for (bb in 2:nlist) {
            D.temp = sample(D[i == list.b[bb]], length(D[i ==
                list.b[bb]]), replace = T)
            D.b = c(D.b, D.temp)
        }
        pihat.b[b] = sum(D.b)/length(D.b)
    }
    e = pihat.b - pihat
    hist(e, nclass = 20)
    endpts = quantile(e, c(0.025, 0.975))
    return(e)
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
