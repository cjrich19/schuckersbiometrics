\name{var.pihat}
\alias{var.pihat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{var.pihat
%%  ~~function to do ... ~~
}
\description{ supplementary function for variance
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
var.pihat(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ dataset
%%     ~~Describe \code{x} here~~
}
}

\value{ Supplementary function for Variance
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{

## The function is currently defined as

function (X) {

  if (sum(X[, 2]) == 0) {
    return(0)
  }
  if (sum(X[, 2]) == nrow(X)) {
    return(0)
  }
  Xtab = table(X[, 1], X[, 2])
  se = fnmr_1_ls_ci(X[, 1], X[, 2], threshold = 0.5)
  return(se^2)
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
