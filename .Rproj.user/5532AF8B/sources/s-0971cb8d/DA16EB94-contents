\name{fnmr_cov_paired}
\alias{fnmr_cov_paired}
\title{fnmr_cov_paired
}
\description{supplementary function for covariance of FNMR paired data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_cov_paired(i1, y1, y2)
}
\arguments{
  \item{i1}{
}
  \item{y1}{
}
  \item{y2}{
}
}


\examples{

## The function is currently defined as
function (i1, y1, y2)
{
    pi1hat = mean(y1)
    pi2hat = mean(y2)
    x1 = y1 - pi1hat
    x2 = y2 - pi2hat
    covvar = sum(x1 * x2)/length(i1)/length(i1)
    return(covvar)
  }
}
