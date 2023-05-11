\name{fcalc3}
\alias{fcalc3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fcalc3
%%  ~~function to do ... ~~
}
\description{F statistic calculating supplementary function
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fcalc3(Dlist, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Dlist}{list of decisions
%%     ~~Describe \code{x} here~~
}
\item{delta}{ delta
%%     ~~Describe \code{x} here~~
}
}

\value{F stat
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{


## The function is currently defined as
function(Dlist,delta=0){

  G=length(Dlist)
  #print("G");print(G);
  ###G is the number of sets of data
  n.g=rep(0,G)
  ###n.g is the number of data points in each set of data
  pihat.g=rep(0,G)
  ###pihat.g is the mean Decisions of each set of data

  Dprime=c(Dlist[[1]][,2])
  for(k in 2:G)
  {
    Dprime=c(Dprime,Dlist[[k]][,2])
  }
  ###Dprime is a list of all of the individuals
  n=length(Dprime)
  #pibar=mean(Dprime)
  #print(pibar)
  for(i in 1:G)
  {
    #print(Dlist[[i]][1:10,2])
    pihat.g[i]=mean(Dlist[[i]][,2])+1e-15
    n.g[i]=length(Dlist[[i]][,2])
  }
  pibar=sum(n.g*pihat.g)/sum(n.g)
  var.pihat.g=rep(0,G)
  ###var.pihat.g is the variance of each set of data
  for(i in 1:G){
    var.pihat.g[i]=var.pihat(cbind(Dlist[[i]][,1],Dlist[[i]][,2]-delta[i]))
  }

  ###pibar is the mean of all of the decisions
  #print(delta)
  #print(pihat.g)
  pib=pihat.g-delta
  #print(pib)
  var2=var.pihat.g/(pihat.g)/(1-pihat.g)*pib*(1-pib)
  #print(pib)
  F=(sum(n.g*(pib-pibar)^2/(G-1)))/sum((n.g^2*var2)/(n-G))
  if(F<0)F=1e-80
  if (F>10^90) F=10^90
  return(F)
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
