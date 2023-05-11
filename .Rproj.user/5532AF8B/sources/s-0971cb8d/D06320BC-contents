\name{fmr_3i_bs_pval}
\alias{fmr_3i_bs_pval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fmr_3i_bs_pval
%%  ~~function to do ... ~~
}
\description{3 sample independent bootstrap
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fmr_3i_bs_pval(listy,nreps=1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{listy}{list of ids
%%     ~~Describe \code{x} here~~
}
 \item{nreps}{number of repetitions
%%     ~~Describe \code{x} here~~
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
## Note: Number of reps is lowered for time purposes
y1=cbind(BANCA.G.g1.imp[,1:2],as.numeric(BANCA.G.g1.imp[,26]>0))
y2=cbind(BANCA.G.g2.imp[,1:2],as.numeric(BANCA.G.g2.imp[,71]>0))
y3=cbind(lausanne.imp[,1:2],as.numeric(lausanne.imp[,9]> 2.0))
ylist=list(y1,y2,y3)

fout3i=fmr_3i_bs_pval(ylist,nreps=100)



## The function is currently defined as
function(listy,nreps=1000)
{
  F=rep(0,nreps)
  nuhat=rep(0,length(listy))
  numb=rep(0,length(listy))
  for (i in 1:length(listy))
  {
    nuhat[i]=mean(listy[[i]][,3])
    numb[i]=length(listy[[i]][,3])
  }

  nuall=sum(numb*nuhat)/sum(numb)
  F[1]=sum(numb*(nuhat-nuall)^2)
  nutemp=rep(0,length(listy))
  numbtemp=rep(0,length(listy))
  for (k in 2:nreps)
  {
    for( j in 1:length(listy))
    {
      indiv.i=names(table(listy[[j]][,1]))
      n.i=length(indiv.i)
      indiv.j=names(table(listy[[j]][,2]))
      n.j=length(indiv.j)
      out=vector(length=0)
      #print(n.i);print(n.j)
      list.i=sample(indiv.i,n.i,replace=T)
      for(t2 in 1:length(list.i))
      {
        list.j=sample(indiv.j,n.j,replace=T)
        for (t3 in 1:length(list.j))
        {
          #print(length(list.j));print(length(list.i));
          temp=listy[[j]][(listy[[j]][,1]==as.numeric(list.i[t2])&listy[[j]][,2]==as.numeric(list.j[t3])),3]
          #print(temp)
          if (length(temp)>0) out= c(out,temp)
        }
      }
      nutemp[j]=mean(out)
      numbtemp[j]=length(out)
      nualltemp=sum(numbtemp*nutemp)/sum(numbtemp)
    }
    #print(nutemp);print(numbtemp)
    F[k]=sum(numbtemp*(nutemp-nuhat)^2)


  }


  print("pvalue")
  print(mean(F >= F[1]))

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
