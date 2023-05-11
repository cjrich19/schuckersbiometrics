\name{fnmr_1p_bs_supplementary}
\alias{fnmr_1p_bs_supplementary}
\title{fnmr_1p_bs_supplementary
}
\description{Supplementary function for 3+ sample paired bootstraping
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fnmr_1p_bs_supplementary(i, ymat)
}
\arguments{
  \item{i}{ id
}
\item{ymat}{ indicator of matches
}
}

\examples{

## The function is currently defined as
function(i,ymat){
  #jk individual
  #boot decisions
  if(length(i)!=nrow(ymat)) {print("lengths unequal");break}
  #out2=rep(0,nreps)
  out=vector("list",1)
  data=cbind(1:length(i),i,ymat)
  G=ncol(data)
  indiv.i=names(table(i))
  n.i=length(indiv.i)
  list.i=rep(0,n.i)

  list.i=sample(indiv.i,replace=T)
  #print(list.i)
  #print(list.j)

  for(t2 in 1:length(list.i))
  {

    #temp=data[(i==as.numeric(list.i[t2])),2:3]
    #print(temp)

    temp=data[(i==as.numeric(list.i[t2])),3:G]
    n.temp=nrow(temp)
    #print(n.temp)
    #add1=temp[sample(1:n.temp,n.temp,replace=T),]
    #print("add")
    #print(add1)
    #temp=data[(i==as.numeric(list.j[t3])&j==as.numeric(list.i[t2])),]
    #n.temp=length(temp)
    #add2=temp[sample(1:n.temp,n.temp,replace=T)]

    out[[1]]= rbind(out[[1]],cbind(rep(t2,nrow(temp)),temp))
    #out[[1]]=rbind(out[[1]],temp)
  }


  return(out)
}



}
