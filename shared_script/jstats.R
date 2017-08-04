LLRD <- function( n.., ni., n.j, nij ) 
  {
  a <- nij
  b <- ni. - nij
  c <- n.j - nij
  d <- n.. - ni. - n.j + nij
  aterm <- suppressWarnings( a*( log(a) - log( a + b) ) )
  cterm <- suppressWarnings( c*( log(c) - log(c + d) ) )
  acterm <- suppressWarnings( ( a + c ) * ( log(a + c) - log ( a + b + c + d)) )
  llr <- aterm + cterm - acterm
  llr[is.nan(llr)] <- 0
  return( llr )
  }

prre_ci <- function( n.., ni., n.j, nij ) {
  prr <- prre( n.., ni., n.j, nij )
  sd <- sqrt( 1/nij- 1/n.j + 1/(ni.-nij) - 1/(n..-n.j) )
  sd[is.infinite(sd)] <- NA
  lb <- exp ( log( prr ) - 1.96*sd )
  ub <- exp ( log( prr ) + 1.96*sd )
  lb[is.infinite(lb)] <- NA
  ub[is.infinite(ub)] <- NA
  return( list(lb=lb, prr=prr, sd=sd, ub = ub) )
  }

prre <- function( n.., ni., n.j, nij ) {
  a <- nij
  b <- ni. - nij
  c <- n.j - nij
  d <- n.. - ni. - n.j + nij
  num <- a/(a + c)
  denom <- b/(b+d)
  val <- num/denom
  val[ which(is.infinite(val) ) ] <- 9999
  return( val )
}

ror <- function( n.., ni., n.j, nij ) {
  num <- nij/(n.j-nij)
  denom <- (ni.)/(n.. - ni.)
  val <- num/denom
  val[ which(is.infinite(val) ) ] <- 9999
  return( val )
}

rr <- function( n.., ni., n.j, nij ) {
  a <- nij
  b <- ni. - nij
  c <- n.j - nij
  d <- n.. - ni. - n.j + nij
  num <- a/(a + b)
  denom <- c/(c+d)
  val <- num/denom
  val[ which(is.infinite(val) ) ] <- 9999
  return( val )
}

logLRnum<-function(x,y, z, n)
{
  logLR<-x*(log(x)-log(y))+ (z-x)*(log(z-x)-log(n-y))
  return(logLR)
}

getCritVal2 <- function(R, n_j, n_i, n, Pvector, prob)
{
  
  I <- length(Pvector)
  Simulatej<-rmultinom(R,size=n_j,prob=Pvector)  
  myLLRs<-matrix(0,I,R)   
  for (i in seq_along(Pvector)){
    for (j in 1:R)
    {
      
       myLLRs[i,j]=logLRnum( Simulatej[i,j] ,n_i[i] , n_j, n )
    }
  }
  myLLRs <- myLLRs - n_j*log(n_j)+n_j*log(n)
  myLLRs[is.nan(myLLRs)] <- 0
  myLLRs[is.na(myLLRs)] <- 0
  mymax <- apply(myLLRs, 2, max)
  critval <- quantile(mymax,  probs = prob)
  critval01 <- quantile(mymax,  probs = .99)       #get cut off value
  return( list(critval=critval, critval01=critval01, mymax=mymax) ) 
}