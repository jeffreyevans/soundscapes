change.point <- function(X, sig = 0.05, nsim = 199, K = NULL, 
                         min.size = 30, alpha = 1){
  if(nsim < 0 && is.null(k))
  	stop("nsim must be a nonnegative integer.")
  if((sig <= 0 || sig >= 1) && is.null(k))
  	stop("sig must be a positive real number between 0 and 1.")
  if(min.size < 2)
  	stop("min.size must be an integer greater than 1.")
  if(alpha > 2 || alpha <= 0)
  	stop("alpha must be in (0,2].")
  n = nrow(X)
  energy = new.env(parent = emptyenv()) 
  setDim = function(r,c,env){
  	env$done_ = matrix(0, nrow=r, ncol=c)
  }
  setVal = function(i,j,val,env){
  	old = env$done_[i,j]
  	env$done_[i,j] = val
  	invisible(old)
  }
  e.split = function(changes,D,min.size,for.sim=FALSE, env=emptyenv()){ 	
   splits = sort(changes) 
   best = c(-1,-Inf)
   ii = jj = -1
     if(for.sim){ 
     	 for(i in 2:length(splits)){
     		  tmp=splitPoint(splits[i-1],splits[i]-1,D,min.size)
     		  if(tmp[2]>best[2]){ 
     		    ii=splits[i-1]; jj=splits[i]-1
     		    best=tmp 
     		  }
     		}
     		changes=c(changes,best[1]) 
     	  return(list('first'=ii,'second'=jj,'third'=changes,'fourth'=best[2]))
     	}
  	else{
  		for(i in 2:length(splits)){ 
  			if(env$done_[splits[i-1],1])
  				tmp = env$done_[splits[i-1],]
  			else{
  				tmp = splitPoint(splits[i-1],splits[i]-1,D,min.size)
  				setVal(splits[i-1],1,tmp[1],env)
  				setVal(splits[i-1],2,tmp[2],env)
  			}
  			if(tmp[2]>best[2]){ 
  				ii = splits[i-1]
  				jj = splits[i]-1
  				best = tmp
  			}
  			
  		}
  		changes = c(changes,best[1])
  		setVal(ii,1,0,env)
  		setVal(ii,2,0,env)
      return(list('first'=ii,'second'=jj,'third'=changes,'fourth'=best[2]))
  	}
  }  
  splitPoint = function(start,end,D,min.size){
  	if(end-start+1 < 2*min.size)
  		return(c(-1,-Inf))
  	D = D[start:end,start:end] 
  	return(splitPointC(start,end,D,min.size))
  }  
  sig.test = function(D,nsim,changes,min.size,obs,env=emptyenv()){
  	if(nsim == 0) 
  		return(0)
  	over = 0
  	for(f in 1:nsim){
  		D1=perm.cluster(D,changes) 
  		tmp=e.split(changes,D1,min.size,TRUE)
  		if(tmp[[4]] >= obs)
  			over = over+1
  	}
  		p.val = (1+over)/(f+1)
  	return(c(p.val,f))
  }
  perm.cluster = function(D, points){
    points = sort(points)
    K = length(points)-1 
      for(i in 1:K ){
      	u = sample(points[i]:(points[i+1]-1))
        D[points[i]:(points[i+1]-1),points[i]:(points[i+1]-1)] = D[u,u]
      }
    return(D)
  }						 
  return( ecp::e.divisive(X, sig.lvl = sig, min.size = min.size, R = nsim,  
	                      k = K, alpha = 1) )
}
