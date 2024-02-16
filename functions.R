

findTernaryRegion<-function(x,x.names=names(x),name.center="cathemeral",n.regions=7){
  
names(x)<-x.names
  
#---3 equal regions (based on ternary corners)
if(n.regions==3){
  
  out<-NA
  out<- paste(names(x)[x==max(x)],collapse=".")#--composite categories only when coordinates exactly on a dividing line
}

  #---following Shepard's soil classification
  if(n.regions==10){
    
    out<-NA
    if(any(x>2/3))out<-names(x)[x>2/3]
    if(is.na(out) & any(x<1/6)){
      temp<-x[x>min(x)]
      temp<-names(temp)[order(temp,decreasing = TRUE)]
      out<-paste(temp,collapse="_")
    }
    if(is.na(out)) out <-name.center
  }
  
  
  if(n.regions==7){
    
    out<-NA
    if(any(x>2/3))out<-names(x)[x>2/3]
    if(is.na(out) & any(x<1/6)) out<-paste(names(x)[x>min(x)],collapse="_")
    if(is.na(out)) out <-name.center
  }
  

  #--- 4 regions based on merging of intermediate areas
  if(n.regions=="4B"){
    
    out<-NA
    if(any(x>2/3))out<-names(x)[x>2/3]
    
    if(is.na(out) & any(x<1/6)){
      out<- names(x)[x==max(x)][1] #paste(names(x)[x>min(x)],collapse="_")
    }  
    
    
    if(is.na(out)) out <-name.center
    
    
  }
  
  
  
  #---4 triangles
  if(n.regions==4){

    out<-NA
    if(any(x>1/2))out<-names(x)[x>1/2]
    #if(is.na(out) & any(x<1/6))out<-paste(names(x)[x>min(x)],collapse="_")
    if(is.na(out)) out <-name.center
  }
  
  return(out)
  
}






plotTernaryRegions<-function(n.categories=7,...){

  
  
background_triangle <- matrix(c(
  0, 0, 100,
  100, 0, 0,
  0, 100, 0
), ncol = 3, byrow = TRUE)

# Add polygon to plot

  TernaryPolygon(background_triangle, ...)

  if(n.categories==3){
    
    # Define a polygon
   
    part1 <- matrix(c(
      0, 0, 1,
      1/2, 0, 1/2,
      1/3, 1/3, 1/3,
      0,1/2,1/2
    ), ncol = 3, byrow = TRUE)
    
    part2 <- matrix(c(
      1,0,0,
      1/2, 1/2, 0,
      1/3, 1/3, 1/3,
      1/2, 0, 1/2
    ), ncol = 3, byrow = TRUE)
    
    part3 <- matrix(c(
      0, 1, 0,
      0, 1/2, 1/2,
      1/3, 1/3, 1/3,
      1/2, 1/2, 0
    ), ncol = 3, byrow = TRUE)
    

     TernaryPolygon(part1, ...)
    TernaryPolygon(part2, ...)
    TernaryPolygon(part3, ...)
    
  }
  
  
if(n.categories==4){
  # Define a polygon
  middle_triangle <- matrix(c(
    50, 0, 50,
    50, 50, 0,
    0, 50, 50
  ), ncol = 3, byrow = TRUE)
  # Add polygon to plot


  triangle1 <- matrix(c(
    0, 0, 100,
    50, 0, 50,
    0, 50, 50
  ), ncol = 3, byrow = TRUE)

  triangle2 <- matrix(c(
    50, 0, 50,
    100, 0, 0,
    50, 50, 0
  ), ncol = 3, byrow = TRUE)

  triangle3 <- matrix(c(
    0, 50, 50,
    50, 50, 0,
    0, 10, 0
  ), ncol = 3, byrow = TRUE)


  TernaryPolygon(triangle1, ...)
  TernaryPolygon(triangle2, ...)
  TernaryPolygon(triangle3, ...)

}


  if(n.categories=="4B"){

    # Define a polygon

    triangle1 <- matrix(c(
      0, 0, 1,
      1/3, 0, 2/3,
      0, 1/3, 2/3
    ), ncol = 3, byrow = TRUE)

    trapez1b <- matrix(c(
      1/6, 1/6, 2/3,
      1/6, 5/12, 5/12,
      0, 1/2, 1/2,
      0, 1/3, 2/3
    ), ncol = 3, byrow = TRUE)



    trapez2a <- matrix(c(
      1/6, 1/6, 2/3,
      1/3, 0, 2/3,
      1/2, 0, 1/2,
      5/12, 1/6, 5/12
    ), ncol = 3, byrow = TRUE)

    corner1 <- matrix(c(
      0, 0, 1,
      1/2, 0, 1/2,
      5/12, 1/6, 5/12,
      1/6, 1/6, 2/3,
      1/6, 5/12, 5/12,
      0, 1/2, 1/2,
      0, 1/3, 2/3
    ), ncol = 3, byrow = TRUE)


    triangle2 <- matrix(c(
      2/3, 0, 1/3,
      1, 0, 0,
      2/3, 1/3, 0
    ), ncol = 3, byrow = TRUE)

    trapez2b <- matrix(c(
      1/2, 0, 1/2,
      2/3, 0, 1/3,
      2/3, 1/6, 1/6,
      5/12, 1/6, 5/12

    ), ncol = 3, byrow = TRUE)

    trapez3a <- matrix(c(
      2/3, 1/6, 1/6,
      2/3, 1/3, 0,
      1/2, 1/2, 0,
      5/12, 5/12, 1/6
    ), ncol = 3, byrow = TRUE)

    corner2 <- matrix(c(
      1/2, 0, 1/2,
      1, 0, 0,
      1/2, 1/2, 0,
      5/12, 5/12, 1/6,
      2/3, 1/6, 1/6,
      5/12, 1/6, 5/12
    ), ncol = 3, byrow = TRUE)

    triangle3 <- matrix(c(
      0, 2/3, 1/3,
      1/3, 2/3, 0,
      0, 1, 0
    ), ncol = 3, byrow = TRUE)

    trapez1a <- matrix(c(
      1/6, 5/12, 5/12,
      1/6, 2/3, 1/6,
      0, 2/3, 1/3,
      0, 1/2, 1/2
    ), ncol = 3, byrow = TRUE)


    trapez3b <- matrix(c(
      5/12, 5/12, 1/6,
      1/2, 1/2, 0,
      1/3, 2/3, 0,
      1/6, 2/3, 1/6
    ), ncol = 3, byrow = TRUE)

    corner3 <- matrix(c(
      1/2, 1/2, 0,
      0, 1, 0,
      0, 1/2, 1/2,
      1/6, 5/12, 5/12,
      1/6, 2/3, 1/6,
      5/12, 5/12, 1/6
    ), ncol = 3, byrow = TRUE)


    middle_triangle <- matrix(c(
      1/6, 1/6, 2/3,
      2/3, 1/6, 1/6,
      1/6, 2/3, 1/6
    ), ncol = 3, byrow = TRUE)

 
    TernaryPolygon(corner1, ...)
    TernaryPolygon(corner2,...)
    TernaryPolygon(corner3,...)
 
  }



  if(n.categories==7){

    # Define a polygon

    triangle1 <- matrix(c(
      0, 0, 1,
      1/3, 0, 2/3,
      0, 1/3, 2/3
    ), ncol = 3, byrow = TRUE)

    triangle2 <- matrix(c(
      2/3, 0, 1/3,
      1, 0, 0,
      2/3, 1/3, 0
    ), ncol = 3, byrow = TRUE)

    triangle3 <- matrix(c(
      0, 2/3, 1/3,
      1/3, 2/3, 0,
      0, 1, 0
    ), ncol = 3, byrow = TRUE)

    trapez1 <- matrix(c(
      1/6, 1/6, 2/3,
      1/6, 2/3, 1/6,
      0, 2/3, 1/3,
      0, 1/3, 2/3
    ), ncol = 3, byrow = TRUE)

    trapez2 <- matrix(c(
      1/6, 1/6, 2/3,
      1/3, 0, 2/3,
      2/3, 0, 1/3,
      2/3, 1/6, 1/6
    ), ncol = 3, byrow = TRUE)

    trapez3 <- matrix(c(
      2/3, 1/6, 1/6,
      2/3, 1/3, 0,
      1/3, 2/3, 0,
      1/6, 2/3, 1/6
    ), ncol = 3, byrow = TRUE)


    middle_triangle <- matrix(c(
      1/6, 1/6, 2/3,
      2/3, 1/6, 1/6,
      1/6, 2/3, 1/6
    ), ncol = 3, byrow = TRUE)
    # Add polygon to plot


    TernaryPolygon(triangle1, ...)
    TernaryPolygon(triangle2,...)
    TernaryPolygon(triangle3,...)

    TernaryPolygon(trapez1,...)
    TernaryPolygon(trapez2,...)
    TernaryPolygon(trapez3,...)


  }

  if(n.categories==11){
    triangle1 <- matrix(c(
      0, 0, 1,
      1/3, 0, 2/3,
      0, 1/3, 2/3
    ), ncol = 3, byrow = TRUE)

    triangle2 <- matrix(c(
      2/3, 0, 1/3,
      1, 0, 0,
      2/3, 1/3, 0
    ), ncol = 3, byrow = TRUE)

    triangle3 <- matrix(c(
      0, 2/3, 1/3,
      1/3, 2/3, 0,
      0, 1, 0
    ), ncol = 3, byrow = TRUE)

    trapez1a <- matrix(c(
      1/6, 5/12, 5/12,
      1/6, 2/3, 1/6,
      0, 2/3, 1/3,
      0, 1/2, 1/2
    ), ncol = 3, byrow = TRUE)


    trapez1b <- matrix(c(
      1/6, 1/6, 2/3,
      1/6, 5/12, 5/12,
      0, 1/2, 1/2,
      0, 1/3, 2/3
    ), ncol = 3, byrow = TRUE)





    trapez2a <- matrix(c(
      1/6, 1/6, 2/3,
      1/3, 0, 2/3,
      1/2, 0, 1/2,
      5/12, 1/6, 5/12
    ), ncol = 3, byrow = TRUE)

    trapez2b <- matrix(c(
      1/2, 0, 1/2,
      2/3, 0, 1/3,
      2/3, 1/6, 1/6,
      5/12, 1/6, 5/12

    ), ncol = 3, byrow = TRUE)


    trapez3a <- matrix(c(
      2/3, 1/6, 1/6,
      2/3, 1/3, 0,
      1/2, 1/2, 0,
      5/12, 5/12, 1/6
    ), ncol = 3, byrow = TRUE)

    trapez3b <- matrix(c(
      5/12, 5/12, 1/6,
      1/2, 1/2, 0,
      1/3, 2/3, 0,
      1/6, 2/3, 1/6
    ), ncol = 3, byrow = TRUE)

    middle_triangle <- matrix(c(
      1/6, 1/6, 2/3,
      2/3, 1/6, 1/6,
      1/6, 2/3, 1/6
    ), ncol = 3, byrow = TRUE)


    TernaryPolygon(triangle1,...)
    TernaryPolygon(triangle2,...)
    TernaryPolygon(triangle3,...)

    TernaryPolygon(trapez1a,...)
    TernaryPolygon(trapez1b,...)
    TernaryPolygon(trapez2a,...)
    TernaryPolygon(trapez2b,...)
    TernaryPolygon(trapez3a,...)
    TernaryPolygon(trapez3b,...)




  }


}

most.common<-function(x,keep.equals=FALSE) {
  if(!keep.equals)  out<-names(table(x))[table(x)==max(table(x))]
  if(keep.equals)  out<-paste(names(table(x))[table(x)==max(table(x))],collapse="-")
  return(out)
}

ProcessCodaOutput <- function(x,DIC=F, params.omit=NULL, verbose=TRUE) {
  
  if(verbose){cat('Calculating statistics.......','\n')}  
  
  #Get parameter names
  params <- colnames(x[[1]])
  #Get number of chains
  m <- length(x)
  
  #Collapse mcmc.lists into matrix
  mat = do.call(rbind,x)
  colnames.sims <- colnames(mat)
  #Get # of iterations / chain
  n <- dim(mat)[1] / m
  
  #Get parameter dimensions
  dim <- get.dim(params)
  
  #Create new parameter name vectors to handle non-scalar params
  expand <- sapply(strsplit(params, "\\["), "[", 1)
  params.simple <- unique(sapply(strsplit(params, "\\["), "[", 1))
  
  #Functions for statistics
  qs <- function(x,y){as.numeric(quantile(x,y))}
  #Overlap 0 function
  ov <- function(x){findInterval(0,sort(c(qs(x,0.025),qs(x,0.975))))==1}
  #f function (proportion of posterior with same sign as mean)
  gf <- function(x){if(mean(x)>=0){mean(x>=0)}else{mean(x<0)}}
  #n.eff function
  calcneff <- function(x,n,m){
    xp <- matrix(x,nrow=n,ncol=m)
    xdot <- apply(xp,2,mean)
    s2 <- apply(xp,2,var)
    W <- mean(s2)
    
    #Non-degenerate case
    if ((W > 1.e-8) && (m > 1)) {
      B <- n*var(xdot)
      sig2hat <- ((n-1)*W + B)/n      
      n.eff <- round(m*n*min(sig2hat/B,1),0)
      #Degenerate case
    } else {
      n.eff <- 1
    }
    n.eff
  }
  
  #Gelman diag function
  gd <- function(i,hold){
    r <- try(gelman.diag(hold[,i], autoburnin=FALSE)$psrf[1], silent=TRUE)
    if(inherits(r, "try-error") || !is.finite(r)) {
      r <- NA
    }
    return(r)
  }
  
  #Make blank lists
  sims.list <- means <- rhat <- n.eff <- se <- as.list(rep(NA,length(params.simple)))
  q2.5 <- q25 <- q50 <- q75 <- q97.5 <- overlap0 <- f <- as.list(rep(NA,length(params.simple)))
  names(sims.list) <- names(means) <- names(rhat) <- names(n.eff) <- params.simple
  names(se) <- names(q2.5) <- names(q25) <- names(q50) <- names(q75) <- names(q97.5) <- params.simple
  names(overlap0) <- names(f) <- params.simple
  
  #This function modifies objects in global environment (output is discarded)
  #Calculates statistics for each parameter
  
  
  calc.stats <- function(i){
    
    #If parameter is not a scalar (e.g. vector/array)
    if(!is.na(dim[i][1])){
      
      #Get all samples
      sims.list[[i]] <<- mat[,expand==i]
      
      #If more than 1 chain, calculate rhat 
      #Done separately for each element of non-scalar parameter to avoid errors
      if(m > 1 && (!i%in%params.omit)){
        hold <- x[,expand==i]
        rhat.vals <- sapply(1:dim(hold[[1]])[2],gd,hold=hold)
        names(rhat.vals) <- colnames(hold[[1]])
        rhat[[i]] <<- populate(rhat.vals,dim[[i]])
      } else if (m == 1){
        hold <- x[,expand==i]
        rhat[[i]] <<- array(NA,dim=dim[[i]])
      }
      
      #Calculate other statistics
      ld <- length(dim(sims.list[[i]]))
      means[[i]] <<- populate(colMeans(sims.list[[i]]),dim[[i]])
      if(!i%in%params.omit){
        try({
          se[[i]] <<- populate(apply(sims.list[[i]],c(2:ld),sd),dim=dim[[i]])
          q2.5[[i]] <<- populate(apply(sims.list[[i]],c(2:ld),qs,0.025),dim=dim[[i]])
          q25[[i]] <<- populate(apply(sims.list[[i]],c(2:ld),qs,0.25),dim=dim[[i]])
          q50[[i]] <<- populate(apply(sims.list[[i]],c(2:ld),qs,0.5),dim=dim[[i]])
          q75[[i]] <<- populate(apply(sims.list[[i]],c(2:ld),qs,0.75),dim=dim[[i]])
          q97.5[[i]] <<- populate(apply(sims.list[[i]],c(2:ld),qs,0.975),dim=dim[[i]])
          overlap0[[i]] <<- populate(apply(sims.list[[i]],c(2:ld),ov),dim=dim[[i]])
          f[[i]] <<- populate(apply(sims.list[[i]],c(2:ld),gf),dim=dim[[i]])
          n.eff[[i]] <<- populate(apply(sims.list[[i]],c(2:ld),calcneff,n,m),dim=dim[[i]])  
        },silent=TRUE)
      }
      
      sims.list[[i]] <<- populate(sims.list[[i]],dim=dim[[i]],simslist=T,samples=dim(mat)[1])
      
      #If parameter is a scalar
    } else {
      
      if(m > 1 && (!i%in%params.omit)){rhat[[i]] <<- gelman.diag(x[,i],autoburnin=FALSE)$psrf[1]}
      
      sims.list[[i]] <<- mat[,i]
      
      means[[i]] <<- mean(sims.list[[i]])
      if(!i%in%params.omit){
        se[[i]] <<- sd(sims.list[[i]])
        q2.5[[i]] <<- qs(sims.list[[i]],0.025)
        q25[[i]] <<- qs(sims.list[[i]],0.25)
        q50[[i]] <<- qs(sims.list[[i]],0.5)
        q75[[i]] <<- qs(sims.list[[i]],0.75)
        q97.5[[i]] <<- qs(sims.list[[i]],0.975)
        overlap0[[i]] <<- ov(sims.list[[i]])
        f[[i]] <<- gf(sims.list[[i]])
        n.eff[[i]] <<- calcneff(sims.list[[i]],n,m)}
    }
    
  }
  
  #Actually run function(nullout not used for anything)
  nullout <- sapply(params.simple,calc.stats)
  
  #Warn user if at least one Rhat value was NA
  if(NA%in%unlist(rhat)&&verbose){
    options(warn=1)
    warning('At least one Rhat value could not be calculated.')
    options(warn=0,error=NULL)
  }
  
  #Do DIC/pD calculations if requested by user
  if(DIC){
    dev <- matrix(data=mat[,'deviance'],ncol=m,nrow=n)   
    pd <- numeric(m)
    dic <- numeric(m)    
    for (i in 1:m){
      pd[i] <- var(dev[,i])/2
      dic[i] <- mean(dev[,i]) + pd[i]
    }    
    pd <- mean(pd)
    dic <- mean(dic)
    
    #Return this list if DIC/pD requested
    if(verbose){cat('\nDone.','\n')}
    return(list(sims.list=sims.list,mean=means,sd=se,q2.5=q2.5,q25=q25,q50=q50,q75=q75,q97.5=q97.5,overlap0=overlap0,
                f=f,Rhat=rhat,n.eff=n.eff,pD=pd,DIC=dic,colnames.sims=colnames.sims))
  } else {
    #Otherwise return list without pD/DIC
    if(verbose){cat('\nDone.','\n')}
    return(list(sims.list=sims.list,mean=means,sd=se,q2.5=q2.5,q25=q25,q50=q50,q75=q75,q97.5=q97.5,overlap0=overlap0,
                f=f,Rhat=rhat,n.eff=n.eff,colnames.sims=colnames.sims))
  }
  
}


get.dim <- function(params){
  
  #Get all unique parameters (i.e., collapse indexed non-scalars)
  ps <- unique(sapply(strsplit(params, "\\["), "[", 1)) 
  #Slice indexes from non-scalar parameter entries
  test <- sapply(strsplit(params, "\\["), "[", 1)
  
  #Calculate dimension for each parameter i
  dim <- lapply(ps, function(i){
    
    #Extract indices from each element j of parameter i
    w <- params[test==i]
    getinds <- lapply(w,FUN=function(j){
      
      w2 <- strsplit(j,'\\[')[[1]][2]
      w3 <- strsplit(w2,"\\]")[[1]] 
      w4 <- as.numeric(unlist(strsplit(w3,",")))
      return(w4)
      
    })
    
    #Get max value from each dimension of i
    collapsedinds <- do.call(rbind,getinds)
    apply(collapsedinds,2,max)  
    
  })
  
  names(dim) = ps
  dim
  
}


populate <- function(input,dim,simslist=FALSE,samples=NULL){
  
  if(!simslist){
    
    charinds <- sub(".*\\[(.*)\\].*", "\\1", names(input), perl=TRUE) 
    
    fill <- array(NA,dim=dim)
    
    for (i in 1:length(input)){
      
      ind <- lapply(strsplit(charinds[i], ','), as.integer)[[1]]
      fill[matrix(ind,1)] <- input[i]
      
    }
  } else {
    
    charinds <- sub(".*\\[(.*)\\].*", "\\1", colnames(input), perl=TRUE) 
    
    fill <- array(NA,dim=c(samples,dim))
    
    for (i in 1:length(charinds)){
      
      #ind <- lapply(strsplit(charinds[i], ','), as.integer)[[1]]
      
      eval(parse(text=paste('fill[','1:',samples,',',charinds[i],']','<- input[,i]',sep="")))
      
    }
  }
  
  return(fill)
  
}


