square_series <- function(length){
  # initialize the output vector
  output <- vector(mode="numeric", length=length)
  ub <- as.integer(sqrt(length))
  output[1] <- 1
  created   <- 1
  for (i in 2:ub){
     empty <- length - created
	if( empty > 0){
	   output[ (created+1) : (created + i^2)] <- i^2
       created <- created + i^2
    } else {
      # exit for loop
      break
    }
  }
  # The created cells may exceed the length
  # Restrict the output to the specified length
  return(output[1:length])
}

square_index <- function(length){

  output <- vector(mode="numeric", length=length)
  ub <- as.integer(sqrt(length))
  output[1] <- 1
  created   <- 1
  for (i in 2:ub){
     empty <- length - created
	if( empty > 0){
	   output[ (created+1) : (created + i^2)] <- 1:i
       created <- created + i^2
    } else {
      # exit for loop
      break
    }
  }
  # The created cells may exceed the length
  # Restrict the output to the specified length
  return(as.character(output[1:length]))
}

myLoop <- function(vObj){

  # Get te size of the input vector 
  vSize  <- length(vObj) 

  # initialize the output vector
  output <- vector(mode="numeric", length=vSize )

  # A boolean, used as a toggle
  b <- TRUE

  # i is the iteration variable
  for(i in 1 : vSize ){
    rmd <- vObj[i] %% 3 
    if( rmd==0 ){ 
      output[i] <- vObj[i]*max(vObj)^2 + (ifelse(b, 1,-1))* min(cumu(i), (vObj[i]*max(vObj)^2 - vObj[i]^3) )
       b <- !b
 	} else if(rmd==1)  {
	 output[i] <- 2*(vObj[i]*max(vObj)^2) - vObj[i]^3 
	} else {
      output[i] <- vObj[i]^3 	}
 }
 return(output)
}


cumu <- function(x){
  n <- as.integer(x)
  # initialize the output vector
  output <- vector(mode="numeric", length=length(n))
  for(i in 1 : length(n) ){
    if(i%%2){
      output[i] <- sum(1: n[i])
    } else {
      output[i] <- -1*sum(1: n[i])^2
    }
  }
  return(output)
}

fibonacci <- function(length){
  # initialize the output vector
  output <- vector(mode="numeric", length=length)
  output[1] <- 1
  output[2] <- 1
  for (i in 3:length){ 
    output[i] <- output[i-1]+output[i-2]
  }
  return(output)
} 

