## makeCacheMatrix caches a copy of a matrix based on the parameter x passed in from the function call
makeCacheMatrix <- function(x = matrix()) {
  flip<-NULL
  set<-function(y){
    x<<-y
    flip<<-NULL
  }
  get<-function() x
  setinverse<-function(setinverse) flip<<-setinverse
  getinverse<-function() flip
  list(getinverse=getinverse,setinverse=setinverse,get=get,set=set)
}


## cacheSolve checks to see if an existing matrix has been cached, if so it grabs the cached data and 
## reports back.  The R solve function is used to invert the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  flip<-x$getinverse()  #attempt to retrieve cached matrix
  if(!is.null(flip)) {  #if flip is NOT null than grab the cached matrix and return it
    message("getting cached data")
    return(flip)
}
  data <- x$get()
  flip <- solve(data, ...) #invert matrix
  x$setinverse(flip)
  flip
}

