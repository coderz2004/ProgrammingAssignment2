##consists of 2 functions, one to create a matrix that is ready to be cached, another to check if the mean for a given matrix is already cached so it can be returned, or calculated depending on whether or not it is present in the cache.

##creates a matrix that is cached
makeCacheMatrix<-function(x=matrix())
{
  inv<- NULL
  set<<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##checks for previusly cached mean, and computes it if not found
cacheSolve<-function(x, ...)
{
  inv<-x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
