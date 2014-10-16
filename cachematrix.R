## I use a global variable to store all set of Matrix / Inverted matrix
## in the cache variable below
cache<-list()

## The function below create a CacheMatrix "object"
## The structure holds:
##    1- the matrix in the x variable.
##    2- the inverted matrix in inverse 
##    3- setter/getter functions for the matrix
##    4- setter/getter functions for the invert matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvert <- function(invert) m <<- invert
  getInvert <- function() m
  list(set = set, get = get,
       setInvert = setInvert,
       getInvert = getInvert)
}


## The function below takes cacheMatrix items and caclulates the invert matrix for the matrix it holds in x.
## If the calculation has already been performed, the function simply returns the inverted matrix.
## Otherwise it calculates the invert and stores it for future re-use.
cacheSolve <- function(x, ...) {
  m <- x$getInvert()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  message("Calling solve")
  m <- solve(data)
  x$setInvert(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

# This function iterates through the cache items to find if the matrix passed in exists.
# If the matrix is not present, the function creates a cache item for it and add it to the cache.
# Otherwise it returns it.
getCacheItem<-function(x=matrix()){
  m<-NULL
  for(i in cache){
    data<-i$get()
    if(identical(data,x)){
      message("Returned cached data")
      m<-i
      break;
    }
  }
  if(is.null(m)){
    message("Make a cache item")
    m<-makeCacheMatrix(x)
    index<-length(cache)+1
    cache<<-c(cache,list(m))
  }
  m
}

# This function is the entry to caclulates the invert matrix
InvertMatrix<-function(x=matrix()){
  i<-getCacheItem(x)
  cacheSolve(i)
}

#Test function
examples<-function(){
  ## Examples
  x<-matrix(c(1,2,3,4),nrow=2,ncol=2,byrow = TRUE)
  y<-matrix(c(1,2,3,4,3,2,1,0,0),nrow=3,ncol=3,byrow = TRUE)
  message("First call to invert x:")
  a<-InvertMatrix(x)
  message("Second call to invert x:")
  b<-InvertMatrix(x)
  print(a)
  print(identical(a,b))
  
  ###################################
  
  message("First call to invert y:")
  a<-InvertMatrix(y)
  message("Second call to invert y:")
  b<-InvertMatrix(y)
  print(a)
  print(identical(a,b))
  
}