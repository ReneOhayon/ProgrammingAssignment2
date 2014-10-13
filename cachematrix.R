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


## The function below takes cacheMatrix items and caclulates the invert matrixes
## If the calculation has alreadz been performed, the function returns the invert
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

# This function iterate through the cache to find if the  matrix passed in.
# If the matrix is not present, the function creates the it and add it to the cache.
# Otherwise it returns it
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
