## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
	## set inverse matrix variable to null
	m <- NULL
	
	## resetting everything to null
    set <- function(y) 
      {
        x <<- y
        m <<- NULL
      }
	  
	## get matrix   
    get <- function() x

    ## set inverse matrix and original matrix for comparison later
	setinverse <- function(solve) 
      {
        m <<- solve
        oldmat <<- x
      }
    
    # get cached inverse matrix
	getinverse <- function() 
      {
        m
      }
    
	## get cached original matrix
    getold <- function() oldmat
	
	## special matrix object
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse, getold = getold) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
	## this function when inputted with special matrix object 
    ## will return inverse only if the input matrix is invertible
    
    ## get the actual matrix
    matdata <- matin$get()
    
    ## get previous/cached stored matrix here 
    oldmat <- matin$getold()
    
    ## get previously cached inverted matrix here 
    invd <- matin$getinverse()
    
    ## if there is no inverted matrix in cache or when matrix has changed
    if(!is.null(invd) && identical(oldmat,matdata))
    {
      ## compare if dimensions are same
      if ((nrow(oldmat) == nrow(matdata)) && 
          (ncol(oldmat) == ncol(matdata)))
      {
        message("getting cached data")
        return(invd)
      }
    }
    
    ## in case when no the cached inverted matrix is there calculate here
    matinv <- solve(matdata, ...)
    matin$setinverse(matinv)
    matinv
}
