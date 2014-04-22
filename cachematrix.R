## The functions set the value of inverse of the matrix in the cache

## The function makeCacheMatrix creates a special "matrix" object that
##can cache itself  
## set the matric (setmatrix)
## get the matric (getmatrix)
## set the inverse (setinverse)
## get the inverse (getinverse)
## return a list with four functions

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix() # inverse matrix
    
    setmatrix <-function(y){
        if (is.matrix(y) == TRUE) x<<-y
        else print("Input is not a matrix")
        m<<-matrix()
    }
    
    getmatrix<-function() x
    
    setmatrixinverse <-function(y){
        if(is.matrix(y) == TRUE) m <<- y
        else print("Inpput is not a matrix")
    }
    
    getmatrixinverse<- function() m 
    
    list (getmatrix = getmatrix, setmatrix = setmatrix, 
          getinverse = getmatrixinverse, setinverse = setmatrixinverse)
}


## The function cacheSolve looks for the inverse of the matrix in the cache. If
##the inverse exists in cache, it uses the calue, else it calculates the 
##inverse of the matrix and sets it in the cache.  

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    matrix_inverse<-x$getinverse()
    if (!identical(matrix_inverse, matrix())) {
        print("getting inverse from cache")
        return(matrix_inverse)
    }
    else{
        matrix_value = x$getmatrix()
        matrix_inverse = solve(matrix_value)
        x$setinverse(matrix_inverse)
    }
        
}
