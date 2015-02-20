## Function cacheSolve is used to show the inverse of a given matrix.
## It first tests if the matrix has already been inversed before.
## If the matrix has been inversed before, then it directly prints the cached value from the previous calculation.
## If the matrix has not been inversed before, it inverses it and prints the value.
## Function makeCacheMatrix basically serves as the Cache to store the values. 
## It also serves as a tool box for function cacheSolve to use.

## makeCacheMatrix is a function with one parameter x, and coerces x into an matrix if it is not already.
makeCacheMatrix <- function ( x = matrix ()) {
        I <- NULL
        ## I is NULL by default when no calculation has been done.
        set <- function (y) {
                x <<- y
                I <<- NULL
        }
        ## set() is meant to designate a new matrix for function makeCacheMatrix(), and reset I to NULL. 
        get <- function () x
        ## get() prints the matrix
        setinverse <- function (inverse) I <<- inverse
        ## setinverse() designates a value to I.
        getinverse <- function () I
        ##getinverse() prints I out.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## Function cacheSolve is used to show the inverse of a given matrix.
cacheSolve <- function (x){
        I <- x$getinverse()
        ## imports the value of I from cache.
        if(!is.null(I)){
                message("getting cached data")
                return (I)
        }
        ## if I is not NULL, that means calculation has been done before, and the function only need to import its value from cache.
        data <- x$get()
        ## if I is NULL, then no calculation has been done. The original matrix is imported.
        I <- solve (data)
        ## the original matrix is solved here.
        x$setinverse (I)
        I
        ## assign this new result to I and print it.
        
}