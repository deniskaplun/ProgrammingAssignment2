##Function checks if matrix has been cached. If so, it provides the inverse of the cached inverse. If not, the matrix gets cached and its inverse is provided.

makeCacheMatrix <- function(x = matrix()) {   #creates matrix that can be cached
        inv <- NULL
        set <- function(y) {    #anonymous function
                x <<- y
                inv <<- NULL	#creats a null matrix that is cached
        }
        get <- function() x	#calls cached matrix
        setsolve <- function() inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)   #list of functions 
}

cacheSolve <- function(x, ...) {  
        inv <- x$getsolve()
        if(!is.null(inv)) {      #checks if matrix has been used before
                message("getting cached inverse")
                return(inv)      #returns cached values of matrix 
        }			 #returns cached inverse value if exists
        data <- x$get()
        inv <- solve(data, ...)		#inverse of non-cached matrix
        x$setsolve(inv)
        inv
}
