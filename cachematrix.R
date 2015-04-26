

# makeCacheMatrix() function having 3 methods which is doing the set matrix, get matrix operations.
# p_x matrix valiable is used to retain the previous run matix value.
# if, current run matrix value x is equal to p_x then inverse should be read from cache,
# otherwise, it should be calculated again.

makeCacheMatrix <- function(x = matrix()) {
        
        
        if (!exists("m")){
                
                m <- NULL
                p_x <<- x
                #print(exists("m"))
                #print(x)
                #print(p_x)
                
        } else
        {
                #print(exists("m"))
                #print(x)
                #print(p_x)
                
                if(!identical(p_x,x)){
                        m <- NULL
                }
        }
        
        
        get <- function() {
                x
        }
        
        setInvMat <- function(invmat) {
                p_x <<- x    # saving the current value of matrix "x", so that it can be compared next time.  
                m <<- invmat
                
        }
        
        getInvMat <- function() {
                m
        }
        
        # Naming the all functions.... So that it can be called from other functions or
        # command prompt
        
        list(get = get, setInvMat = setInvMat, getInvMat = getInvMat)
        
}


# This is the calling function.
# Calling statement would be like :-> cacheSolve(makeCacheMatrix(x))

cacheSolve <- function(x, ...) {
        m <- x$getInvMat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        # Calculate the inverse of the matrix.
        
        m <- solve(data)
        
        # Set the inverse into the cache.
        x$setInvMat(m)
        
        m <<- m
        
        m
}


