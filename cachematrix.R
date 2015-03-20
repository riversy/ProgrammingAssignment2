##
#   Programming Assignment 2: Lexical Scoping
#   for course "R Programming" on Coursera
#   by Igor Goltsov <riversy@gmail.com>
##

makeCacheMatrix <- function(x = matrix()) {
## This functione prepares 
## Cache instance for some matrix "x"   
## Methods:
#      get()            retrieves the source of the matrix
#      set(y)           store source matrix and nulled calculation results in the Global Scope 
#      setsolve(solve)  stores calculations in the global scope 
#      getsolve()       retrieves previously calculated value from Global Scope

    m <- NULL # Flush local variable "m" if there were some calculations before 

    # This method store source matrix for future uses 
    # and reset "m" variable in Global environment
    set <- function(y) {
        x <<- y 
        m <<- NULL
    }

    # This function will retrieve us matrix source
    get <- function() x

    setsolve <- function(solve) m <<- solve

    # This function will retrieve us cached calculations
    getsolve <- function() m

    # Prepare the Cache Instance to be 
    # used in "cacheSolve" method
    # and retrieves that
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
## This method check if calculations were apllied before 
## If matrix was inverted before, it just retireves 
## previously calculated result.
## If this is a first call, it will inverse the matrix  
## what was stored in the Cache Instance and cache result there 

    # Trying to get value from cache instance
    value <- x$getsolve()
    
    # If value isn't null we may just return that
    if (!is.null(value)){

        # Drop a message to console about what going on
        print("Getting cached value.")
        return(value)
    }
    
    # Here we need to proceed calculations
    value <- solve(x$get(), ...)

    # Drop a message about what going on
    print("Save solved value")

    # and save calculated value in the Cache Instance 
    x$setsolve(value)

    # Retrieves calculated value
    value
}
