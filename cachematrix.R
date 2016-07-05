# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


# matrix()
# matrix creates a matrix from the given set of values.
# as.matrix attempts to turn its argument into a matrix.
# is.matrix tests if its argument is a (strict) matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment
                # different from the current environment.
                x <<- y
                inv <<- NULL
        }
        #Dynamic lookup
        #Lexical scoping determines where to look for values, not when to look for them. R looks for values when the function is run, not when it’s created. This means that the output of a function can be different depending on objects outside its environment:
        #Ref: Advanced R by Hadley Wickham
        #http://adv-r.had.co.nz/Functions.html
        get = function() x #Looks dynamically for the matrix "x"
        setinv = function(inverse) inv <<- inverse #R will look for "inv" dynamically in "cacheSolve" function
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Test if the inversion of the matrix is cached
## If cached, display a message "Getting cached data..."
## If not cached, calculate the inverse

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()

        inv = x$getinv()

        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation.
                message("Getting cached data...")
                return(inv)
        }

        # otherwise, calculates the inverse
        matrix.data = x$get()
        inv = solve(matrix.data, ...)

        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)

        return(inv)
}
