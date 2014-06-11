
##  These functions allow creation of a special 'matrix' object
##    that can cache its own inverse and use that object to retrieve
##    the cached inverse of the matrix once it has been created

##  This function creates a special 'matrix' object that can
##    calculate and cache its own inverse

makeCacheMatrix <- function( x = matrix() ) {

    inv <- NULL

    ##  Create a function to cache the matrix
    set <- function( y ) {
        x <<- y
        inv <<- NULL
    }

    ##  Create a function to retrieve the matrix
    get <- function() x

    ##  Create a function to cache the matrix inverse
    setinverse <- function( i ) inv <<- i

    ##  Create a function to retrieve the matrix inverse
    getinverse <- function() inv

    ##  Return a list containing the four functions
    list( set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse )

}


##  This function returns the inverse of the 'matrix'
##    created by the makeCacheMatrix function.
##  If the inverse has already been calculated, then
##    it is retrieved from the cache, otherwise it is
##    computed and saved to the cache

cacheSolve <- function( x, ... ) {

    ##  Attempt to retrieve the inverse from cache
    inv <- x$getinverse()

    ##  If inverse has been cached, return it
    if( !is.null( inv ) ) {
        message( "Returning matrix inverse from cache ..." )
        return( inv )
    }

    ##  Otherwise compute the inverse ...
    data <- x$get()
    inv <- solve( data, ... )

    ##    ... and save it to the cache
    x$setinverse( inv )

    ##  Return the matrix inverse
    inv

}

