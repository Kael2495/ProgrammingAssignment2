## The first function store inside the cache memory the inverse matrix of x, then calling the second function you
## calculate the inverse matrix of your input, but if you have already calculated that matrix (and stored in the cache matrix)
## you will be able to get that value instead of calculating it again

## As it's already done for the vector creation, here I start calculating the inverse of the matrix
## x, and storing that value in the parameter inv inside the cache memory (inv <<- solve)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## As it's already done for the vector mean calculation, here I star checking if I have already calculated the 
## inverse matrix of x (from the previous function), if the value was stored inside the inv variable (in the cache memory)
## I took that value as the output of the cacheSolve function, otherwise I calculate the inverse matrix with the 
## solve function and give it as the output


cacheSolve <- function(x, ...) {
       inv <- x$getinv()
       if(!is.null(inv)){
         message("getting cached data")
         return(inv)
       }
       data <- x$get()
       inv <- solve(data, ...)
       x$setinv(inv)
       inv
}
