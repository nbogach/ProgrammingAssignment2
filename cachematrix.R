## ProgrammingAssignment2
## functions do

## Function makeCacheMatrix is similar to makeCacheVector shown in example

makeCacheMatrix <- function(x = matrix()) {
            
            ## Make stored Soved Matrix NULL 
            solved_m <- NULL 
            
            ## Store Matrix passed to function as parameter
            set <- function(y) {
                  x <<- y
                  sovled_m <<- NULL
            }
            
            ## Get stored Matrix
            get <- function() x
            
            ## Store calculated solved Matrix in "cache"
            setsolved_m <- function(solved) solved_m <<- solved
            
            ## Get solved Matrix from "cache"
            getsolved_m <- function() solved_m
            
            ## Return functions described above to object assigned by makeCacheMatrix
            list(set = set, get = get,
                 setsolved_m = setsolved_m,
                 getsolved_m = getsolved_m)
}


## This function returns solved matrix from cache if any. Otherwise calculate solved matrix.

cacheSolve <- function(x, ...) {

      ## Get solved Matrix from cache
      solved_m <- x$getsolved_m()
      
      ## Check if data exists, i.e. is not NULL
      if(!is.null(solved_m)) {
            message("getting cached data")
            return(solved_m)
      }
      
      ## No solved Matrix in cache. Start calculation...
      data <- x$get()
      solved_m <- solve(data, ...)
      x$setsolved_m(solved_m)
      solved_m

}

