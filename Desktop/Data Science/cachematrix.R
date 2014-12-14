## The purpose is to write a pair of functions 
## that cache the inverse of a matrix.


## Create a list containing a function to set and get  
## both of a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(y){
            x<<-y
            inv<<-NULL
      }
      get<-function()x
      set_inv<-function(solve)inv<<-solve
      get_inv<-function()inv
      list(set=set,get=get,set_inv=set_inv,get_inv=get_inv) 

}

## Return a matrix that is the inverse of 'x
cacheSolve <- function(x, ...) { 
      inv<-x$get_inv() 
      if(!is.null(inv)){   ## Check first if it's been already calculated
            message("getting cached data")
            return(inv)   ## Return the value from the cache 
                          ## and skip the computation
      }
      data<-x$get() 
      inv<-solve(data,...)   ## Calculate the inverse of the matrix
      x$set_inv(inv)   ## Set the value in the cache via the set function
      inv         
        
}
