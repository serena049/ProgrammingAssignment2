## Put comments here that give an overall description of what your functions do
## The makeCacheMatrix function is a list containing
## four subfunctions; the purpose of the function is to store the cached data (including the original matrix and the inverse matrix)
## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function() i
        list(set=set,get=get,setinverse = setinverse, getinverse =getinverse)

}


## Write a short comment describing this function
##The cacheSolve function is used to estimate the inverse of an input matrix, if the inverse has been estimated previously
##the function will link to the cache data directly; if not, a new one will be estimated 
cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
        message("getting cached data")
        return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}
        