## Inverting is like implementing a reciprocal.  when you multiply a number
##by its reciprocal you get 1 and multiplying a Matrix by its Inverse you get 1
## doing this by hand takes several steps but R has a built in function called Solve
##This function can still take up computer resources so a shortcut to a solved Inverse
##is to save the result in a cached reserve and call it when opportune

## the makeCacheMatris function creates special objects in an environment
##that will cache the inverse of the an invertible matrix and hold it there
##to save time when calling to inverse the matrix you create. What it retrurns 
##is a list of functions (set,get,setInv,getInv).  These can be used in
#conjunction with a self described invertible matrix.  

makeCacheMatrix <- function(x = matrix()) {
        inv_M<-NULL #set initialize the inverse Matrix to empty
        #internal function that sets the x argument to special object called y
        set_M <-function(y){
                x<<-y
                inv_M<<-Null
        }
        #start your the rest of your list of 4 functions
        get_M<-function() x #get the calling argument x directly
        set_Inv<- function(inverse) inv_M<<-inverse
        get_Inv<-function() inv_M
        #make the list and apply variable to them
        list(set_M=set_M,get_M=get_M,set_Inv=set_Inv,get_Inv=get_Inv)
}


## If the Inverse has already been computed then get the result from the cached copy

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_M<-x$get_Inv()
        #conditionally matrix has not been cached then compute
        if(!is.null(inv_M)){ 
           message("retrieving the cached matrix")
                return(inv_M)
        }
        ##has not been cached solve(inverse matrix)
        the_Matrix <-x$get_M()
        inv_M<-solve(the_Matrix,...)
        x$set_Inv(inv_M)
        inv_M
        
}
##Test the functions


