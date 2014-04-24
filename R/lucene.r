.session <- new.env(parent=emptyenv())

LuceneObjects <- function(query){
res <- .jcall(.session$lo, "S", "getResults", query)
return(res)
}