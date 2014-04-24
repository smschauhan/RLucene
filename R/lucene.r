.session <- new.env(parent=emptyenv())

LuceneObjects <- function(query){
res <- .jcall(lo, "S", "getResults", query)
return(res)
}