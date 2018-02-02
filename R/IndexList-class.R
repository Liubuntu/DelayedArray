## IndexList object

setRefClass(
    "IndexList",
    fields = c(index = "list")
)

## constructor
IndexList <- function(index)
{
    new("IndexList", index=index)
}

setClassUnion("list_OR_IndexList", c("list", "IndexList"))

## Methods
setGeneric(".index", function(x) standardGeneric(".index"))
setMethod(".index", "list", function(x) x)
setMethod(".index", "IndexList", function(x) x$index)

setGeneric(".clone", function(x) standardGeneric(".clone"))
setMethod(".clone", "ANY", identity)
setMethod(".clone", "IndexList", function(x) x$copy())

setGeneric(
    ".index<-",
    function(x, ..., value) standardGeneric(".index<-"),
    signature = "x"
)

setReplaceMethod(".index", "ANY", function(x, ..., value) {
    as(value, class(x))
})

setReplaceMethod(".index", "DelayedArray", function(x, ..., value) {
    x@index <- value
    x
})

setReplaceMethod(".index", "IndexList", function(x, ..., value) {
    x <- .clone(x)
    x$index <- value
    x
})

setGeneric(
    ".clone_assign_index",
    function(x, index) standardGeneric(".clone_assign_index"),
    signature = "x"
)

setMethod(".clone_assign_index", "ANY", function(x, index) {
    index
})

setMethod(".clone_assign_index", "IndexList", function(x, index) {
    x <- .clone(x)
    x$index <- index
    x
})
