load('connectivity_cubes.rda')
# this is the data frames and arrays from the global environment
arrayList <- ls(envir = globalenv())[sapply(ls(envir = globalenv()), function(x) class(get(x))) == "array" ]

designList<- grep('des',ls(envir = globalenv())[sapply(ls(envir = globalenv()), function(x) class(get(x))) == "character" ],value = T)
