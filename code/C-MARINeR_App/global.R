load('connectivity_cubes.rda')
# this is the data frames and arrays from the global environment



if(exists(".marinerApp")){
  arrayList = .marinerApp$arrays
  designList = .marinerApp$designs
} else{
  arrayList <- ls(envir = globalenv())[sapply(ls(envir = globalenv()), function(x) class(get(x))) == "array" ]
  designList<- grep('des',ls(envir = globalenv())[sapply(ls(envir = globalenv()), function(x) class(get(x))) == "character" ],value = T)
  
}
