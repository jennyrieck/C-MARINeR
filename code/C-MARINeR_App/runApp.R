runMariner = function(arrays,designs,...){
  
  .GlobalEnv$.marinerApp$arrays = arrays
  .GlobalEnv$.marinerApp$designs = designs
  on.exit(rm(.marinerApp, envir=.GlobalEnv))
  
  runApp('.',...)
}
