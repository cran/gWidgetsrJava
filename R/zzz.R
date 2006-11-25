.onLoad <- function(libname,pkgname,...) {
  require(methods)
  require(rJava)

  ## we supply our own JavaGD class
  Sys.putenv("JAVAGD_CLASS_NAME"="org/rosuda/JGR/toolkit/JavaGD")

  

  
  .jinit(c(system.file(paste("java","gWidgetsrJava.jar",
                             sep=.Platform$file.sep),
                       package="gWidgetsrJava"),
           system.file(paste("jri","JRI.jar",
                             sep=.Platform$file.sep),
                                   package="rJava"))
         )
}
         

       

.onAttach <- function(...) {
   #  loadGWidgetIcons()
}
