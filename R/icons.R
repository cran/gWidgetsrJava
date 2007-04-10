## returns a list with key the icon name
## and value the filepath
## use as in .jnew("javax/swing/ImageIcons",icons[["ts"]])
getgWidgetsrJavaIcons = function() {
  path = system.file("images",package="gWidgetsrJava")
  allIcons = list.files(path)
  ## create a hash with name -> location
  iconPaths = list()
  for(i in allIcons) {
    filename = sub("\\.xpm$|\\.gif$|\\.jpg$|\\.jpeg$|\\.png$|\\.tiff$","",i)
    iconPaths[[filename]] <- system.file("images",i,package="gWidgetsrJava")
  }
  return(iconPaths)
}

## global hash containing icons
gWidgetsrJavaIcons = getgWidgetsrJavaIcons()



## find the stock icons. This includes those added bia loadGWidgetIcons()
setMethod(".getStockIcons",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit) {
            .stockicons = list()
            for(i in unlist(getgWidgetsrJavaIcons())) {
              name = sub("[a-zA-Z0-9]*-","",i)
              .stockicons[[name]] = i
            }
            return(.stockicons)
          })

## name can be a vector
## return NA, if not there
getstockiconname = function(name=NULL) {
  .stockicons = getStockIcons(toolkit=guiToolkit("rJava"))         # cache?

  if(is.null(name))
    return(unlist(.stockicons))
  

  tmpfun = function(names) {
    sapply(names, function(name) {
      ## already a stock name?
      if(name %in% .stockicons)
        return(name)
      
      if(name %in% names(.stockicons)) {
        return(.stockicons[[name]])
      } else {
        return(NA)
      }
    })
  }
  
  return(tmpfun(name))
}


#################################################
## functions to deal with icons
## class to icon translation -- return stock name
## with prefix

## find the stock icons. This includes those added bia loadGWidgetIcons()
setMethod(".stockIconFromClass",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,theClass, ...) {

            default = "symbol_star"
            
            if(is.null(theClass) ||
               is.na(theClass) ||
               length(theClass) == 0
               )
              return(NA)
            
            if(theClass %in% .models)
              return(getstockiconname("lines"))
            if(theClass %in% .ts)
              return(getstockiconname("ts"))
            if(theClass %in% .functions)
              return(getstockiconname("function"))
            
            ret = switch(theClass,
              "numeric"= "numeric",
              "integer"= "numeric",
              "logical" = "logical",
              "character"="select-font",
              "matrix" = "matrix",
              "data.frame" = "dataframe",
              "list" = "dataframe",
              "complex"="numeric",
              "factor"="factor",
              "recordedplot" = "plot",
              NA)
            
            return(getstockiconname(ret))
          })


setMethod(".stockIconFromObject",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,obj, ...) {
            .stockIconFromClass(class(obj)[1])
          })



##
## 
##loadGWidgetIcons()

