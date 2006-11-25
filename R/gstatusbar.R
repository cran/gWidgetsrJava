## gtkStatusBar. Use value to push message, value to pop
setClass("gStatusbarrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )
## constructor
setMethod(".gstatusbar",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="", container=NULL, ...) {

            group = ggroup(horizontal = TRUE)
            statusbar = glabel("text", container=group)

            force(toolkit)            
            
            obj = new("gStatusbarrJava",block=group, widget=statusbar, toolkit=toolkit, ID=getNewID())
            
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, expand=FALSE)
            }
  
            invisible(obj)
          })

### methods

## This gets from glabel instance
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gStatusbarrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            svalue(obj@widget)
          })

## This pushes to label
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gStatusbarrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   svalue(obj@widget) <- value
                   return(obj)
                 })
