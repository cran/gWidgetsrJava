##################################################
## add a separator to a container. Needs the container

setClass("gSeparatorrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

## should this return object?
setMethod(".gseparator",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   horizontal = TRUE, container = NULL, ...) {

            force(toolkit)

            j = .jnew("javax/swing/JSeparator")
            
            if(horizontal) {
              separator = .jnew("javax/swing/JSeparator",j$HORIZONTAL)
            } else {
              separator = .jnew("javax/swing/JSeparator",j$VERTICAL)
            }

            obj = new("gSeparatorrJava", block=separator, widget=separator, toolkit=toolkit, ID=getNewID())

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }

            invisible(obj)
            
          })


