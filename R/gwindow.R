## constructor
setMethod(".gwindow",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   title="Window", visible=TRUE,
                   width = NULL, height = NULL,
                   handler=NULL, action = NULL,
                   ...
                   ) {

            force(toolkit)
            
            window = .jnew("javax/swing/JFrame",title)
            UIM = .jnew("javax/swing/UIManager")
            UIM$setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
            

            ## set Preferred size.
            ## use size<- to set minimum size
            if(!is.null(width)) {
              if(is.null(height)) height = .7 * width
            } else {
              width <- height <- 200
            }
            
            d = .jnew("java/awt/Dimension", as.integer(width), as.integer(height))
            .jcall(.jcast(window,"javax/swing/JComponent"),"V","setPreferredSize",d)

            ## should be JFrame.EXIT_ON_CLOSE
            ## window$setDefaultLookAndFeelDecorated(TRUE)
            window$setDefaultCloseOperation(as.integer(1))
            
            
            
            obj = new("gWindowrJava",block=window, widget=window, toolkit=toolkit, ID=getNewID())

            svalue(obj) <- title
            
            if (!is.null(handler)) {
              id <- addhandlerdestroy(obj, handler=handler, action=action)
            }

            if(visible)
              visible(obj) <- visible

            return(obj)
          })
##################################################
## Methods

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava", value="gWidgetrJava"),
          function(obj, toolkit, value, ...) {
            .jcall(obj@widget,"Ljava/awt/Component;", "add",
                   .jcast(getBlock(value), "java/awt/Component"))
            .jcall(obj@widget,"V", "pack")
#            .jcall(obj@widget,"V", "invalidate")
#            .jcall(obj@widget,"V", "validate")
          })


## methods

## svalue refers to title
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            ## return title
            .jcall(obj@widget, , "getTitle")
          })

setMethod(".svalue<-",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, index=NULL,..., value) {
            ## set the title
            .jcall(obj@widget, , "setTitle",as.character(value))
            return(obj)
          })

## no visible() method
setMethod(".visible<-",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, ..., value) {
            value = as.logical(value)

            .jcall(obj@widget,,"setVisible", value)

            return(obj)
          })


setMethod(".size",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, ...) {
            missingMsg(".size,gwindow")
            return()
          })

setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, ...) {
            .jcall(obj@widget,,"dispose")
          })


##################################################
## handlers
setMethod(".addhandlerdestroy",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="destroy", handler, action, ...)
          })
