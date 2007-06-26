setClass("gButtonrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )


setMethod(".gbutton",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {

            force(toolkit)
            
            theArgs = list(...)
            

            
            iconFile = gWidgetsrJavaIcons[[text]]
            if(!is.null(iconFile)) {
              ## put icon and text
              icon = .jnew("javax/swing/ImageIcon",iconFile)
              button = .jnew("javax/swing/JButton",
                .jnew("java/lang/String",text),.jcast(icon,"javax/swing/Icon"))
            } else {
              button = .jnew("javax/swing/JButton",.jnew("java/lang/String",text))
            }

            ## look like button if border=FALSE
            if(border == FALSE) {
              button$setBorderPainted(FALSE) # no border
              button$setContentAreaFilled(FALSE) # no shading
            }

            obj = new("gButtonrJava",
              block=button, widget=button, toolkit=toolkit,ID=getNewID())

            ## add to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }

            ## add handler
            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }
            
            invisible(obj)
          })
          
### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            .jcall(obj@widget,"S","getText")
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   button = obj@widget

                   iconFile = gWidgetsrJavaIcons[[value]]
                   if(!is.null(iconFile)) {
                     ## put icon and text
                     icon = .jnew("javax/swing/ImageIcon",iconFile)
                     .jcall(.jcast(button,"javax/swing/AbstractButton"),"V",
                            "setIcon",.jcast(icon,"javax/swing/Icon"))
                     .jcall(button,"V","setText",as.character(value))
                   } else {
                     .jcall(button,"V","setText",as.character(value))
                   }

                   return(obj)
                 })

### handlers
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ID = addJHandler(obj,handler, action,
              type="addActionListener",
              event = "ActionEvent",
                        class = "java/awt/event/ActionListener",
                        cast = "javax/swing/AbstractButton")
            return(ID)
          })
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerclicked(obj, handler, action)
          })

## for popup menu
setMethod(".addpopupmenu",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
          function(obj, toolkit, menulist, action=NULL, ...) {
            addPopupMenuWithSignal(obj, toolkit, menulist, signal="clicked",...)
})
