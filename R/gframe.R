setClass("gFramerJava",
         contains="gGrouprJava",
         prototype=prototype(new("gGrouprJava"))
         )

## add a frame for packing. subclass of gGroup
setMethod(".gframe",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text = "", markup=FALSE,
                   pos = 0, ## pos in [0,1] 0 for left, (.01,.99) center, 1 for right
                   container=NULL,
                   ...) {

            force(toolkit)

            gp = .ggroup(..., toolkit=toolkit)
            pane = getWidget(gp)

            ## we can't do any markup here. Font() could be used
            if(markup) {
              cat("HTML markup not supported for title. Try font(), it could work.\n")
              text = gsub("<[^>]*>","",text)    # strip off HTML
            }
            
            border = .jnew("javax/swing/BorderFactory")
            ## incorporate position
            titledBorder = border$createTitledBorder(text)
            if(pos == 1)
              titledBorder$setTitleJustification(titledBorder$RIGHT)
            else if(.01 < pos && pos < .99)
              titledBorder$setTitleJustification(titledBorder$CENTER)
            else
              titledBorder$setTitleJustification(titledBorder$LEFT)

            ## set the border
            .jcall(
                   .jcast(pane,"javax/swing/JComponent"),,
                   "setBorder",
                   .jcast(titledBorder,"javax/swing/border/Border"))
            


            ## make object
            obj = new("gFramerJava",
              block=gp@block, widget=gp@widget, toolkit=toolkit,ID=getNewID())

            ## add to container if desired
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj)
            }
            return(obj)
          })

### methods -- inherited from ggroup

setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gFramerJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            ## adds some breathing room to object
            ## value is pixels
            .svalue(obj@widget, toolkit=obj@toolkit,
                    index=index, drop=drop, ...) <- value

            return(obj)
          })
