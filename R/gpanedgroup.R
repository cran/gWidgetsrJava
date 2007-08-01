setClass("gPanedgrouprJava",
         contains="gContainerrJava",
         prototype=prototype(new("gContainerrJava"))
         )

## TODO: method obj[1 or 2 ] <- replacewidget
setMethod(".gpanedgroup",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   widget1, widget2, horizontal=TRUE, container=NULL, ...) {
            ## add a paned group

            force(toolkit)
            
            sp = .jnew("javax/swing/JSplitPane")
            if(horizontal) {
              sp$setOrientation(.jfield(sp,name="HORIZONTAL_SPLIT"))
            } else {
              sp$setOrientation(.jfield(sp,name="VERTICAL_SPLIT"))
            }
            
            ## left or right *or* top or bottom
            leftgroup = ggroup(); add(leftgroup, widget1, expand=TRUE)
            rightgroup = ggroup(); add(rightgroup, widget2, expand=TRUE)

            .jcall(sp,,"setTopComponent", .jcast(leftgroup@widget@block,"java/awt/Component"))
            .jcall(sp,,"setRightComponent", .jcast(rightgroup@widget@block,"java/awt/Component"))

            ## balance these off
            theArgs = list(...)
            if(!is.null(theArgs$splitProportion)) 
              splitProportion = as.numeric(theArgs$splitProportion)
            else
              splitProportion = 0.5
            .jcall(sp,"V","setDividerLocation", splitProportion)

            ## make object
            obj = new("gPanedgrouprJava", block=sp, widget=sp,
              toolkit=toolkit,ID=getNewID())

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }

            ## make UI update
            .jcall(sp,,"updateUI")
            
            return(obj)
          })


## no methods, but *could* add obj[1]<- obj[2]<- for dynamically adding widget
