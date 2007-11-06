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
            leftgroup = ggroup()
            rightgroup = ggroup() 

            
            if(!is.missing(widget1)) {
              add(leftgroup, widget1, expand=TRUE)
              tag(obj,"ctr") <- 1
            }

            if(!is.missing(widget2)) {
              add(rightgroup, widget2, expand=TRUE)
              tag(obj,"ctr") <- 2
            }
            
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
              toolkit=toolkit,ID=getNewID(),  e = new.env())

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }

            ## make UI update
            .jcall(sp,,"updateUI")
            
            return(obj)
          })



## add -- use this rather than at construction time
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gPanedgrouprJava", value="gWidgetrJava"),
          function(obj, toolkit, value, ...) {
            ctr = tag(obj,"ctr")
            if(is.null(ctr))
              ctr = 0

            if(ctr == 0) {
              add(tag(obj,"leftgroup"), value, expand=TRUE)
              ctr = 1
            } else if(ctr ==1) {
              add(tag(obj,"rightgroup"), value, expand=TRUE)
              ctr = 2
            } else {
              cat("Can only add two widgets to a gpanedgroup\n")
            }
            tag(obj,"ctr") <- ctr
            
          })

## svalue show get/set sash position
### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gPanedgrouprJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            cat("Implement me in rJava\n")
          })

## svalue sets position
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gPanedgrouprJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(0 <= value && value <= 1) {
                     cat("Implement me in rJava\n")
                   }
                   return(obj)
                 })
