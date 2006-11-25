## class in aaaClasses.R
## constructor
setMethod(".ggroup",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   horizontal = TRUE, spacing = 5, container = NULL, ... 
                   ) {

            force(toolkit)
            
            theArgs = list(...)                   # raise.on.dragmotion
            
            if(is.null(spacing))
              spacing = 0


            if (horizontal) {
              group = .jnew("javax/swing/Box",as.integer(0))
              group$createHorizontalBox()
            } else {
              group = .jnew("javax/swing/Box",as.integer(1))
              group$createVerticalBox()
            }
            
            ## let breath a little
            ##group$SetBorderWidth(2)
            
            obj = new("gGrouprJava", block=group, widget=group, toolkit=toolkit, ID=getNewID(), horizontal=horizontal)

            
            ## attach to container if there
            if(!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=toolkit)
              add(container, obj)
            }

            ## raise if we drag across
            DEBUG("ggroup: need to fix raise on dragmotion\n")
            if(!is.null(theArgs$raise.on.dragmotion)) {
              adddroptarget(obj, handler = function(h,...) {})
              adddropmotion(obj, handler = function(h,...) getWidget(h$obj)$GetWindow()$Raise())
            }
            return(obj)
          })


##################################################
## methods



setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gGrouprJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            ## adds some breathing room to object
            ## value is pixels
            DEBUG("ggroup: implement svalue<-")

            return(obj)
          })

##################################################
## handlers
