## build widget based on gcheckbox
setClass("gCheckboxgrouprJava",
         representation = representation("gComponentrJava",
           coercewith="NULLorFunction"),
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

setMethod(".gcheckboxgroup",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   items, checked = FALSE,
                   horizontal=FALSE, 
                   handler = NULL, action = NULL, container = NULL, ...) {

            force(toolkit)
            
            if(missing(items) || length(items) == 0)
              stop("Need items to be a vector of items")
            
            checked = rep(checked, length(items))

            group = ggroup(horizontal = horizontal)
            
            lst = list()
            n = length(items)
            for(i in 1:n) {
              newItem = gcheckbox(items[i], checked=checked[i],
                handler=handler, action=action)
              lst[[ as.character(items[i]) ]] = newItem
              add(group, newItem)
            }
  

            theArgs = list(...)
            if(!is.null(theArgs$coerce.with)) {
              coerce.with = theArgs$coerce.with
            } else {
              if(is.numeric(items))
                coerce.with = as.numeric
              else
                coerce.with = as.character
            }
            if(is.character(coerce.with))
              coerce.with = get(coerce.with)

            
            ## make combination widget with all the values
            obj = new("gCheckboxgrouprJava",block=group, widget=group,
              toolkit=toolkit,  e = new.env(), coercewith = coerce.with)
  
            tag(obj, "items") <- items
            tag(obj, "itemlist") <- lst
            
            if(!is.null(handler))
              id = addhandlerchanged(obj,handler,action)

            ## do we add to the container?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=obj@toolkit)
              add(container,  obj, ...)
            }
            
            return(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gCheckboxgrouprJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            theArgs = list(...)
            
            lst = tag(obj, "itemlist")
            vals = sapply(lst, svalue)         # logicals

            if(!is.null(index) && index == TRUE) {
              return(which(vals))       # return indices
            } else {
              vals = tag(obj,"items")[vals]
              coerce.with = obj@coercewith
              if(is.null(coerce.with))
                return(vals)
              else
                return(coerce.with(vals))
            }
          })

## toggles state to be T or F
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gCheckboxgrouprJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   items = tag(obj,"items")
                   lst = tag(obj,"itemlist")
                   values = rep(value, length.out=length(items))

                   sapply(1:length(items), function(i) svalue(lst[[i]]) <- values[i])
                   
                   return(obj)
                 })

## [ and [<- refer to the names -- not the TF values

setMethod("[",
          signature(x="gCheckboxgrouprJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gCheckboxgrouprJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            items = tag(x,"items")
            if(missing(i))
              return(items)
            else
              return(items[i])
          })

## assigns names
setReplaceMethod("[",
                 signature(x="gCheckboxgrouprJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gCheckboxgrouprJava"),
          function(x, toolkit, i, j, ..., value) {
            items = tag(x,"items")
            lst = tag(x,"itemlist")
            n = length(items)

            if(missing(i))
              i = 1:length(items)
  
            if(is.logical(i))
              i = which(i)
            items[i] = value
            
            sapply(1:n, function(i) 
                   lst[[i]][] <- items[i]
                   )
            tag(x,"items") <- items
            tag(x,"itemlist") <- lst
  
             return(x)
          })

## handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gCheckboxgrouprJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            sapply(tag(obj,"itemlist"),function(i) {
              addhandlerchanged(i, handler, action, ...)
            })
            
          })
