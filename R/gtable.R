## table for selecting values
## most methods in gdf.R inherited from gGrid class
setClass("gTablerJava",
         representation = representation("gGridrJava",
           chosencol="numeric"),

         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )


## ## constructor for selecting values from a data set -- not meant for editing
setMethod(".gtable",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   items,
                   multiple = FALSE,
                   chosencol = 1,                        # for drag and drop, value
                   icon.FUN = NULL,
                   filter.column = NULL,
                   filter.labels = NULL,
                   filter.FUN = NULL,   # two args gtable instance, filter.labels element
                   handler = NULL,
                   action = NULL,
                   container = NULL,
                   ...) {

            ## NOT IMPLEMENTED
            ## * icon.FUN
            ## * filtering
            ## * sorting
            
            force(toolkit)
            
            ## the colors
            theArgs = list(...)
            colors = theArgs$colors
            if(is.null(colors))
              colors = c(
                bg = "navajo white",fg = "black",
                rbg = "white smoke",rfg="red"
                )
            

            x = items
            
            if(is.vector(x))
              x = as.data.frame(x)
            ## get dimensions
            d = dim(x)

            ## Using JTable
##            tbl = jnew("JTable",as.integer(d[1]),as.integer(d[2]))
##            sp = jnew("JScrollPane", .jcast(tbl,"java/awt/Component"))
##            .jcall(tbl,"V","setAutoResizeMode", tbl$AUTO_RESIZE_OFF)

            ## with gTable
            sp = .jnew("gWidgetsrJava/gTable",as.integer(d[1]),as.integer(d[2]), as.integer(chosencol-1))
            tbl = .jcall(sp,"Ljavax/swing/JTable;","getTable")
            

            obj = new("gTablerJava",block=sp,widget=tbl,
              toolkit=toolkit,ID=getNewID(),
              chosencol = as.numeric(chosencol))

            obj[,] <- x

            ## set classes for each column
            theClass = sapply(x,class)
            for(col in 1:dim(x)[2]) {
              ## make a new column in object
              .jcall(sp,"V","setColumnRclass", # sp is the gDf object
                     as.integer(col),
                     .jnew("java/lang/String",switch(theClass[col][[1]],
                                                     "character"="character",
                                                     "numeric"="numeric",
                                                     "integer"="numeric",
                                                     "logical"="logical",
                                                     "factor"="factor",
                                                     "character"))
                     
                     )
            }

            ## set names
            names(obj) <- names(x)

            ## make look a bit different
            .jcall(tbl,"V","setShowGrid", FALSE)
            .jcall(tbl,"V","setShowHorizontalLines", TRUE)


            ## multiple
            if(!as.logical(multiple)) {
              ## set single, multiple is default
              lsm = tbl$getSelectionModel()
              .jcall(lsm,"V","setSelectionMode",
                     lsm$SINGLE_SELECTION)
            }
            
            ## no cell editing
            ## *implement me
            

            ## add handler
            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }

            
            ## add to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj)
            }


            return(obj)
            
          })


## These are defined in gDf for fGrid objects. We only make changes here
## data frame methods

## incorporate chosenval here
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTablerJava"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            tbl = obj@widget
            lsm = tbl$getSelectionModel()
            
            if(.jcall(lsm,"Z","isSelectionEmpty"))
              return(NA)                          # nothing selected
            
            indices = sapply(1:dim(obj)[1],function(i) .jcall(lsm,"Z","isSelectedIndex",as.integer(i-1)))

            
            if(!is.null(index) && index == TRUE)
              return(indices)
            
            ## Now a value
            if(missing(drop) || is.null(drop))
              drop = FALSE

            ##
            chosencol = obj@chosencol
            if (is.null(chosencol) || drop==FALSE)
              return(obj[indices,,drop=drop])
            else
              return(obj[indices,chosencol,drop=drop])
          })

## handlers

setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTablerJava"),
          function(obj, toolkit, handler, action=NULL, ...) {

            ## for gTable
            ID = addJHandler(obj,handler, action,
              type="addMouseListener",
              event = "TwoMouseClicked",
              class = "java/awt/event/MouseListener")
           return(ID)

##             ## for JTable
##             theTable = getWidget(obj)
##             lsm = theTable$getSelectionModel() ## handler goes here!
##             addJHandler(obj,handler, action,
##                         type="addListSelectionListener",event = "",,
##                         class = "javax/swing/event/ListSelectionListener",
##                         jobj = lsm)

            
          })
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTablerJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerdoubleclick(obj, handler, action)
          })

