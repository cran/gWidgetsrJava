## add calendar widget: shoule I have gcalendar, gcalendarbrowser?
## no handler function, can add to entry object with addhandler

setClass("gCalendarrJava",
         representation = representation("gComponentrJava",
           format="character"),
         contains="gEditrJava",
         prototype=prototype(new("gEditrJava"))
         )


setMethod(".gcalendar",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="",
                   format="%Y-%m-%d",
                   handler = NULL, action=NULL,
                   container=NULL,...) {

            force(toolkit)

            
            ## This just selects a date to putinto textarea
            datepicker = .jnew("gWidgetsrJava/JXDatePicker")
            
            obj = new("gCalendarrJava",
              block=datepicker, widget = datepicker, toolkit=toolkit,
              ID=getNewID(), format=format)

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj)
            }

            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }
            
            invisible(obj)
          })

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gCalendarrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            jobj = getWidget(obj)
            date = jobj$getDate()$toString()

            ## need to strip out TZ -- can't convert to date object
            ## easily
            n = nchar(date)
            date = paste(substr(date,1,19),substr(date,n-3,n),sep=" ")
            date = as.Date(date,"%a %b %d %T %Y")

            format = obj@format
            date = format(date, format=format)

            return(date)
          })

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gCalendarrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {

            ID = addJHandler(obj,handler, action,
              type="addActionListener",
              event = "ActionEvent",
              class = "java/awt/event/ActionListener")
            return(ID)
          })

