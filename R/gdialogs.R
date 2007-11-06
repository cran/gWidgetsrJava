## some dialogs for R
## dialogs don't get windows, they make them
## dialogs are modal
## dialogs return their value -- not an object. so source(gfile()) should work

setMethod(".gmessage",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   message,
                   title = "message",
                   icon = c("info","warning","error","question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {

            icon = match.arg(icon)

            if(!is.null(parent)) gwCat("Implement parent in gmessage\n")
            
            op = .jnew("gWidgetsrJava/gDialog")
            .jcall(op,"V","gMessage",
                   .jnew("javax/swing/JFrame", .jnew("java/lang/String",title)),
                   .jnew("java/lang/String",as.character(message)),
                   .jnew("java/lang/String",as.character(title)),
                   .jnew("java/lang/String",icon)
                   )
            
          })
  
## if OK then run handler, else not
setMethod(".gconfirm",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   message,
                   title = "Confirm",
                   icon = c("info", "warning", "error", "question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            icon = match.arg(icon)

            if(!is.null(parent)) gwCat("Implement parent in gconfirm\n")

            op = .jnew("gWidgetsrJava/gDialog")
            ans= .jcall(op,"I","gConfirm",
              .jnew("javax/swing/JFrame", .jnew("java/lang/String",title)),
              .jnew("java/lang/String",as.character(message)),
              .jnew("java/lang/String",as.character(title)),
              .jnew("java/lang/String",icon)
              )
            ## 1 for yes, 0 for no -1 for cancel
            if(ans == 1)
              invisible(TRUE)
            else
              invisible(FALSE)
          })


## Add input to the above
## h,... in handler has componets action, input (for value)
setMethod(".ginput",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   message,
                   text = "",
                   title = "Input",
                   icon = c("info","warning","error","question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            icon = match.arg(icon)


            if(!is.null(parent)) gwCat("Implement parent in ginput\n")
            
            op = .jnew("gWidgetsrJava/gDialog")
            ans= .jcall(op,"S","gInput",
              .jnew("javax/swing/JFrame",
                    .jnew("java/lang/String",as.character(title))),
              .jnew("java/lang/String",as.character(message)),
              .jnew("java/lang/String",as.character(text)),
              .jnew("java/lang/String",as.character(title)),
              .jnew("java/lang/String",icon)
         )

            ## call handler if asked
            if(!is.null(handler)) 
              handler(list(obj=NULL, action=action, input=ans))
                      
            invisible(ans)
            
            
          })

## add a widget to the dialog. This is modal
setMethod(".gbasicdialog",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   title = "Dialog",
                   widget,
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
  
            icon = match.arg(icon)

            if(!is.null(parent )) gwCat("implement parent in gbasicdialog\n")
            
            g = ggroup()
            add(g,widget)
            
            op = .jnew("gWidgetsrJava/gDialog")
            ans= .jcall(op,"I","gBasicDialog",
              .jnew("javax/swing/JFrame", .jnew("java/lang/String",as.character(title))),
              g@widget@widget,
              .jnew("java/lang/String",as.character(title))
              )
            
            if(ans == 1) {
              ## yes
              if(!is.null(handler)) {
                handler(list(ref=widget,widget=widget,action=action, ...))
              }
              return(invisible(TRUE))
            } else {
              ## no
              return(invisible(FALSE))
            }
            
            return(ans)
          })

