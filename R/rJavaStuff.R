## coerce to java/awt/Component
as.jcomponent = function(x) .jcast(x,"java/awt/Component")
as.jstring = function(x) .jnew("java/lang/String",x)
is.rJava = function(x) "jobjRef" %in% class(x)

## convenience
jnew = function(x,...,type="javax/swing/") {
  tmp = paste(type,x,sep="",collapse="")
  .jnew(tmp,...)
}
jcall = function(x,method,...,ret="V")
  .jcall(x,ret,method,...)


## make a jobject
asjobject = function(x) UseMethod("asjobject")

asjobject.character = function(x)
  .jcast(.jnew("java/lang/String",x),"java/lang/Object")

asjobject.numeric = function(x) {
  if(is.integer(x)) 
    .jcast(.jnew("java/lang/Integer",as.integer(x)),"java/lang/Object")
  else
    .jcast(.jnew("java/lang/Double",x),"java/lang/Object")
}


## get the frame for this rJava object
getWindow = function(obj)obj$getRootPane()$getParent()



## return rJava objects from others
getBlock = function(widget) {
  if(is.rJava(widget)) return(widget)
  if(is(widget,"gWidgetrJava")) return(getBlock(widget@block))
  if(is(widget,"guiWidget")) return(getBlock(widget@widget))
  cat("Can't get block")
  return(NA)
}

getWidget = function(widget) {
  while(!is.rJava(widget))
    widget = widget@widget
  return(widget)
}

