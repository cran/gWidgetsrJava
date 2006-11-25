gMessage = function(msg, icon="info", button="close") {

  m = .jnew("gWidgetsrJava/gMessage",
    .jnew("java/lang/String",msg),
    .jnew("java/lang/String",icon),
    .jnew("java/lang/String",button))

  return(m)
}
