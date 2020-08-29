

#' @export
#' @details 
heartbeat<-function(){
  return('OK')
}


null_replace<-function(x,alt=NA){
  if(is.null(x) || length(x) == 0 ) alt else x
}
