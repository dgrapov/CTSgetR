
#' #' @param data original data used for the query
#' #' @param query results form CTSgetR
#' #' @param from name of translated variable in the data
#' #' @param to name of data base translated to in the query
#' #' @export
#' #' @details combine query results with an existing data set
#' #' @import dplyr
#' CTSgetR_join<-function(data,query,from,to){
#'   
#'   #join
#'   by<-'searchTerm'
#'   names(by)<-from
#'   out<-left_join(data,
#'                  query,
#'                  by=by)
#'   
#'   #cleanup
#'   out<- out %>%
#'     select(one_of(c(colnames(data),'value')))
#'   colnames(out)[colnames(out) == 'value']<-make.names(to)
#'   
#'   return(out)
#'   
#' }
#' 



#' #check if cid is valid
#' @export
#' @param cid compound identifier see https://pubchem.ncbi.nlm.nih.gov/
#' @details Check if valid CID; used to filter bad queries to PUG endpoints
check_cid<-function(cid){
  
  f<-function(cid){
    
    pb$tick()$print()
    
    url<-paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/',
                cid,
                '/property/MolecularFormula/JSON')
    
    res<-GET(url)
    status<-http_status(res)
    msg<-paste0(status$message,": ",cid)
    
    out<-list(status=status$category,message=msg)
    
    
    if(status$category != 'Success'){
      out$valid<-FALSE
    } else {
      out$valid<-TRUE
    }
    
    data.frame(out)
  }
  
  pb <- progress_estimated(length(cid))
  
  as.list(cid) %>%
    map( ~ f(.)) %>%
    do.call('rbind',.)
  
}

null_replace<-function(x,alt=NA){
  if(is.null(x) || length(x) == 0 ) alt else x
}
