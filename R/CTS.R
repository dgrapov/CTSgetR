#' @title CTSgetR
#' @param id a vector of metabolite identifier(s) or name(s) to translate see \code{\link{CTS.options}}
#' @param from Database name describing \code{id} see \code{\link{CTS.options}}
#' @param to Database name to translate \code{id} to see \code{\link{CTS.options}}
#' from	Database name describing \code{id} see \code{\link{CTS.options}}
#' @param limit.values return a single (first non empty) value or a comma separated string of multiple values (defaults to FALSE).
#' @param server Web address for the Chemical Translation System web services (defaults to http://cts.fiehnlab.ucdavis.edu/service/convert).
#' @return data frame of results including \code{from} (fromIdentifier), \code{id} (searchTerm), \code{to} (toIdentifier) and translated values
#' @details Interface to CTS (http://cts.fiehnlab.ucdavis.edu/) for metabolite identifier translation between > 200 of the most common biological databases including: Chemical Name, InChIKey, PubChem CID, ChemSpider, BioCyc, ChEBI, CAS, HMDB, KEGG and LipidMAPS. Entries not found in CTS are returned as "", CTS errors are returned as 'error'. Observation:  CTS errors can be random, i.e. running the same query multiple times might result in success or error :(
#' @seealso  \code{\link{CTS.options}}
#' @export
#' @import httr
#' @examples
#' \dontrun{
#' id<-c("C15973","C00026","C05381")
#' from<-"KEGG" 
#' to<-"PubChem CID"
#' CTSgetR(id,from,to)
#' }
CTSgetR<-function(id,from,to,limit.values=TRUE,progress=TRUE,server="http://cts.fiehnlab.ucdavis.edu/service/convert"){ 

	opts<-CTS.options()
	if(any(!to%in%opts) || any(!from%in%opts)) {
	
	stop(
	  paste0("The supplied to: ",to, " or from: ", from," are not in the list of available options.\nSee CTS.options() for possible arguments.","\n",collapse="")
	)
	}
# 
# 	fixlc<-function(obj){as.character(unlist(obj))} #redo w/ dplyr and httr
	if(progress) cat("Getting translations...","\n")

	  if(length(to)>1){
	    CTSgetR:::multi.CTSgetR(server=server,from=from,to=to,id=id,progress=progress,limit.values=limit.values)
	  } else {
	    CTSgetR:::CTS.translate(server=server,from=from,to=to,id=id,progress=progress,limit.values=limit.values) 
	  }
		
	
}


CTS.translate<-function(server,from,to,id,progress=TRUE,limit.values=TRUE){ 
	  
  
    url<-paste(server,from,to,id,sep="/")
	
		if(progress) pb <- txtProgressBar(min = 0, max = length(id), style = 3)
		content<-lapply(1:length(id), function(i)
			{
				if(progress) setTxtProgressBar(pb, i)
		    out<-content(GET(URLencode(url[i]))) 
		    #condition on results
		    if(length(out[[1]]$result)==0) {
		      out[[1]]$result<-""
		      #remove error
		      if(!is.null(out[[1]]$error)) {
		        out[[1]]$result<-"error"
		        out[[1]]$error<-NULL
		      }
		    }  
	      #combine multiple results 
	      #combine multiple results
	      out[[1]]$result<-data.frame(value=unlist(out[[1]]$result))
	      
		    #parse multiple returns
		  if(limit.values) {
		    setNames(data.frame(out),c('fromIdentifier','searchTerm','toIdentifier','value'))[1,,drop=FALSE]
		  } else {
		    setNames(data.frame(out),c('fromIdentifier','searchTerm','toIdentifier','value'))
		  } 
		   
			})
		
		if(progress) close(pb)
		
		
		return(do.call("rbind",content))
}


# get possible translations from CTS
#' @title CTS.options
#' @param url for options
#' @export
#' @import httr
#' @details get translation options from CTS
CTS.options<-function(url="http://cts.fiehnlab.ucdavis.edu/service/conversion/toValues"){
		content(GET(url))
	}

#' @title multi.CTSgetR
multi.CTSgetR<-function(id, from, to ,...) {
  #TODO make parallel
  obj<-lapply(1:length(to),function (i)
  {  
    CTSgetR(id,from,to[i],...)
  })
  
  return(do.call("rbind",obj))
  
}
