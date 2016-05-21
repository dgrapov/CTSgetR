#' @title CTSgetR
#' @export
#' @import httr
CTSgetR<-function(id,from,to,limit.values=TRUE,progress=TRUE,server="http://cts.fiehnlab.ucdavis.edu/service/convert",...){ 

	opts<-CTS.options()
	if(!to%in%opts|!from%in%opts) {
	
	stop(paste0("The supplied to or from name is not in the list of available options.","\n",
	"Did you mean from = '", opts[agrep(from,opts,ignore.case = TRUE)],"' and to = '",opts[agrep(to,opts,ignore.case = TRUE)],"'?", "\n",
	"See CTS.options() for all available options.","\n"))
	}

	fixlc<-function(obj){as.character(unlist(obj))} #redo w/ dplyr and httr
	if(progress) cat("Getting translations...","\n")

		out<-CTSgetR:::CTS.translate(server=server,from=from,to=to,id=id,progress=progress) 
	
	if(limit.values){ 
		parser<-function(x){
			tmp<-jsonlite::fromJSON(x)
			tryCatch(tmp[,"result"]<-strsplit(fixlc(tmp[,"result"]),"/,")[[1]],error=function(e) {NULL})
			return(tmp)
		}
		
		res<-do.call("rbind",lapply(out,parser))
		#format output
		res$result<-sapply(1:length(res[,"result"]),function(i){
				if(length(res[,"result"][[i]])==0) { ""
				} else {
					res[,"result"][[i]]
				}
			})
						
	} else {
		res<-do.call("rbind",lapply(out,jsonlite::fromJSON))
		#format output, could collapse on comma, not sure which is more useful?
		res<-do.call("rbind",lapply(1:length(res[,"result"]),function(i){
				if(length(res[,"result"][[i]])==0) { 
					expand.grid(res[,"searchTerm"][[i]],"")
				} else {
					expand.grid(res[,"searchTerm"][[i]],res[,"result"][[i]])
				}
			}))
		id<-res$Var1
		res$result<-res$Var2 # getting ugly	
			
	}
		
	out<-data.frame(from=id,to=res$result)
	colnames(out)<-c(from,to)	
	return(out)
	
}
	
CTS.translate<-function(server,from,to,id,progress=TRUE){ #arguably parallel, seems more connection stable than asynchronous
		#require("RCurl")
		# results are returned as JSON encoded strings
		id<-as.character(unlist(id))
		url<-paste(server,from,to,id,sep="/")
		url<-gsub("\\ ","%20",url) # fix spaces
		if(progress) pb <- txtProgressBar(min = 0, max = length(id), style = 3)
		content<-lapply(1:length(id), function(i)
			{
				if(progress) setTxtProgressBar(pb, i)
  				readLines(url[i])
			})
		if(progress) close(pb)
		return(content)
}

#slow and breaks?
#' #' @export
#' #' @import httr purrr
#' CTS.translate<-function(server,from,to,id,progress=TRUE){ #arguably parallel, seems more connection stable than asynchronous
#'   id<-as.character(unlist(id))
#'   url<-URLencode(paste(server,from,to,id,sep="/")) # tryin not to dplyr dep:  %>% URLencode()
#'   
#'   if(progress) pb <- txtProgressBar(min = 0, max = length(id), style = 3)
#'   content<-lapply(1:length(id), function(i)
#'   {
#'     if(progress) setTxtProgressBar(pb, i)
#'     tryCatch(content(GET(url[i])), error=function(e){NULL})
#'   })
#'   if(progress) close(pb)
#'   return(content)
#' }

# get possible translations from CTS
#' @title CTS.options
#' @export
#' @details get possible translations from CTS
CTS.options<-function(){
		options(warn=-1)	
		url<-readLines("http://cts.fiehnlab.ucdavis.edu/service/conversion/toValues")
		jsonlite::fromJSON(url)
	}

#from one to multiple translations
#' @title multi.CTSgetR
#' @export
#' @details from one to multiple translations
multi.CTSgetR<-function(id, from, to ,...) {
  obj<-lapply(1:length(to),function (i)
  {  
    tryCatch(CTSgetR(id,from,to[i],...)[,2,drop=FALSE], error=function(e){""})
  })
  tmp<-cbind(id,do.call("cbind",obj))
  colnames(tmp)<-c(from,to)
  tmp  
}

#TODO: make these tests via testthat
test<-function(){
	id<-c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
	from<-"KEGG" 
	to<-"PubChem CID"
	x<-CTSgetR(id,from,to)
	
	#multiple translations
	id<-"KPGXRSRHYNQIFN-UHFFFAOYSA-N"
	from<-"InChIKey"
	to<-c("PubChem CID","KEGG")
	x<-multi.CTSgetR(id,from,to)
	
	#get all values for the translation
	id <- c("QNAYBMKLOCPYGJ-REOHCLBHSA-N")
	from <- "InChIKey"
	to <- "Chemical Name"
	CTSgetR(id,from,to,limit.values=FALSE)
	
	#test
	id<-c("HMDB00012", "HMDB00302", "HMDB00226", "HMDB00296", "HMDB00902", 
	      "HMDB00217", "HMDB00050", "HMDB01273", "HMDB00139", "HMDB60618", 
	      "HMDB00288", "HMDB00190", "HMDB00807", "HMDB35100", "HMDB01051", 
	      "HMDB00162", "HMDB00805", "HMDB01565", "HMDB31769", "HMDB29933", 
	      "HMDB00736", "HMDB00230", "HMDB00251", "HMDB00094", "HMDB04814", 
	      "HMDB00227", "HMDB06024", "HMDB02322", "HMDB00191", "HMDB31342", 
	      "HMDB00121", "HMDB00148", "HMDB01046", "HMDB00042", "HMDB33722"
	)
	
}	
