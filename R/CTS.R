CTSgetR<-function(id,from,to,parallel=FALSE,async=TRUE,limit.values=TRUE,server="http://cts.fiehnlab.ucdavis.edu/service/convert"){ 
		# id = vector of identifiers
		#to/from =	"Chemical Name","InChIKey","InChI Code","PubChem CID","Pubchem SID","ChemDB","ZINC","Southern Research Institute","Specs","MolPort","ASINEX","ChemBank","MLSMR","Emory University Molecular Libraries Screening Center","ChemSpider","DiscoveryGate","Ambinter","Vitas-M Laboratory","ChemBlock"
		#result is a 3 column data frame of from, to and translated values
		
		options(warn=-1) 
		
		#get URL contents control the number of urls sent to avoid issues
		message(cat("Getting translations...","\n"))
		if(async){
			tryCatch(out<-CTS.translate.async(server=server,from=from,to=to,id=id) ,error=function(e){stop("Check that CTS server is working")})
		} else{
			out<-CTS.translate(server=server,from=from,to=to,id=id,parallel=parallel) 
		}
		
		#fxn to parse JSON		
		.parseJSON<-function(obj,limit)
			{
					if(any(obj=="[]")) # no value from server
					{
							final<-data.frame(matrix("error",nrow=4))
					} else {
							if(limit==TRUE) #only return first answer
							{ 
								final<-tryCatch(data.frame(RJSONIO::fromJSON(obj))[,1,drop=FALSE],error=function(e){data.frame(matrix("error",nrow=4))})
							} else {
								final<-tryCatch(data.frame(RJSONIO::fromJSON(obj)),error=function(e){data.frame(matrix("error",nrow=4))})
								if(ncol(final)>1) # combine multiple answers in a comma separated string
									{
										tmp<-as.matrix(final[,1,drop=F])
										tmp[4,1]<-paste(as.character(unlist(final[4,])),collapse=",")
										final<-tmp
									}
							}
					}
				
				return(final)	
			}
			
		message(cat("\n","Formatting output...","\n"))
		if(parallel==TRUE){ # add progress bar
				
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK")  # windows specific
				registerDoSNOW(cl.tmp) 
				#do work
				out<-foreach(i=c(1:length(out))) %dopar% .parseJSON(out[i],limit=limit.values)
				stopCluster(cl.tmp)	
		} else {
				 
				out<-lapply(1:length(out),function(i,pb = txtProgressBar(min = 0, max = length(id), style = 3)){
					setTxtProgressBar(pb, i)
					.parseJSON(out[i],limit=limit.values)})
			}
			
		#parse  into a triple
		tmp<-do.call("cbind",out)
		tmp<-cbind(from=id,to=to,result=as.character(unlist(tmp[4,])))
		colnames(tmp)<-c(paste("from",from,sep=" :"),"to:",to)
		return(data.frame(tmp[,3,drop=FALSE]))
	}

CTS.translate<-function(server,from,to,id,parallel=FALSE){ #arguably parallel, seems more connection stable than asynchronous
		#require("RCurl")
		# results are returned as JSON encoded strings
		id<-as.character(unlist(id))
		url<-paste(server,from,to,id,sep="/")
		url<-gsub("\\ ","%20",url) # fix spaces 
		
		if(parallel==TRUE){ # not clear if actuall improves speed, why RCurls was tried first
				library(snow);library(doSNOW);library(foreach)
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK")  # windows specific
				registerDoSNOW(cl.tmp) 
				#do work
				content<-as.character(unlist(foreach(i=c(1:length(id))) %dopar% readLines(url[i])))
				stopCluster(cl.tmp)		
				
		} else {
			content<-as.character(unlist(sapply(1:length(id), function(i, pb = txtProgressBar(min = 0, max = length(id), style = 3))
				{
					setTxtProgressBar(pb, i)
					tryCatch(readLines(url[i]),error=function(e){"error"})
				})))
		}
			return(content)
}

#asynchronous
CTS.translate.async<-function(server,from,to,id){ # sometimes will not work due to loss of connection when webservices are overwhelemed
		require("RCurl")
		# results are returned as JSON encoded strings
		id<-as.character(unlist(id))
		url<-paste(server,from,to,id,sep="/")
		url<-gsub("\\ ","%20",url) # fix spaces 
		
		options(RCurlOptions = list(verbose = TRUE,
                              followlocation = TRUE,
                              autoreferer = TRUE,
                              nosignal = TRUE))
							  				  
		#out<-getURL(url,  ssl.verifypeer = FALSE) not stable missing some args?
		con = multiTextGatherer(url)
		getURIAsynchronous(url, write = con)
}

# get possible translations from CTS
CTS.options<-function(){
	options(warn=-1)	
	url<-readLines("http://cts.fiehnlab.ucdavis.edu/service/convert/toValues")
	RJSONIO::fromJSON(url)
	}

#from one to multiple translations
multi.CTSgetR<-function(id, from, to, limit.values, server) {
  obj<-sapply(1:length(to),function (i)
  {  
    CTSgetR(id,from,to[i])
  })
  tmp<-do.call("cbind",obj)
  colnames(obj)<-c(paste("From:",from),"To","Value")
  obj  
}

test<-function(){
	id<-rep(c(14242, 5760),300) # PubChem CIDs
	from<-"PubChem CID"
	to<-"InChIKey"
	
	start<-Sys.time()
	x1<-CTSgetR(id,from,to,async=FALSE)
	ellapsed1<-Sys.time()-start
	
	start<-Sys.time()
	x2<-CTSgetR(id,from,to)
	ellapsed2<-Sys.time()-start
	
	
}