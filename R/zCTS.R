CTS.getR<-function(id,from,to,parallel=FALSE,server="http://uranus.fiehnlab.ucdavis.edu:8080/cts-2.0-beta-2/service/convert"){ 
		# id = vector of identifiers
		#to/from =	"Chemical Name","InChIKey","InChI Code","PubChem CID","Pubchem SID","ChemDB","ZINC","Southern Research Institute","Specs","MolPort","ASINEX","ChemBank","MLSMR","Emory University Molecular Libraries Screening Center","ChemSpider","DiscoveryGate","Ambinter","Vitas-M Laboratory","ChemBlock"
		#result is a 3 column data frame of from, to and translated values
		#check.get.packages(c("snow","doSNOW","foreach","RJSONIO","RCurl"))
		
		#may need to detach RCurl upon strange error
		#detach("package:RCurl", unload=TRUE)
		
		options(warn=-1) # yehaw
		
		#get URL contents control the number of urls sent to avoid issues
		message(cat("Getting translations...","\n"))
		out<-CTS.translate(server=server,from=from,to=to,id=id,parallel=parallel) 
		#closeAllConnections() # trying to avoid loss of connection erro for later
		
		#fxn to parse JSON		
		.parseJSON<-function(obj){
				
				if(any(obj=="[]")) # no value from server
					{
							final<-data.frame(matrix("error",nrow=4))
						} else {
							final<-tryCatch(data.frame(RJSONIO::fromJSON(obj))[,1,drop=FALSE],error=function(e){data.frame(matrix("error",nrow=4))})
				}
				return(final)	
			}
			
		message(cat("\n","Formatting output...","\n"))
		if(parallel==TRUE){ # add progress bar
				
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK")  # windows specific
				registerDoSNOW(cl.tmp) 
				#do work
				out<-foreach(i=c(1:length(out))) %dopar% .parseJSON(out[i])
				stopCluster(cl.tmp)	
		} else {
				out<-lapply(1:length(out),function(i,pb = txtProgressBar(min = 0, max = length(id), style = 3)){
					setTxtProgressBar(pb, i)
					.parseJSON(out[i])})
			}
			
		#parse  into a triple
		tmp<-do.call("cbind",out)
		tmp<-cbind(from=id,to=to,result=as.character(unlist(tmp[4,])))
		colnames(tmp)<-c(paste("from",from,sep=" :"),"to:","RESULT")
		return(tmp)
	}
	
# CTS.translate<-function(server,from,to,id,number=NULL){ # ussues with loss of connection, seems to be due to RCurl?
		# #require("RCurl")
		# # results are returned as JSON encoded strings
		# url<-paste(server,from,to,id,sep="/")
		# url<-gsub("\\ ","%20",url) # fix spaces 
		
		# if(!is.null(number)) # control number of asyncronous calls getURL (too many can lead to connection problems)
			# {
				# if(number>length(id)){number<-length(id)}
				# end.id<-c(1:length(id))[c(1:length(id))%%number==0]
				# start.id<-end.id-number+1
				# extra<-length(id)-max(end.id)# for the ends
				# start.id<-c(start.id,(max(end.id)+1))
				# end.id<-c(end.id,max(end.id+extra))
				
				# content<-as.character(unlist(sapply(1:length(end.id), function(i,start=start.id, val=url, pb = txtProgressBar(min = 0, max = length(end.id), style = 3))
					# {
						# setTxtProgressBar(pb, i)
						# tmp<-val[start[i]:end.id[i]]
						# as.character(unlist(getURL(tmp,ssl.verifypeer=FALSE)))
						# closeAllConnections() # trying to avoid loss of connection... may want to create a dedicated connections and keep calling it
					# })))
			
			# } else {
				# content<-as.character(unlist(getURL(url,ssl.verifypeer=FALSE)))
			# }
		
			# return(content)
# }

CTS.translate<-function(server,from,to,id,parallel=FALSE){ #arguably parallel, seems more connection stable than asynchronous
		#require("RCurl")
		# results are returned as JSON encoded strings
		url<-paste(server,from,to,id,sep="/")
		url<-gsub("\\ ","%20",url) # fix spaces 
		
		if(parallel==TRUE){ # not clear if actuall improves speed, why RCurls was tried first
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK")  # windows specific
				registerDoSNOW(cl.tmp) 
				#do work
				content<-as.character(unlist(foreach(i=c(1:length(id))) %dopar% readLines(url[i])))
				stopCluster(cl.tmp)		
				
		} else {
			content<-as.character(unlist(sapply(1:length(id), function(i, pb = txtProgressBar(min = 0, max = length(id), style = 3))
				{
					setTxtProgressBar(pb, i)
					readLines(url[i])
				})))
		}
			return(content)
}

# get possible translations from CTS
CTS.options<-function(){
	options(warn=-1)	
	url<-readLines("http://cts.fiehnlab.ucdavis.edu/service/convert/fromValues")
	tmp<-unlist(strsplit(url[1],""))
	obj<-tmp[-c(1:2,(length(tmp)-2):length(tmp))]
	unlist(strsplit(paste(obj[!obj=='\"'],collapse=""),","))
	}

