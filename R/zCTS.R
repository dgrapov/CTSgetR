#use CTS to get translations between identifiers
CTS.getR<-function(id,from,to,progress=TRUE){ #add parallel processing
		# id = vector of identifiers
		#to/from =	"Chemical Name","InChIKey","InChI Code","PubChem CID","Pubchem SID","ChemDB","ZINC","Southern Research Institute","Specs","MolPort","ASINEX","ChemBank","MLSMR","Emory University Molecular Libraries Screening Center","ChemSpider","DiscoveryGate","Ambinter","Vitas-M Laboratory","ChemBlock"
		#result is a 3 column data frame of from, to and translated values
		
		#if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)} # need RCurl for web querry
		#if(require(RJSONIO)==FALSE){install.packages("RJSONIO");library(RJSONIO)} else { library(RJSONIO)} # for parsing JSON
		
		if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = length(id), style = 3)} # show progress bar
	
		start<-"http://uranus.fiehnlab.ucdavis.edu:8080/cts-2.0-beta-2/service/convert" 
	
		out<-lapply(1:length(id),function(i)
			{
				if (progress == TRUE){setTxtProgressBar(pb, i)}
				url<-paste(start,from,to,id[i],sep="/")
				url<-gsub("\\ ","%20",url) # fix spaces 
				out<-as.character(unlist(tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){"error"})	))
				tryCatch(data.frame(fromJSON(out))[,1,drop=FALSE],error=function(e){data.frame(matrix("error",nrow=3))})	#limit to first answer	
				#later take many and bind into a string				
			})
			
		#parse  into a triple
			tmp<-do.call("cbind",out)
			tmp<-cbind(from=id,to=to,result=as.character(unlist(tmp[4,])))
			colnames(tmp)<-c(paste("from",from,sep=" :"),"to:","RESULT")
		if (progress == TRUE){close(pb)}
		return(tmp)
		}