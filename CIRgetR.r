CIRgetR<-function(id,to=c("pubchem_sid"),return.all=TRUE,progress=TRUE){
		#id needs to be  one of the folowing types of structural ids "inchi","inchiKey" or "smiles"
		#to can be:	smiles, names, iupac_name, cas, inchi, 
		#			stdinchi, inchikey, stdinchikey,
		#			ficts, ficus, uuuuu, image, # here return url do not evaluate 
		#			mw, monoisotopic_mass,file, 
		#			chemspider_id
		#			pubchem_sid, chemnavigator_sid, formula, chemnavigator_sid


		# smiles are coerced to a 1 column data frame
		obj<-data.frame(matrix(unlist(id),ncol=1))
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)} # need RCurl for web querry
		if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = nrow(obj), style = 3)} # show progress bar
	
		start<-"http://cactus.nci.nih.gov/chemical/structure/"
		end<-paste("/",to,sep="")
		out<-sapply(1:nrow(obj),function(i)
			{
				if (progress == TRUE){setTxtProgressBar(pb, i)}
				url<-paste(start,as.character(unlist(obj[i,])),end,sep="")
				url<-gsub("\\ ","%20",url) # fix spaces 
				if(end=="/image"){ # return string for url
						url
					} else {
						tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){"error"})
				}
					
			})
			
			if (progress == TRUE){close(pb)}
			
		#format output to only return InchI
		bad<-is.na(out)
		out<-as.character(unlist(out))
		out[bad]<-"error"
		if(return.all==FALSE){ # only return first result
				obj<-strsplit(out,"\n")
				out<-sapply(1:length(obj),function(i)
					{
						obj[[i]][1]
					})
			} 
		
		results<-matrix(out,ncol=1)
		colnames(results)<-to
		return(data.frame(results))
		}
