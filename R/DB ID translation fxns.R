#convert KEGG id to chemical name
KEGGtoName<-function(id,all.values=FALSE,progress=TRUE){
	library(KEGGREST)
	#should vectorize
	results<-do.call("rbind",lapply(1:length(id),function(i)
	{
		if(progress==TRUE){message(i)}
		names<-tryCatch(keggGet(id[i])[[1]]$NAME,error=function(e){"error"})
		names<-gsub(";","",names)
		if(all.values==TRUE){paste(names,collapse=";")} else {names[1]}
	}))
	data.frame(KEGG.id=id,names=results)
}

#to do name to KEGG
NametoKEGG<-function(name){
	#duplicates cause problems and should be removed before hand to match data
	sids<-NametoPubChem(name,ID="sids")
	#KEGG to SID table
	lookup<-getURL("http://rest.kegg.jp/conv/pubchem/compound/") 
	tmp<-data.frame(matrix(unlist(strsplit(fixlc(strsplit(lookup,"\n")),"\t")),ncol=2,byrow=TRUE))
	tmp$SID<-gsub("pubchem:","",fixlc(tmp[,2]))
	tmp$KEGG<-gsub("cpd:","",fixlc(tmp[,1]))
	
	#extract results
	matched<-tmp[tmp$SID%in%fixlc(sids[,2]),c("KEGG","SID")]
	matched2<-sids[fixlc(sids[,2])%in%fixlc(tmp$SID),]
	id<-duplicated(fixlc(matched2$name))&!duplicated(fixlc(matched2$id))# need to maintain duplicate names but remove duplicate KEGG matches
	matched2<-matched2[!id,]
	id<-duplicated(fixlc(matched2$name))&!duplicated(fixlc(matched2$id))
	matched2<-matched2[!id,] # no clue why this needs to be done twice!
	
	#for now return first hit for each compound
	# matched2<-matched2[!duplicated(fixlc(matched2$name)),] # could id biologically relevant entries
	missing<-name[!name%in%fixlc(matched2$name)]
	if(length(missing)>0){
		matched2<-rbind(matched2,data.frame(name=missing,id="error"))
	} 
	
	trans<-translate.index(id=as.matrix(fixlc(matched2$id)), lookup=rbind(matched[,2:1],c("error","error")))
	res<-data.frame(name=matched2$name,KID=trans)
	res<-res[order(fixlc(res$name)),]
	#match the initial name order
	init<-data.frame(id=1:length(name),name=fixlc(name))
	init<-init[order(init$name),]
	back<-order(init$id)
	return(res[back,])	
}

#CID to Name
CIDtoName<-function(cid){
	library(RCurl)
	url<-paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/",cid,"/synonyms/TXT")
	content<-tryCatch(getURL(url, ssl.verifypeer = FALSE),error=function(e){character()})
	res<-unlist(strsplit(content,"\n"))
	if(length(cid)>1){
		cids<-matrix(unlist(strsplit(names(res),"https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/")),ncol=2,byrow=T)
		cids<-matrix(unlist(strsplit(cids[,2],"/synonyms/")),ncol=2,byrow=T)[,1]
	} else { cids<-cid }	
	data.frame(id=cids,name=as.character(res[]))
}

#going from name to CID 
NametoPubChem<-function(name,ID="cids", limit=FALSE){
	library(RCurl)
	url<-paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/",gsub(" ","%20",name),"/",ID,"/TXT")
	content<-tryCatch(getURL(url, ssl.verifypeer = FALSE),error=function(e){character()})
	res<-unlist(strsplit(content,"\n"))
	names<-name 
	if(length(name)>1){

			names<-matrix(unlist(strsplit(names(res),"https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/")),ncol=2,byrow=T)
			names<-gsub("%20"," ", matrix(unlist(strsplit(names[,2],paste0("/",ID,"/"))),ncol=2,byrow=T)[,1])
		} 
	#results with many matches
	res<-data.frame(name=names,id=as.character(res[]))
	if(limit){
		bigl<-split(res,factor(res[,1], levels=name)) #need to make sure the levels match the name
		res<-do.call("rbind",lapply(1:length(bigl),function(i){
			tmp<-bigl[[i]]
			if(!all(is.na(fixln(tmp[,2])))){
				tmp[order(fixln(tmp[,2]),decreasing=F),][1,]# pick smallest cid
			} else {
				tmp[1,]	
			}
		}))
	}
	#remove error description for consistency
	res$id<-gsub("Status: 404","error",fixlc(res[,2]))
	return(res)
}

# get IDs (only KEGG and PubChem) from CTS based on synonym and verify using web services
CTSgetR.verify<-function(id,to=c("KEGG","PubChem CID"),max.distance=.5){
	library(vwr) # string distance
	library(CTSgetR) # translations
	
	IDS<-CTSgetR(id,from="Chemical Name",to,limit.values=FALSE)
	
	if(to=="KEGG"){
		#verify KEGG id by translating to name
		big.l<-strsplit(fixlc(IDS[,2]),",")
		k.names<-lapply(1:length(big.l),function(i){
			KEGGtoNames(id=big.l[[i]],all.values=TRUE,progress=TRUE)
		})
		
		#match on name
		matched.kid<-do.call("rbind",lapply(1:length(id),function(i){
			met<-id[i]
			tmp<-k.names[[i]]
			matches<-do.call("rbind",lapply(1:nrow(tmp),function(j){
					tmp.names<-strsplit(as.matrix(tmp[j,"names",drop=F]),";")[[1]]
					match<-agrep(met,tmp.names,max.distance,ignore.case = TRUE) # very loose matching could go right to lev.d distance?
					
					if(length(match)>0){
					dist<-levenshtein.damerau.distance(met, tmp.names[match])
					data.frame(KID=as.matrix(tmp[j,"KEGG.id"]),name=tmp.names[match],distance=dist)} else {data.frame(KID=tmp[j,"KEGG.id",drop=T],name="error",distance=NA)}
				}))
			
			#choose compounds over drugs
			if(is.null(matches)){return(data.frame(KID="error",name="error"))} else {tmp<-matches}
			if(nrow(matches)>1){
				tmp<-matches[which.min(matches$distance),]
				#check for compd and drug entries and select compound
				if(nrow(tmp)>1){
					tmp<-fixlc(matches[,1])
					id<-grep("C",tmp)[1] #select cmpd
					tmp<-tmp[id,tmp]
				}
			} 	
			return(tmp[,1:2,drop=FALSE])	
		}))
		matched.kid$start.name<-id
		results<-matched.kid # should standardize colnames?
		
	}
	
	if(to=="PubChem CID"){
		#to verify CIDS use PUG
		big.l<-unlist(strsplit(fixlc(IDS[,2]),","))
		len<-sapply(strsplit(fixlc(IDS[,2]),","),length)
		big.l<-data.frame(name=rep(id,times=len),id=big.l)
		met.names<-unlist(IDS[,1])
		#need to match index for splitting used down lower
		# avoid all errors getting collected by enumerating?
		tmp<-fixlc(IDS[,2])
		er<-tmp=="error"
		tmp[er]<-paste(tmp[er],1:sum(er))
		big.l$id<-unlist(strsplit(tmp,","))
		c.names<-CIDtoNames(cid=big.l$id) # vectorized
		#need to group multiple CIDs for one metabolite for word matching
		#get best matches for each id
		names<-fixlc(c.names[,2])
		matched.cid<-do.call("rbind",lapply(1:length(id),function(i){
			met<-id[i]
			dist<-levenshtein.damerau.distance(met,names)
			data.frame(c.names[which.min(dist)[1],],start.name=met) # control what distance is acceptable , rewrite above to vectorized form
		}))	
		
				
		}))	
		return(results)
	}
	
#check synonym via chemify http://cts.fiehnlab.ucdavis.edu/chemify/rest
NametoInchI<-function(name){
	library(RCurl)
	library(RJSONIO)
	url<-paste0("http://cts.fiehnlab.ucdavis.edu/chemify/rest/identify/",gsub(" ","%20",name))
	content<-tryCatch(lapply(url,getURL, ssl.verifypeer = FALSE),error=function(e){character()}) # bad catch error
	translate<-function(obj,return.max=TRUE){
		res1<-lapply(1:length(obj), function(i){
			tmp<-obj[[i]]
			res<-data.frame(do.call("rbind",RJSONIO::fromJSON(tmp)))
			if(return.max){
				res<-res[which.max(res$score),,drop=F]
			}
			res
		})
		return(res1)
	}
	#not clear why one fxn cant do all of this in one loop
	res<-do.call("rbind",sapply(1:length(content),function(i){
		translate(content[[i]])
		}))
	#return, InChI, chemify name, and query
	return(data.frame(id=unlist(res$result),name=unlist(res$query), start.name=name))	
}

#test
{
#name to InChI
name<-fixlc(ids)
id<-NametoInchI(name)

trans<-multi.CTSgetR(fixlc(id$id),from="InChIKey", to=c("KEGG","PubChem CID"))
#translate InChI to KEGG/CID

#use modified chemify query to get KEGG/CID
KID<-NametoKEGG(id$name)
CID<-NametoPubChem(name=id$name,ID="cids",limit=T)

res<-data.frame(KID,CID)
}
