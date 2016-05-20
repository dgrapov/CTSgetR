#convert KEGG id to chemical name
KEGGtoName<-function(id,all.values=FALSE,progress=TRUE){

	#should vectorize
	if(progress){pb = txtProgressBar(min = 0, max = length(id), style = 3)}
	results<-do.call("rbind",lapply(1:length(id),function(i)
	{
		if(progress){setTxtProgressBar(pb, i)}
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
	
	transl<-translate.index(id=as.matrix(fixlc(matched2$id)), lookup=rbind(matched[,2:1],c("error","error")))
	res<-data.frame(name=matched2$name,KID=transl)
	res<-res[order(fixlc(res$name)),]
	#match the initial name order
	init<-data.frame(id=1:length(name),name=fixlc(name))
	init<-init[order(init$name),]
	back<-order(init$id)
	return(res[back,])	
}

#CID to Name (Should make general PubChemtoName and sid support)
CIDtoName<-function(cid){

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
		#duplicates cause a nightmare
		dupes<-sapply(split(name,factor(name,levels=name[!duplicated(name)])),length)
		bigl<-split(res,factor(res[,1], levels=name[!duplicated(name)])) #need to make sure the levels match the name
		res<-do.call("rbind",lapply(1:length(bigl),function(i){
				tmp<-bigl[[i]]
				matrix(unlist(tmp[order(fixln(tmp[,2]),decreasing=F),][1,]),nrow=dupes[i],ncol=2,byrow=TRUE)# pick smallest cid, account for duplicates
			
		}))
		res<-data.frame(res)
		colnames(res)<-c("name","id")
		# duplicates cause problems again, need to sort to original order
		init<-data.frame(id=1:length(name),name=fixlc(name))
		init<-init[order(init$name),]
		back<-order(init$id)
		res<-res[back,]	
	}
	#remove error description for consistency
	res$id<-gsub("Status: 404","error",fixlc(res[,2]))
	return(res)
}

#search IDEOM for matches against synonym, or CID for KEGG (removes duplicates!)
# very slow for synonyms should be made parallel
getIDEOM<-function(id,from,to,word.dist=0,agrep.dist=.15,progress=TRUE, DB = NULL){
	#TODO add custom expansion character
	# generalize to other DBs
	# speed up
	
	#avoid duplicates<
	id<-make.unique(id)
	

	if(is.null(DB)){
			DB<-IDEOMgetR()
		} 
	key<-DB[,sapply(1:length(from),function(i){agrep(from[i],colnames(DB))}),drop=FALSE]
	#expand synonyms
	if(any(colnames(key)%in%"Synonyms")){
		if(progress){message(cat("Expanding Synonyms...\n"))}
		obj<-strsplit(fixlc(key[,colnames(key)%in%"Synonyms"]),"_")
		exp<-do.call("rbind",lapply(1:length(obj),function(i){unlist(obj[[i]])}))	
		key<-cbind(key,exp)
		key<-key[,!colnames(key)%in%"Synonyms"]
	} 
	if(progress){message(cat("Matching Database...\n"))}
	hits<-lapply(1:ncol(key),function(i){
		#wide agrep first then test word distance
		obj<-fixlc(key[,i])

		res<-lapply(1:length(id),function(i){
			if(!is.na(id[i])&!id[i]==""){
				hits<-agrep(id[i],obj,max.distance=agrep.dist,ignore.case = TRUE)# limit to exact matches here
			} else {hits<-NULL}	
			if(length(hits)==0){
				as.matrix(data.frame(start.name=id[i],match.name="nothing", hits=(length(obj)+1),distance=1e6),nrow=1)
			} else {
				dist<-levenshtein.damerau.distance(id[i], obj[hits])
				as.matrix(data.frame(start.name=id[i],match.name=names(dist), hits=hits,distance=dist)[which.min(dist),] )# first top hit maybe be wrong
			}
		})
	
	})
	res<-data.frame(matrix(unlist(hits),ncol=4,byrow=T))
	
	#extract top hit for each term (no duplicates else see nightmare code in "NametoPubChem")
	bigl<-split(res,factor(res[,1],levels=id))	
	#filter and take best hits
	filter.res<-do.call("rbind",lapply(1:length(bigl),function(i){
		tmp<-bigl[[i]]
		tmp[which.min(tmp[,4]),]
	}))
	
	#limit word distance
	filter.res[fixln(filter.res[,4])>word.dist,3]<-(nrow(key)+1) 
	#match return columns
	ret<-rbind(as.matrix(data.frame(DB[,unlist(sapply(1:length(to),function(i){agrep(to[i],colnames(DB))})),drop=FALSE])),rep("error",ncol(DB)))
	return(data.frame(start.name=id,match.name=filter.res[,2],ret[fixln(filter.res[,3]),,drop=FALSE]))
	}

#check synonym via chemify http://cts.fiehnlab.ucdavis.edu/chemify/rest
NametoInchI<-function(name,progress=TRUE){


	url<-paste0("http://cts.fiehnlab.ucdavis.edu/chemify/rest/identify/",gsub("/","%2F",gsub(" ","%20",name)))
	if(progress){;message(cat("Getting Keys from CTS \n"));pb = txtProgressBar(min = 0, max = length(url), style = 3)}
	content<-lapply(1:length(url),function(i){
		if(progress){setTxtProgressBar(pb, i)}
		tryCatch(getURL(url[i], ssl.verifypeer = FALSE),error=function(e){character("error")}) # bad catch error
	})
	
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
	if(progress){pb = txtProgressBar(min = 0, max = length(content), style = 3)}
	res<-do.call("rbind",sapply(1:length(content),function(i){
		if(progress){setTxtProgressBar(pb, i)}
		out<-translate(obj=content[[i]])
		if(length(out[[1]])==0){"error"} else {out} # 
		}))
	
	#return, InChI, chemify name, and query
	if(progress){message(cat("\n"))}
	return(data.frame(id=fixlc(res$result),name=fixlc(res$query), start.name=name))	
}

#get InchIKey, KEGG and CID from CTS, KEGG and PubChem form name
NametoKeyID<-function(name){
	#get InchIKey, KEGG and CID from CTS, KEGG and PubChem
	# fill in missing using InChI to key translations via CTS
	# replace KEGG drug with compound
	message(cat("Getting InChIKey \n"))
	keys<-NametoInchI(name)
	keyid<-fixlc(keys$id)
	res<-as.matrix(multi.CTSgetR(id=keyid,from="InChIKey", to=c("KEGG","PubChem CID")))

	#try to get missing IDs for missing InChIKeys
	#KEGG
	get.kegg<-c(1:nrow(res))[res[,"KEGG"]=="error"]
	if(length(get.kegg)>0){
		message(cat("Getting KEGG ID \n"))
		kegg2<-NametoKEGG(name=name[get.kegg])
		res[get.kegg,"KEGG"]<-fixlc(kegg2$KID)
	}
	#CID
	get.cid<-c(1:nrow(res))[res[,"PubChem CID"]=="error"]
	if(length(get.cid)>0){
			message(cat("Getting PubChem CID \n"))
		cid2<-NametoPubChem(name=name[get.cid],ID="cids", limit=TRUE)
		res[get.cid,"PubChem CID"]<-fixlc(cid2$id)
	} 
	
	#try to switch KEGG DRUG IDs to KIDs
	is.D<-c(1:nrow(res))[grep("D",fixlc(res[,"KEGG"]))]
	if(length(is.D)>0){
		message(cat("Translating KEGG DRUG ids \n"))
		kegg3<-NametoKEGG(name=name[is.D])
		res[is.D,"KEGG"]<-fixlc(kegg3$KID)
	}	
	
	return(data.frame(start.name=keys$start.name,matched.name=keys$name,res))
}

test<-function(){

NametoInchI(name=c("glucose","trihydroxypyrazine","poop"))



}

