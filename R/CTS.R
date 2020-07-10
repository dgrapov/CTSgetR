<<<<<<< HEAD
#' @title CTSgetR
#' @param id a vector of metabolite identifier(s) or name(s) to translate see \code{\link{valid_to}} and code{\link{valid_from}}
#' @param from Database name describing \code{id} see \code{\link{valid_from}}
#' @param to Database name to translate \code{id} see \code{\link{valid_to}}
#' @param db_name string path for sqlite database to store cached results in see \code{\link{valid_to}}
#' @return data frame of results including \code{from} (fromIdentifier), \code{id} (searchTerm), \code{to} (toIdentifier) and translated values
#' @details Interface to CTS (http://cts.fiehnlab.ucdavis.edu/) for metabolite identifier translation between
#'  > 200 of the most common biological databases including: Chemical Name, InChIKey, PubChem CID, 
#'  ChemSpider, BioCyc, ChEBI, CAS, HMDB, KEGG and LipidMAPS.
#' @seealso  \code{\link{valid_to}} \code{\link{valid_from}} \code{\link{name_to_inchikey}}
#' @export
#' @import httr dplyr
#' @examples
#' \dontrun{
#' id<-c("C15973","C00026","C05381")
#' from<-"KEGG"
#' to<-"PubChem CID"
#' CTSgetR(id,from,to)
#' }
CTSgetR <- function(id, from, to, db_name = NULL) {
  
  
  if (any(!to %in% valid_to()) | any(!from %in% valid_from())) {
    stop(
      paste0(
        "The supplied to: ",
        to,
        " or from: ",
        from,
        " are not in the list of available options.\nSee valid_to() and valid_from() for options.",
        "\n",
        collapse = ""
      )
    )
    
  }
  
  if (from == 'Chemical Name') {
    #name --> to inchikey --> to
    id <- tolower(id)
    ikeys <- CTSgetR_query(id, from, to = 'InChIKey', db_name)
    
    keys <- CTSgetR_query(ikeys$key, from = 'InChIKey', to, db_name)
    
    out <-
      left_join(ikeys,
                keys,
                by = c('key' = 'id'),
                suffix = c("", ".y"))
    
    out <- out %>% select(id, from, to.y, key.y) %>%
      setNames(., c('id', 'from', 'to', 'key'))
    
    #fill in NA in from to
    out$from[is.na(out$from)] <- na.omit(out$from)[1]
    out$to[is.na(out$to)] <- na.omit(out$to)[1]
    
  } else {
    out <- CTSgetR_query(id, from, to, db_name)
    
  }
  
  #format
  
  return(out)
  
}
=======
CTSgetR<-function(id,from,to,async=FALSE,limit.values=TRUE,progress=TRUE,server="http://cts.fiehnlab.ucdavis.edu/service/convert",...){ 

	opts<-CTS.options()
	if(!to%in%opts|!from%in%opts) {
	
	stop(paste0("The supplied to or from name is not in the list of available options.","\n",
	"Did you mean from = '", opts[agrep(from,opts,ignore.case = TRUE)],"' and to = '",paste(opts[agrep(to,opts,ignore.case = TRUE)],collapse=","),"'?", "\n",
	"See CTS.options() for all available options.","\n"))
	}

	
	
	if(progress) cat("Getting translations...","\n")
	if(async){
		out<-unlist(CTS.translate.async(server=server,from=from,to=to,id=id,...))
		out<-lapply(1:length(out),function(i){out[i]})
	} else{
		out<-CTS.translate(server=server,from=from,to=to,id=id,progress=progress) 
	}
	
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
		url<-paste(server,curlEscape(from),curlEscape(to),curlEscape(id),sep="/") # seperate encoding because can have forward slash in args
		if(progress) pb <- txtProgressBar(min = 0, max = length(id), style = 3)
		content<-lapply(1:length(id), function(i)
			{
				if(progress) setTxtProgressBar(pb, i)
				readLines(url[i])
			})
		if(progress) close(pb)
		return(content)
}

#asynchronous, need to debug
CTS.translate.async<-function(server,from,to,id,async.limit=100,...){ 
		require("RCurl")
		# results are returned as JSON encoded strings
		# limit controls the maximum number of request per call to the server
		id<-as.character(unlist(id))
		url<-paste(server,curlEscape(from),curlEscape(to),curlEscape(id),sep="/") # seperate encoding because can have forward slash in args
	
		options(RCurlOptions = list(verbose = TRUE,
                              followlocation = TRUE,
                              autoreferer = TRUE,
                              nosignal = TRUE))
		curl = getCurlHandle()	
		
		if(is.null(async.limit)){
			getURL(url,  ssl.verifypeer = FALSE, useragent = "R", timeout=10, curl = curl) #not stable missing some args
		} else {
			if(async.limit>=length(url)){async.limit<-length(url)-1}
			cuts<-cut(1:length(url),breaks=seq(1,length(url),length.out=ceiling(length(url)/async.limit)+1),include.lowest = TRUE)
			url.list<-split(url,cuts)
			lapply(1:length(url.list), function(i) {
				tmp.url<-url.list[[i]]
				getURL(tmp.url,  ssl.verifypeer = FALSE, useragent = "R", timeout=10, curl = curl) # unstable for unclear reasons
			} )
		}

>>>>>>> 9d36fd0bcb45c1b4e8b5a1667b8d960acb844d59

# #convert name to inchikey and get top score
#' @export
#' @param chemical names as strings or a vector
#' @param algorithm string one of  'biological' or 'popularity' defaults to 'biological' see https://cts.fiehnlab.ucdavis.edu/services
#' @return inchikeys and scores or NA if absent for each chemical name
name_to_inchikey<-function(name,algorithm='biological'){
  
  f<-function(x){
    
    x %>%
      tolower() %>%
      URLencode(.) %>%
      paste0(
        'https://cts.fiehnlab.ucdavis.edu/rest/score/Chemical%20Name/',
        .,
        '/',
        algorithm
      )
    
  }
  
  url <- name %>%
    map( ~ f(.)) %>%
    unlist()
  
  
  f<-function(url){
    pb$tick()$print()
    GET(url) %>% content()
  }
  
  pb <- progress_estimated(length(url))
  
  res<- url %>%
    map(~ f(.))
  
  #extract scores
  score_inchikey<-function(req){
    
    f<-function(x){
      data.frame(name=null_replace(x$searchTerm),
                 inchikey=null_replace(x$result[[1]]$InChIKey),
                 score=null_replace(x$result[[1]]$score))
    }
    
    req %>%
      map(~ f(.)) %>%
      do.call('rbind',.)
    
  }
  
  score<-score_inchikey(res)
  
  #convert to expected format
  data.frame(from='Chemical Name', to='InChIKey', id=score$name, key=score$inchikey) 
  
}

#convert inchi to id
get_translation<-function(source_id,source,target){
  
  
  f<-function(x,source,target){
    
    x %>%
      paste(
        'https://cts.fiehnlab.ucdavis.edu/rest/convert',
        source,
        target,
        .,
        sep='/') %>%
      map(~URLencode(.)) %>%
      unlist()
    
  }
  
  url <- source_id %>%
    map( ~ f(.,source,target))
  
  
  f<-function(url){
    pb$tick()$print()
    GET(url) %>% content()
  }
  
  pb <- progress_estimated(length(source_id))
  res<- url %>%
    map(~ f(.))
  
  
  parse_response<-function(x){
    
    tmp<-x %>%
      flatten(.)
    
    if(length(tmp$results)>0){
      tmp$results <-unlist(flatten(tmp$results))[1]
    }
    
    data.frame(
      id = null_replace(tmp$searchTerm),
      from = null_replace(tmp$fromIdentifier),
      to = null_replace(tmp$toIdentifier),
      key = null_replace(tmp$results)
    )
    
  }
  
  res %>%
    map(~parse_response(.)) %>%
    do.call('rbind',.) %>%
    mutate(id = id) # to returned normalized results in upon error
}

CTSgetR_query <-
  function(id, from, to, db_name = NULL) {
    if (!is.null(db_name)) {
      in_db <- db_get(id, from, to, db_name)
      
      have <- id[id %in% in_db$id]
      need <- id[!id %in% in_db$id]
    } else {
      have <- character()
      need <- idid
    }
    
    
    
    #need to retrieve data from API
    if (length(need) > 0) {
      if (from == 'Chemical Name') {
        api_res <- name_to_inchikey(need)
      } else {
        api_res <- get_translation(need, from, to)
      }
      
      
      #add to db
      tmp <- api_res %>%
        db_transform(.) %>%
        .$data %>%
        mutate(key_index = db_key(.))
      #%>% na.omit()
      
      if (!is.null(db_name)) {
        if (nrow(tmp) > 0) {
          db_add(tmp$source_id,
                 tmp$source,
                 tmp$target,
                 tmp$target_id,
                 db_name)
        }
      }
      
      if (nrow(in_db) > 0) {
        in_db <- rbind(in_db, api_res)
      } else {
        in_db <- api_res
      }
      
      in_db <- in_db[as.character(in_db$id) %in% id,]
      
    }
    
    return(in_db)
    
  }

#' @export
#' @param update logical (default FALSE) if live or cached results should be returned
valid_from <- function(update = FALSE) {
  if (update) {
    url <- 'https://cts.fiehnlab.ucdavis.edu/rest/fromValues'
    GET(url) %>% content()
    #these should be saved to the package
  } else {
    list(
      "BioCyc",
      "CAS",
      "ChEBI",
      "Chemical Name",
      "Human Metabolome Database",
      "InChIKey",
      "KEGG",
      "LMSD",
      "LipidMAPS",
      "PubChem CID",
      "Pubchem SID",
      "ChemSpider",
      "ChemDB",
      "ChEMBL",
      "ChemBank",
      "ZINC",
      "Broad Institute",
      "The Scripps Research Institute Molecular Screening Center",
      "DrugBank",
      "MMDB",
      "ChemMol",
      "NIAID",
      "Southern Research Institute",
      "NIH Clinical Collection",
      "Comparative Toxicogenomics Database",
      "MolPort",
      "SMID",
      "ChemBridge",
      "GlaxoSmithKline (GSK)",
      "NINDS Approved Drug Screening Program",
      "NIST",
      "SCRIPDB",
      "Southern Research Specialized Biocontainment Screening Center",
      "TimTec",
      "Tox21",
      "Web of Science",
      "ABI Chem",
      "Center for Chemical Genomics, University of Michigan",
      "ChemIDplus",
      "Sigma-Aldrich",
      "ASINEX",
      "ChemBlock",
      "DiscoveryGate",
      "Specs",
      "Ambinter",
      "Emory University Molecular Libraries Screening Center",
      "MLSMR",
      "Vitas-M Laboratory",
      "AAA Chemistry",
      "ChemFrog",
      "Isoprenoids",
      "NCGC",
      "Acesobio",
      "ALDRICH",
      "Ambit Biosciences",
      "CLRI (CSIR)",
      "InFarmatik",
      "IS Chemical Technology",
      "MOLI",
      "SIGMA",
      "Tetrahedron Scientific Inc",
      "Thomson Pharma",
      "ABBLIS Chemicals",
      "Abbott Labs",
      "AbMole Bioscience",
      "Achemica",
      "Acorn PharmaTech",
      "Active Biopharma",
      "Adooq BioScience",
      "AKos Consulting & Solutions",
      "AK Scientific, Inc. (AKSCI)",
      "Alagar Yadav, Karpagam University",
      "Alinda Chemical",
      "Alsachim",
      "Amadis Chemical",
      "Amatye",
      "AmicBase - Antimicrobial Activities",
      "Angene Chemical",
      "Angene International",
      "Ark Pharm, Inc.",
      "BioChemPartner",
      "BroadPharm",
      "Chembase.cn",
      "Chembo",
      "chemicalize.org by ChemAxon",
      "Fragmenta",
      "Anitha, Department of Bioinformatics, Karpagam University",
      "Annker Organics",
      "Anward",
      "Apeiron Synthesis",
      "ApexBio Technology",
      "Apexmol",
      "Aromsyn catalogue",
      "Aronis",
      "Aurora Fine Chemicals LLC",
      "Aurum Pharmatech LLC",
      "Avanti Polar Lipids",
      "Beijing Advanced Technology Co, Ltd",
      "Bertin Pharma",
      "Bhaskar Lab, Department of Zoology, Sri Venkateswara University",
      "BIDD",
      "BIND",
      "BindingDB",
      "Biological Magnetic Resonance Data Bank (BMRB)",
      "Bioprocess Technology Lab, Department of Microbiology, Bharathidasan University",
      "Biosynth",
      "Burnham Center for Chemical Genomics",
      "Calbiochem",
      "Cambridge Crystallographic Data Centre",
      "CAPOT",
      "Cayman Chemical",
      "CC_PMLSC",
      "ChemExper Chemical Directory",
      "Chemical Biology Department, Max Planck Institute of Molecular Physiology",
      "ChemScene",
      "ChemSynthesis",
      "ChemTik",
      "Chiralblock Biosciences",
      "Circadian Research, Kay Laboratory, University of California at San Diego (UCSD)",
      "CMLD-BU",
      "Columbia University Molecular Screening Center",
      "Creasyn Finechem",
      "Department of Pharmacy, LMU",
      "DTP/NCI",
      "EDASA Scientific Compounds June 2013",
      "EMD Biosciences",
      "Enamine",
      "Ennopharm",
      "EPA DSSTox",
      "Excenen Pharmatech",
      "Exchemistry",
      "FINETECH",
      "Finley and King Labs, Harvard Medical School",
      "FLUKA",
      "ForeChem",
      "Georganics",
      "GLIDA, GPCR-Ligand Database",
      "GNF / Scripps Winzeler lab",
      "Golm Metabolome Database (GMD), Max Planck Institute of Molecular Plant Physiology",
      "Hangzhou APIChem Technology",
      "Hangzhou Trylead Chemical Technology",
      "HDH Pharma",
      "HUMGENEX",
      "IBCH RAS",
      "IBM",
      "ICCB-Longwood/NSRB Screening Facility, Harvard Medical School",
      "Immunology Lab, Department of Biotechnology, Calicut University",
      "Inhibitor 2",
      "Insect Molecular Biology Lab, Department of Environmental Biotechnology, Bharathidasan University",
      "iThemba Pharmaceuticals",
      "IUPHAR-DB",
      "Jamson Pharmachem Technology",
      "Japan Chemical Substance Dictionary (Nikkaji)",
      "Johns Hopkins Ion Channel Center",
      "Kingston Chemistry",
      "KUMGM",
      "LeadScope",
      "MedChemexpress MCE",
      "MICAD",
      "MIC Scientific",
      "Milwaukee Institute for Drug Discovery",
      "Molecular Libraries Program, Specialized Chemistry Center, University of Kansas",
      "MP Biomedicals",
      "MTDP",
      "Nanjing Pharmaceutical Factory",
      "Nantong Baihua Bio-Pharmaceutical Co., Ltd",
      "Nature Chemical Biology",
      "Nature Chemistry",
      "Nature Communications",
      "NIST Chemistry WebBook",
      "Nitric Oxide Research, National Cancer Institute (NCI)",
      "NMMLSC",
      "NMRShiftDB",
      "NovoSeek",
      "Oakwood Products",
      "ORST SMALL MOLECULE SCREENING CENTER",
      "P3 BioSystems",
      "PANACHE",
      "Paul Baures",
      "PCMD",
      "PDSP",
      "PENN-ABS",
      "PennChem-GAM",
      "PFC",
      "P.Ravikumar, M.Jeyam and G.Shalini. Biochematics Division, Bharathiar University",
      "priyadharshini sabarathinam angayarkanni murugesh palaniswamy",
      "Prous Science Drugs of the Future",
      "Rangan Lab, Department of Biotechnology, IIT Guwahati",
      "R&D Chemicals",
      "R.Sathishkumar, Phytomatics Laboratory, Department of Bioinformatics, Bharathiar University",
      "RSChem, LLC",
      "SASTRA University, Quorum sensing and Peptidomimetics Laboratory",
      "Selleckbio",
      "Selleck Chemicals",
      "SGCOxCompounds",
      "SGCStoCompounds",
      "S.GURUDEEBAN, T.RAMANATHAN & K.SATYAVANI, Marine Medicinal Plant Biotechnology Laboratory, Faculty of Marine Sciences, Annamalai Universtiy",
      "Shanghai Institute of Organic Chemistry",
      "Shanghai Sinofluoro Scientific Company",
      "SLING Consortium",
      "SRMLSC",
      "Structural Genomics Consortium",
      "SureChem",
      "SYNCHEM OHG",
      "Syntechem",
      "TCI (Tokyo Chemical Industry)",
      "ten Dijke Lab, Leiden University Medical Center",
      "Therapeutic Targets Database",
      "Total TOSLab Building-Blocks",
      "True PharmaChem",
      "Tyger Scientific",
      "UCLA Molecular Screening Shared Resource",
      "UM-BBD",
      "UniCarbKB",
      "University of Pittsburgh Molecular Library Screening Center",
      "UPCMLD",
      "Vanderbilt Screening Center for GPCRs, Ion Channels and Transporters",
      "Vanderbilt Specialized Chemistry Center",
      "Vanderbilt University Medical Center",
      "VIT University",
      "Watec Laboratories",
      "Watson International Ltd",
      "xPharm",
      "Zancheng Functional Chemicals",
      "zealing chemical"
    )
  }
}

#to values
#' @export
#' @param update logical (default FALSE) if live or cached results should be returned
valid_to <- function(update = FALSE) {
  if (update) {
    url <- 'https://cts.fiehnlab.ucdavis.edu/rest/toValues'
    GET(url) %>% content()
    
  } else {
    list(
      "BioCyc",
      "CAS",
      "ChEBI",
      "Chemical Name",
      "Human Metabolome Database",
      "InChI Code",
      "InChIKey",
      "KEGG",
      "LMSD",
      "LipidMAPS",
      "PubChem CID",
      "Pubchem SID",
      "ChemSpider",
      "ChemDB",
      "ChEMBL",
      "ChemBank",
      "ZINC",
      "Broad Institute",
      "The Scripps Research Institute Molecular Screening Center",
      "DrugBank",
      "MMDB",
      "ChemMol",
      "NIAID",
      "Southern Research Institute",
      "NIH Clinical Collection",
      "Comparative Toxicogenomics Database",
      "MolPort",
      "SMID",
      "ChemBridge",
      "GlaxoSmithKline (GSK)",
      "NINDS Approved Drug Screening Program",
      "NIST",
      "SCRIPDB",
      "Southern Research Specialized Biocontainment Screening Center",
      "TimTec",
      "Tox21",
      "Web of Science",
      "ABI Chem",
      "Center for Chemical Genomics, University of Michigan",
      "ChemIDplus",
      "Sigma-Aldrich",
      "ASINEX",
      "ChemBlock",
      "DiscoveryGate",
      "Specs",
      "Ambinter",
      "Emory University Molecular Libraries Screening Center",
      "MLSMR",
      "Vitas-M Laboratory",
      "AAA Chemistry",
      "ChemFrog",
      "Isoprenoids",
      "NCGC",
      "Acesobio",
      "ALDRICH",
      "Ambit Biosciences",
      "CLRI (CSIR)",
      "InFarmatik",
      "IS Chemical Technology",
      "MOLI",
      "SIGMA",
      "Tetrahedron Scientific Inc",
      "Thomson Pharma",
      "ABBLIS Chemicals",
      "Abbott Labs",
      "AbMole Bioscience",
      "Achemica",
      "Acorn PharmaTech",
      "Active Biopharma",
      "Adooq BioScience",
      "AKos Consulting & Solutions",
      "AK Scientific, Inc. (AKSCI)",
      "Alagar Yadav, Karpagam University",
      "Alinda Chemical",
      "Alsachim",
      "Amadis Chemical",
      "Amatye",
      "AmicBase - Antimicrobial Activities",
      "Angene Chemical",
      "Angene International",
      "Ark Pharm, Inc.",
      "BioChemPartner",
      "BroadPharm",
      "Chembase.cn",
      "Chembo",
      "chemicalize.org by ChemAxon",
      "Fragmenta",
      "Anitha, Department of Bioinformatics, Karpagam University",
      "Annker Organics",
      "Anward",
      "Apeiron Synthesis",
      "ApexBio Technology",
      "Apexmol",
      "Aromsyn catalogue",
      "Aronis",
      "Aurora Fine Chemicals LLC",
      "Aurum Pharmatech LLC",
      "Avanti Polar Lipids",
      "Beijing Advanced Technology Co, Ltd",
      "Bertin Pharma",
      "Bhaskar Lab, Department of Zoology, Sri Venkateswara University",
      "BIDD",
      "BIND",
      "BindingDB",
      "Biological Magnetic Resonance Data Bank (BMRB)",
      "Bioprocess Technology Lab, Department of Microbiology, Bharathidasan University",
      "Biosynth",
      "Burnham Center for Chemical Genomics",
      "Calbiochem",
      "Cambridge Crystallographic Data Centre",
      "CAPOT",
      "Cayman Chemical",
      "CC_PMLSC",
      "ChemExper Chemical Directory",
      "Chemical Biology Department, Max Planck Institute of Molecular Physiology",
      "ChemScene",
      "ChemSynthesis",
      "ChemTik",
      "Chiralblock Biosciences",
      "Circadian Research, Kay Laboratory, University of California at San Diego (UCSD)",
      "CMLD-BU",
      "Columbia University Molecular Screening Center",
      "Creasyn Finechem",
      "Department of Pharmacy, LMU",
      "DTP/NCI",
      "EDASA Scientific Compounds June 2013",
      "EMD Biosciences",
      "Enamine",
      "Ennopharm",
      "EPA DSSTox",
      "Excenen Pharmatech",
      "Exchemistry",
      "FINETECH",
      "Finley and King Labs, Harvard Medical School",
      "FLUKA",
      "ForeChem",
      "Georganics",
      "GLIDA, GPCR-Ligand Database",
      "GNF / Scripps Winzeler lab",
      "Golm Metabolome Database (GMD), Max Planck Institute of Molecular Plant Physiology",
      "Hangzhou APIChem Technology",
      "Hangzhou Trylead Chemical Technology",
      "HDH Pharma",
      "HUMGENEX",
      "IBCH RAS",
      "IBM",
      "ICCB-Longwood/NSRB Screening Facility, Harvard Medical School",
      "Immunology Lab, Department of Biotechnology, Calicut University",
      "Inhibitor 2",
      "Insect Molecular Biology Lab, Department of Environmental Biotechnology, Bharathidasan University",
      "iThemba Pharmaceuticals",
      "IUPHAR-DB",
      "Jamson Pharmachem Technology",
      "Japan Chemical Substance Dictionary (Nikkaji)",
      "Johns Hopkins Ion Channel Center",
      "Kingston Chemistry",
      "KUMGM",
      "LeadScope",
      "MedChemexpress MCE",
      "MICAD",
      "MIC Scientific",
      "Milwaukee Institute for Drug Discovery",
      "Molecular Libraries Program, Specialized Chemistry Center, University of Kansas",
      "MP Biomedicals",
      "MTDP",
      "Nanjing Pharmaceutical Factory",
      "Nantong Baihua Bio-Pharmaceutical Co., Ltd",
      "Nature Chemical Biology",
      "Nature Chemistry",
      "Nature Communications",
      "NIST Chemistry WebBook",
      "Nitric Oxide Research, National Cancer Institute (NCI)",
      "NMMLSC",
      "NMRShiftDB",
      "NovoSeek",
      "Oakwood Products",
      "ORST SMALL MOLECULE SCREENING CENTER",
      "P3 BioSystems",
      "PANACHE",
      "Paul Baures",
      "PCMD",
      "PDSP",
      "PENN-ABS",
      "PennChem-GAM",
      "PFC",
      "P.Ravikumar, M.Jeyam and G.Shalini. Biochematics Division, Bharathiar University",
      "priyadharshini sabarathinam angayarkanni murugesh palaniswamy",
      "Prous Science Drugs of the Future",
      "Rangan Lab, Department of Biotechnology, IIT Guwahati",
      "R&D Chemicals",
      "R.Sathishkumar, Phytomatics Laboratory, Department of Bioinformatics, Bharathiar University",
      "RSChem, LLC",
      "SASTRA University, Quorum sensing and Peptidomimetics Laboratory",
      "Selleckbio",
      "Selleck Chemicals",
      "SGCOxCompounds",
      "SGCStoCompounds",
      "S.GURUDEEBAN, T.RAMANATHAN & K.SATYAVANI, Marine Medicinal Plant Biotechnology Laboratory, Faculty of Marine Sciences, Annamalai Universtiy",
      "Shanghai Institute of Organic Chemistry",
      "Shanghai Sinofluoro Scientific Company",
      "SLING Consortium",
      "SRMLSC",
      "Structural Genomics Consortium",
      "SureChem",
      "SYNCHEM OHG",
      "Syntechem",
      "TCI (Tokyo Chemical Industry)",
      "ten Dijke Lab, Leiden University Medical Center",
      "Therapeutic Targets Database",
      "Total TOSLab Building-Blocks",
      "True PharmaChem",
      "Tyger Scientific",
      "UCLA Molecular Screening Shared Resource",
      "UM-BBD",
      "UniCarbKB",
      "University of Pittsburgh Molecular Library Screening Center",
      "UPCMLD",
      "Vanderbilt Screening Center for GPCRs, Ion Channels and Transporters",
      "Vanderbilt Specialized Chemistry Center",
      "Vanderbilt University Medical Center",
      "VIT University",
      "Watec Laboratories",
      "Watson International Ltd",
      "xPharm",
      "Zancheng Functional Chemicals",
      "zealing chemical"
    )
    
  }
}