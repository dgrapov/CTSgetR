library(CTSgetR)
library(dplyr)
library(httr)
library(purrr)
library(RSQLite)

# #from values
# valid_from<-function(update=FALSE){
#   
#   if(update){
#     url<-'https://cts.fiehnlab.ucdavis.edu/rest/fromValues'
#     GET(url) %>% content()
#     #these should be saved to the package
#   } else {
#     
#     list("BioCyc", "CAS", "ChEBI", "Chemical Name", "Human Metabolome Database", 
#          "InChIKey", "KEGG", "LMSD", "LipidMAPS", "PubChem CID", "Pubchem SID", 
#          "ChemSpider", "ChemDB", "ChEMBL", "ChemBank", "ZINC", "Broad Institute", 
#          "The Scripps Research Institute Molecular Screening Center", 
#          "DrugBank", "MMDB", "ChemMol", "NIAID", "Southern Research Institute", 
#          "NIH Clinical Collection", "Comparative Toxicogenomics Database", 
#          "MolPort", "SMID", "ChemBridge", "GlaxoSmithKline (GSK)", 
#          "NINDS Approved Drug Screening Program", "NIST", "SCRIPDB", 
#          "Southern Research Specialized Biocontainment Screening Center", 
#          "TimTec", "Tox21", "Web of Science", "ABI Chem", "Center for Chemical Genomics, University of Michigan", 
#          "ChemIDplus", "Sigma-Aldrich", "ASINEX", "ChemBlock", "DiscoveryGate", 
#          "Specs", "Ambinter", "Emory University Molecular Libraries Screening Center", 
#          "MLSMR", "Vitas-M Laboratory", "AAA Chemistry", "ChemFrog", 
#          "Isoprenoids", "NCGC", "Acesobio", "ALDRICH", "Ambit Biosciences", 
#          "CLRI (CSIR)", "InFarmatik", "IS Chemical Technology", "MOLI", 
#          "SIGMA", "Tetrahedron Scientific Inc", "Thomson Pharma", 
#          "ABBLIS Chemicals", "Abbott Labs", "AbMole Bioscience", "Achemica", 
#          "Acorn PharmaTech", "Active Biopharma", "Adooq BioScience", 
#          "AKos Consulting & Solutions", "AK Scientific, Inc. (AKSCI)", 
#          "Alagar Yadav, Karpagam University", "Alinda Chemical", "Alsachim", 
#          "Amadis Chemical", "Amatye", "AmicBase - Antimicrobial Activities", 
#          "Angene Chemical", "Angene International", "Ark Pharm, Inc.", 
#          "BioChemPartner", "BroadPharm", "Chembase.cn", "Chembo", 
#          "chemicalize.org by ChemAxon", "Fragmenta", "Anitha, Department of Bioinformatics, Karpagam University", 
#          "Annker Organics", "Anward", "Apeiron Synthesis", "ApexBio Technology", 
#          "Apexmol", "Aromsyn catalogue", "Aronis", "Aurora Fine Chemicals LLC", 
#          "Aurum Pharmatech LLC", "Avanti Polar Lipids", "Beijing Advanced Technology Co, Ltd", 
#          "Bertin Pharma", "Bhaskar Lab, Department of Zoology, Sri Venkateswara University", 
#          "BIDD", "BIND", "BindingDB", "Biological Magnetic Resonance Data Bank (BMRB)", 
#          "Bioprocess Technology Lab, Department of Microbiology, Bharathidasan University", 
#          "Biosynth", "Burnham Center for Chemical Genomics", "Calbiochem", 
#          "Cambridge Crystallographic Data Centre", "CAPOT", "Cayman Chemical", 
#          "CC_PMLSC", "ChemExper Chemical Directory", "Chemical Biology Department, Max Planck Institute of Molecular Physiology", 
#          "ChemScene", "ChemSynthesis", "ChemTik", "Chiralblock Biosciences", 
#          "Circadian Research, Kay Laboratory, University of California at San Diego (UCSD)", 
#          "CMLD-BU", "Columbia University Molecular Screening Center", 
#          "Creasyn Finechem", "Department of Pharmacy, LMU", "DTP/NCI", 
#          "EDASA Scientific Compounds June 2013", "EMD Biosciences", 
#          "Enamine", "Ennopharm", "EPA DSSTox", "Excenen Pharmatech", 
#          "Exchemistry", "FINETECH", "Finley and King Labs, Harvard Medical School", 
#          "FLUKA", "ForeChem", "Georganics", "GLIDA, GPCR-Ligand Database", 
#          "GNF / Scripps Winzeler lab", "Golm Metabolome Database (GMD), Max Planck Institute of Molecular Plant Physiology", 
#          "Hangzhou APIChem Technology", "Hangzhou Trylead Chemical Technology", 
#          "HDH Pharma", "HUMGENEX", "IBCH RAS", "IBM", "ICCB-Longwood/NSRB Screening Facility, Harvard Medical School", 
#          "Immunology Lab, Department of Biotechnology, Calicut University", 
#          "Inhibitor 2", "Insect Molecular Biology Lab, Department of Environmental Biotechnology, Bharathidasan University", 
#          "iThemba Pharmaceuticals", "IUPHAR-DB", "Jamson Pharmachem Technology", 
#          "Japan Chemical Substance Dictionary (Nikkaji)", "Johns Hopkins Ion Channel Center", 
#          "Kingston Chemistry", "KUMGM", "LeadScope", "MedChemexpress MCE", 
#          "MICAD", "MIC Scientific", "Milwaukee Institute for Drug Discovery", 
#          "Molecular Libraries Program, Specialized Chemistry Center, University of Kansas", 
#          "MP Biomedicals", "MTDP", "Nanjing Pharmaceutical Factory", 
#          "Nantong Baihua Bio-Pharmaceutical Co., Ltd", "Nature Chemical Biology", 
#          "Nature Chemistry", "Nature Communications", "NIST Chemistry WebBook", 
#          "Nitric Oxide Research, National Cancer Institute (NCI)", 
#          "NMMLSC", "NMRShiftDB", "NovoSeek", "Oakwood Products", "ORST SMALL MOLECULE SCREENING CENTER", 
#          "P3 BioSystems", "PANACHE", "Paul Baures", "PCMD", "PDSP", 
#          "PENN-ABS", "PennChem-GAM", "PFC", "P.Ravikumar, M.Jeyam and G.Shalini. Biochematics Division, Bharathiar University", 
#          "priyadharshini sabarathinam angayarkanni murugesh palaniswamy", 
#          "Prous Science Drugs of the Future", "Rangan Lab, Department of Biotechnology, IIT Guwahati", 
#          "R&D Chemicals", "R.Sathishkumar, Phytomatics Laboratory, Department of Bioinformatics, Bharathiar University", 
#          "RSChem, LLC", "SASTRA University, Quorum sensing and Peptidomimetics Laboratory", 
#          "Selleckbio", "Selleck Chemicals", "SGCOxCompounds", "SGCStoCompounds", 
#          "S.GURUDEEBAN, T.RAMANATHAN & K.SATYAVANI, Marine Medicinal Plant Biotechnology Laboratory, Faculty of Marine Sciences, Annamalai Universtiy", 
#          "Shanghai Institute of Organic Chemistry", "Shanghai Sinofluoro Scientific Company", 
#          "SLING Consortium", "SRMLSC", "Structural Genomics Consortium", 
#          "SureChem", "SYNCHEM OHG", "Syntechem", "TCI (Tokyo Chemical Industry)", 
#          "ten Dijke Lab, Leiden University Medical Center", "Therapeutic Targets Database", 
#          "Total TOSLab Building-Blocks", "True PharmaChem", "Tyger Scientific", 
#          "UCLA Molecular Screening Shared Resource", "UM-BBD", "UniCarbKB", 
#          "University of Pittsburgh Molecular Library Screening Center", 
#          "UPCMLD", "Vanderbilt Screening Center for GPCRs, Ion Channels and Transporters", 
#          "Vanderbilt Specialized Chemistry Center", "Vanderbilt University Medical Center", 
#          "VIT University", "Watec Laboratories", "Watson International Ltd", 
#          "xPharm", "Zancheng Functional Chemicals", "zealing chemical")
#   }
# }
# 
# #to values
# valid_to<-function(update=FALSE){
#   
#   if(update){
#     url<-'https://cts.fiehnlab.ucdavis.edu/rest/toValues'
#     GET(url) %>% content()
#     
#   } else {
#     
#     list("BioCyc", "CAS", "ChEBI", "Chemical Name", "Human Metabolome Database", 
#          "InChI Code", "InChIKey", "KEGG", "LMSD", "LipidMAPS", "PubChem CID", 
#          "Pubchem SID", "ChemSpider", "ChemDB", "ChEMBL", "ChemBank", 
#          "ZINC", "Broad Institute", "The Scripps Research Institute Molecular Screening Center", 
#          "DrugBank", "MMDB", "ChemMol", "NIAID", "Southern Research Institute", 
#          "NIH Clinical Collection", "Comparative Toxicogenomics Database", 
#          "MolPort", "SMID", "ChemBridge", "GlaxoSmithKline (GSK)", 
#          "NINDS Approved Drug Screening Program", "NIST", "SCRIPDB", 
#          "Southern Research Specialized Biocontainment Screening Center", 
#          "TimTec", "Tox21", "Web of Science", "ABI Chem", "Center for Chemical Genomics, University of Michigan", 
#          "ChemIDplus", "Sigma-Aldrich", "ASINEX", "ChemBlock", "DiscoveryGate", 
#          "Specs", "Ambinter", "Emory University Molecular Libraries Screening Center", 
#          "MLSMR", "Vitas-M Laboratory", "AAA Chemistry", "ChemFrog", 
#          "Isoprenoids", "NCGC", "Acesobio", "ALDRICH", "Ambit Biosciences", 
#          "CLRI (CSIR)", "InFarmatik", "IS Chemical Technology", "MOLI", 
#          "SIGMA", "Tetrahedron Scientific Inc", "Thomson Pharma", 
#          "ABBLIS Chemicals", "Abbott Labs", "AbMole Bioscience", "Achemica", 
#          "Acorn PharmaTech", "Active Biopharma", "Adooq BioScience", 
#          "AKos Consulting & Solutions", "AK Scientific, Inc. (AKSCI)", 
#          "Alagar Yadav, Karpagam University", "Alinda Chemical", "Alsachim", 
#          "Amadis Chemical", "Amatye", "AmicBase - Antimicrobial Activities", 
#          "Angene Chemical", "Angene International", "Ark Pharm, Inc.", 
#          "BioChemPartner", "BroadPharm", "Chembase.cn", "Chembo", 
#          "chemicalize.org by ChemAxon", "Fragmenta", "Anitha, Department of Bioinformatics, Karpagam University", 
#          "Annker Organics", "Anward", "Apeiron Synthesis", "ApexBio Technology", 
#          "Apexmol", "Aromsyn catalogue", "Aronis", "Aurora Fine Chemicals LLC", 
#          "Aurum Pharmatech LLC", "Avanti Polar Lipids", "Beijing Advanced Technology Co, Ltd", 
#          "Bertin Pharma", "Bhaskar Lab, Department of Zoology, Sri Venkateswara University", 
#          "BIDD", "BIND", "BindingDB", "Biological Magnetic Resonance Data Bank (BMRB)", 
#          "Bioprocess Technology Lab, Department of Microbiology, Bharathidasan University", 
#          "Biosynth", "Burnham Center for Chemical Genomics", "Calbiochem", 
#          "Cambridge Crystallographic Data Centre", "CAPOT", "Cayman Chemical", 
#          "CC_PMLSC", "ChemExper Chemical Directory", "Chemical Biology Department, Max Planck Institute of Molecular Physiology", 
#          "ChemScene", "ChemSynthesis", "ChemTik", "Chiralblock Biosciences", 
#          "Circadian Research, Kay Laboratory, University of California at San Diego (UCSD)", 
#          "CMLD-BU", "Columbia University Molecular Screening Center", 
#          "Creasyn Finechem", "Department of Pharmacy, LMU", "DTP/NCI", 
#          "EDASA Scientific Compounds June 2013", "EMD Biosciences", 
#          "Enamine", "Ennopharm", "EPA DSSTox", "Excenen Pharmatech", 
#          "Exchemistry", "FINETECH", "Finley and King Labs, Harvard Medical School", 
#          "FLUKA", "ForeChem", "Georganics", "GLIDA, GPCR-Ligand Database", 
#          "GNF / Scripps Winzeler lab", "Golm Metabolome Database (GMD), Max Planck Institute of Molecular Plant Physiology", 
#          "Hangzhou APIChem Technology", "Hangzhou Trylead Chemical Technology", 
#          "HDH Pharma", "HUMGENEX", "IBCH RAS", "IBM", "ICCB-Longwood/NSRB Screening Facility, Harvard Medical School", 
#          "Immunology Lab, Department of Biotechnology, Calicut University", 
#          "Inhibitor 2", "Insect Molecular Biology Lab, Department of Environmental Biotechnology, Bharathidasan University", 
#          "iThemba Pharmaceuticals", "IUPHAR-DB", "Jamson Pharmachem Technology", 
#          "Japan Chemical Substance Dictionary (Nikkaji)", "Johns Hopkins Ion Channel Center", 
#          "Kingston Chemistry", "KUMGM", "LeadScope", "MedChemexpress MCE", 
#          "MICAD", "MIC Scientific", "Milwaukee Institute for Drug Discovery", 
#          "Molecular Libraries Program, Specialized Chemistry Center, University of Kansas", 
#          "MP Biomedicals", "MTDP", "Nanjing Pharmaceutical Factory", 
#          "Nantong Baihua Bio-Pharmaceutical Co., Ltd", "Nature Chemical Biology", 
#          "Nature Chemistry", "Nature Communications", "NIST Chemistry WebBook", 
#          "Nitric Oxide Research, National Cancer Institute (NCI)", 
#          "NMMLSC", "NMRShiftDB", "NovoSeek", "Oakwood Products", "ORST SMALL MOLECULE SCREENING CENTER", 
#          "P3 BioSystems", "PANACHE", "Paul Baures", "PCMD", "PDSP", 
#          "PENN-ABS", "PennChem-GAM", "PFC", "P.Ravikumar, M.Jeyam and G.Shalini. Biochematics Division, Bharathiar University", 
#          "priyadharshini sabarathinam angayarkanni murugesh palaniswamy", 
#          "Prous Science Drugs of the Future", "Rangan Lab, Department of Biotechnology, IIT Guwahati", 
#          "R&D Chemicals", "R.Sathishkumar, Phytomatics Laboratory, Department of Bioinformatics, Bharathiar University", 
#          "RSChem, LLC", "SASTRA University, Quorum sensing and Peptidomimetics Laboratory", 
#          "Selleckbio", "Selleck Chemicals", "SGCOxCompounds", "SGCStoCompounds", 
#          "S.GURUDEEBAN, T.RAMANATHAN & K.SATYAVANI, Marine Medicinal Plant Biotechnology Laboratory, Faculty of Marine Sciences, Annamalai Universtiy", 
#          "Shanghai Institute of Organic Chemistry", "Shanghai Sinofluoro Scientific Company", 
#          "SLING Consortium", "SRMLSC", "Structural Genomics Consortium", 
#          "SureChem", "SYNCHEM OHG", "Syntechem", "TCI (Tokyo Chemical Industry)", 
#          "ten Dijke Lab, Leiden University Medical Center", "Therapeutic Targets Database", 
#          "Total TOSLab Building-Blocks", "True PharmaChem", "Tyger Scientific", 
#          "UCLA Molecular Screening Shared Resource", "UM-BBD", "UniCarbKB", 
#          "University of Pittsburgh Molecular Library Screening Center", 
#          "UPCMLD", "Vanderbilt Screening Center for GPCRs, Ion Channels and Transporters", 
#          "Vanderbilt Specialized Chemistry Center", "Vanderbilt University Medical Center", 
#          "VIT University", "Watec Laboratories", "Watson International Ltd", 
#          "xPharm", "Zancheng Functional Chemicals", "zealing chemical")
#     
#   }
# }

# null_replace<-function(x,alt=NA){
#   if(is.null(x) || length(x) == 0 ) alt else x
# }
# 
# # #convert name to inchikey and get top score
# name_to_inchikey<-function(name,algorithm='biological'){
# 
#   f<-function(x){
# 
#     x %>%
#       tolower() %>%
#       URLencode(.) %>%
#       paste0(
#         'https://cts.fiehnlab.ucdavis.edu/rest/score/Chemical%20Name/',
#         .,
#         '/',
#         algorithm
#       )
# 
#   }
# 
#   url <- name %>%
#     map( ~ f(.)) %>%
#     unlist()
# 
# 
#   f<-function(url){
#     pb$tick()$print()
#     GET(url) %>% content()
#   }
# 
#   pb <- progress_estimated(length(url))
# 
#   res<- url %>%
#     map(~ f(.))
# 
#   #extract scores
#   score_inchikey<-function(req){
# 
#     f<-function(x){
#       data.frame(name=null_replace(x$searchTerm),
#                  inchikey=null_replace(x$result[[1]]$InChIKey),
#                  score=null_replace(x$result[[1]]$score))
#     }
# 
#     req %>%
#       map(~ f(.)) %>%
#       do.call('rbind',.)
# 
#   }
# 
#   score<-score_inchikey(res)
# 
#   #convert to expected format
#   data.frame(from='Chemical Name', to='InChIKey', id=score$name, key=score$inchikey) 
#   
# }
# 
# 
# #convert inchi to id
# get_translation<-function(source_id,source,target){
# 
# 
#     f<-function(x,source,target){
# 
#       x %>%
#         paste(
#           'https://cts.fiehnlab.ucdavis.edu/rest/convert',
#           source,
#           target,
#           .,
#         sep='/') %>%
#         map(~URLencode(.)) %>%
#         unlist()
# 
#     }
# 
#     url <- source_id %>%
#       map( ~ f(.,source,target))
# 
# 
#     f<-function(url){
#       pb$tick()$print()
#       GET(url) %>% content()
#     }
# 
#     pb <- progress_estimated(length(source_id))
#     res<- url %>%
#       map(~ f(.))
# 
#     
#     parse_response<-function(x){
#       
#       tmp<-x %>%
#         flatten(.)
#       
#       if(length(tmp$results)>0){
#         tmp$results <-unlist(flatten(tmp$results))[1]
#       }
#      
#         data.frame(
#           id = null_replace(tmp$searchTerm),
#           from = null_replace(tmp$fromIdentifier),
#           to = null_replace(tmp$toIdentifier),
#           key = null_replace(tmp$results)
#         )
# 
#     }
# 
#     res %>%
#       map(~parse_response(.)) %>%
#       do.call('rbind',.) %>%
#       mutate(id = id) # to returned normalized results in upon error
# }

# get_translation<-function(source_id,source,target){
#   
#   
#   data.frame(
#     id = source_id,
#     from = source,
#     to = target,
#     key = 'trans-foo'
#   ) 
#   
# }
# 
# name_to_inchikey<-function(source_id){
# 
#   
#   data.frame(
#     id = source_id,
#     from = 'Chemical Name',
#     to = 'InChIKey',
#     key = 'inchi-foo'
#   ) 
#   
# }


#' #check if cid is valid
#' #' @export
#' check_cid<-function(cid){
#'   
#'   f<-function(cid){
#'   
#'     pb$tick()$print()
#'     
#'     url<-paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/',
#'                 cid,
#'                 '/property/MolecularFormula/JSON')
#'     
#'     res<-GET(url)
#'     status<-http_status(res)
#'     msg<-paste0(status$message,": ",cid)
#'     
#'     out<-list(status=status$category,message=msg)
#'     
#'     
#'     if(status$category != 'Success'){
#'       out$valid<-FALSE
#'     } else {
#'       out$valid<-TRUE
#'     }
#'     
#'     data.frame(out)
#'   }
#'   
#'   pb <- progress_estimated(length(cid))
#'   
#'   as.list(cid) %>%
#'     map( ~ f(.)) %>%
#'     do.call('rbind',.)
#' 
#' }

#' #' @export
#' CTSgetR_query<-function(id,from,to,db_name='inst/ctsgetr.sqlite'){
#'   
#'   
#'   in_db<-db_get(id, from, to, db_name)
#'   
#'   have<-id[id %in% in_db$id]
#'   need<-id[!id %in% in_db$id]
#'   
#'   #need to retrieve data from API
#'   if(length(need)>0){
#'     
#'     if(from == 'Chemical Name'){
#'       api_res<-name_to_inchikey(need)
#'     } else {
#'       api_res<-get_translation(need,from,to)
#'     }
#'     
#'     
#'     #add to db
#'     tmp<-api_res %>%
#'       db_transform(.) %>%
#'       .$data %>%
#'       mutate(key_index=db_key(.))
#'       #%>% na.omit()
#'     
#'     if(nrow(tmp)>0){
#'       db_add(tmp$source_id, tmp$source, tmp$target,tmp$target_id,db_name)
#'     }
#'     
#'     if(nrow(in_db)>0){
#'       in_db<- rbind(in_db,api_res )
#'     } else {
#'       in_db<-api_res
#'     }
#'     
#'     in_db <-in_db[ as.character(in_db$id) %in% id,]
#'     
#'   } 
#'   
#'   return(in_db)
#'   
#' }

#' #reverse compatible function
#' #' @export
#' CTSgetR<-function(id, from, to, db_name='inst/ctsgetr.sqlite'){ 
#'   
#'   #clunky bind between api naming and db naming
#' 
#'   if(any(!to%in%valid_to()) | any(!from%in%valid_from())) {
#'       
#'     stop(
#'       paste0("The supplied to: ",to, " or from: ", from," are not in the list of available options.\nSee valid_to() and valid_from() for valid options.","\n",collapse="")
#'     )
#'     
#'   }
#'   
#'   if(from == 'Chemical Name'){
#'     #name --> to inchikey --> to 
#'     id<-tolower(id)
#'     ikeys<-CTSgetR_query(id,from,to='InChIKey',db_name)
#'     
#'     keys<-CTSgetR_query(ikeys$key,from='InChIKey',to,db_name)  
#'     
#'     out<-left_join(ikeys,keys, by=c('key'='id'),suffix = c("", ".y"))
#'   
#'     out<-out %>% select(id,from,to.y,key.y) %>%
#'       setNames(.,c('id','from','to','key'))
#'     
#'     #fill in NA in from to
#'     out$from[is.na(out$from)]<-na.omit(out$from)[1]
#'     out$to[is.na(out$to)]<-na.omit(out$to)[1]
#'       
#'   } else {
#'     
#'     out<-CTSgetR_query(id,from,to,db_name)
#'     
#'   }
#'   
#'   #format
#'   
#'   return(out)
#'   
#' }

#' # SQLite functions -------------------------------------------------------
#' #format API response to db structure (names)
#' db_transform<-function(obj){
#'   
#'   #match database
#'   #trying to detect column
#'   #directed i.e. don't need symmetrical queries from
#'   ord<-order(c(obj$from[1] %>% as.character(),obj$to[1] %>% as.character()))
#'     
#'   name<-c('source','target')[ord]
#'   id<-c('source_id','target_id')[ord]
#'   
#'   list(data=data.frame(obj$from %>% unlist(),obj$to %>% unlist(),obj$id %>% unlist(),obj$key %>% unlist()) %>%
#'     setNames(.,c(name,id)),inverted = ord[1]> ord[2]) 
#'   
#' }
#' 
#' 
#' #unique key
#' db_key<-function(obj){
#'   
#'   #this assumes data is normalized 
#'   #for db structure already
#'   paste(obj$source,obj$target,obj$source_id,obj$target_id, sep='_')
#'   
#' }
#' 
#' #check if key exists in db
#' #and add if absent else add methods for upsert
#' #' @export
#' db_add<-function(id, from, to, key,db_name='inst/ctsgetr.sqlite',upsert=FALSE){
#'   
#'   mydb <- tryCatch(dbConnect(RSQLite::SQLite(), db_name),error=function(e){})
#'   
#'   if(is.null(mydb)){stop('No database found!')}
#'   
#'   
#'   #format input
#'   input <- data.frame(id, from, to,key) %>% 
#'     db_transform(.) %>% .$data %>%
#'     mutate(key_index=db_key(.))
#'   
#'   #check if values exist
#'   #if so do not replace
#'   if(!upsert){
#'     query<-"SELECT EXISTS(SELECT 1 FROM CTSgetR WHERE key_index=:key_index);"
#'     params<-list(key_index=input$key_index %>% as.character()) # all exist
#'     old<-dbGetQuery(mydb, query,params) %>% unlist() %>% as.logical()
#'     
#'     input<-input[!old,]
#'     
#'   }
#'   
#'   #add
#'   if(nrow(input) > 0){
#'     dbWriteTable(mydb, "CTSgetR", input, append=TRUE)
#'   } else {
#'     print('All values are already in the database, use upsert=TRUE to replace.')
#'   }
#'   
#'   DBI::dbDisconnect(mydb)
#'   
#' }
#' 
#' #retrieve from DB if present
#' #' @export
#' db_get <-function(id, from, to, db_name='inst/ctsgetr.sqlite'){
#'   
#'   mydb <- tryCatch(dbConnect(RSQLite::SQLite(), db_name),error=function(e){})
#'   
#'   if(is.null(mydb)){print('No database found!');return()}
#'   
#'  
#'   #format input
#'   input <- data.frame(id, from, to,key=NA) %>% db_transform()
#'   invert<-!input$inverted
#'   input<-input$data
#'   
#'   
#'   if(invert){
#'     query <- "SELECT * FROM CTSgetR
#'   WHERE
#'   (
#'   (source = :source and target = :target)
#'   AND
#'   (source_id = :source_id)
#'   )"
#'     
#'     pnames<-c('source','target','source_id')
#'     
#'   } else{
#'     query <- "SELECT * FROM CTSgetR
#'   WHERE
#'   (
#'   (source = :source and target = :target)
#'   AND
#'   (target_id = :target_id)
#'   )"
#'     
#'     pnames<-c('source','target','target_id')
#'   }
#'   
#' 
#'   params <-input[pnames] %>% as.list()
#' 
#'   
#'   res<-dbGetQuery(mydb, query,params) 
#'   
#'   #format as API results
#'   if(invert){
#'     vars <- c("id"='source_id' , 'from'='source', "key"='target_id' , 'to' = 'target')
#'     
#'   } else {
#'      vars <- c("id"='target_id' , 'from'='target', "key"='source_id' , 'to' = 'source')
#'   }
#'  
#'   
#'   out<- res %>%
#'     rename(., !!vars) %>%
#'     select(-key_index) %>%
#'     .[c('id','from','to','key')]
#'   
#'   DBI::dbDisconnect(mydb)
#'   
#'   return(out)
#' }
#' 
#' #get edge list stats
#' 
#' 
#' db_stats<-function(data=FALSE,db_name='inst/ctsgetr.sqlite'){
#'   
#'   mydb <- dbConnect(RSQLite::SQLite(), db_name)
#'   query<-"SELECT * FROM CTSgetR"
#'   res<-dbGetQuery(mydb, query)
#'   DBI::dbDisconnect(mydb)
#'   
#'   x<-paste(as.character(res$source),as.character(res$target),sep=' <--> ')
#'   if(data){
#'     res
#'   } else {
#'     list(data.frame(table(x)) %>% setNames(.,c('translation','n')),total=length(x))
#'   }
#'   
#' }
#' 
#' #' @export
#' init_CTSgetR_db<-function(db_name='inst/ctsgetr.sqlite'){
#'   
#'  x<-structure(list(source = structure(c(1L, 1L, 1L), .Label = "InChIKey", class = "factor"), 
#'                    target = structure(c(1L, 1L, 1L), .Label = "PubChem CID", class = "factor"), 
#'                    source_id = structure(1:3, .Label = c("JVTAAEKCZFNVCJ-UHFFFAOYSA-N", 
#'                                                          "BWLBGMIXKSTLSX-UHFFFAOYSA-N", "AEMRFAOFKBGASW-UHFFFAOYSA-N"
#'                    ), class = "factor"), target_id = structure(1:3, .Label = c("19789253", 
#'                                                                                "11671", "3698251"), class = "factor"), key_index = c("InChIKey_PubChem CID_JVTAAEKCZFNVCJ-UHFFFAOYSA-N_19789253", 
#'                                                                                                                                      "InChIKey_PubChem CID_BWLBGMIXKSTLSX-UHFFFAOYSA-N_11671", 
#'                                                                                                                                      "InChIKey_PubChem CID_AEMRFAOFKBGASW-UHFFFAOYSA-N_3698251"
#'                                                                                )), class = "data.frame", row.names = c(NA, -3L))
#'   mydb <- dbConnect(RSQLite::SQLite(), db_name)
#'   dbWriteTable(mydb, "CTSgetR", x, overwrite=TRUE)
#' }


# tests -------------------------------------------------------------------
id <- c('1 2-Propanediol',
        'Lactic acid',
        '2-Hydroxyisobutyric acid',
        'Glycolic acid')
from<-'Chemical Name'
to<-'PubChem CID'
(res<-CTSgetR(id, from, to))

id <- c("19789253", "11671", "3698251")
from<-'PubChem CID'
to<-'KEGG'
(res<-CTSgetR(id, from, to))

eeeee
id <- c("19789253", "11671", "3698251")
from<-'PubChem CID'
to<-'KEGG'
(res<-CTSgetR(id, from, to))

#name to inchikey
id <- c('1 2-Propanediol',
          'Lactic acid',
          '2-Hydroxyisobutyric acid',
          'Glycolic acid')
from<-'Chemical Name'
to<-'InChIKey'

(res<-CTSgetR(id, from, to))

# (inchikeys<-name_to_inchikey(name))

#get matches
from<-'InChIKey'
to<-'KEGG'
# key<-'JVTAAEKCZFNVCJ-UHFFFAOYSA-N'
to<-'KEGG'
key<-inchikeys$inchikey %>% unlist() %>% na.omit()

#
to<-'PubChem CID'
(res<-get_translation(key,from,to))

from<-"Chemical Name"
to<-'KEGG'


#name to CID
id<-c('1 2-Propanediol',
      'Lactic acid',
      '2-Hydroxyisobutyric acid',
      'Glycolic acid')
from<-"Chemical Name"
to<-'PubChem CID'
(res1<-CTSgetR(key,from,to))

#CID to KEGG
id<-res1$id
from<-'InChIKey'
to<-c('KEGG')
(res2<-CTSgetR(id,from,to))

# #format results for db
# vars <- c(source_id = "id", source='from', target_id ="key", target ='to')
# 
# input<- res %>%
#   na.omit() %>%
#   rename(., !!vars)

example0<-structure(list(name = structure(1:4, .Label = c("1 2-propanediol", 
                                                          "lactic acid", "2-hydroxyisobutyric acid", "glycolic acid"), class = "factor"), 
                         inchikey = c(NA, "JVTAAEKCZFNVCJ-UHFFFAOYSA-N", "BWLBGMIXKSTLSX-UHFFFAOYSA-N", 
                                      "AEMRFAOFKBGASW-UHFFFAOYSA-N"), score = c(0, 1, 1, 1)), row.names = c(NA, 
                                                                                                            -4L), class = "data.frame")


example<-structure(list(id = structure(1:3, .Label = c("JVTAAEKCZFNVCJ-UHFFFAOYSA-N", 
                                                       "BWLBGMIXKSTLSX-UHFFFAOYSA-N", "AEMRFAOFKBGASW-UHFFFAOYSA-N"), class = "factor"), 
                        from = structure(c(1L, 1L, 1L), .Label = "InChIKey", class = "factor"), 
                        to = structure(c(1L, 1L, 1L), .Label = "PubChem CID", class = "factor"), 
                        key = structure(1:3, .Label = c("19789253", "11671", "3698251"
                        ), class = "factor")), row.names = c(NA, -3L), class = "data.frame")


#innchi to CID
id<-example$id %>% as.character()
from<-'InChIKey'
to<-'PubChem CID'


example2<-structure(list(id = structure(1:3, .Label = c("JVTAAEKCZFNVCJ-UHFFFAOYSA-N", 
                                                        "BWLBGMIXKSTLSX-UHFFFAOYSA-N", "AEMRFAOFKBGASW-UHFFFAOYSA-N"), class = "factor"), 
                         from = structure(c(1L, 1L, 1L), .Label = "InChIKey", class = "factor"), 
                         to = structure(c(1L, 1L, 1L), .Label = "KEGG", class = "factor"), 
                         key = structure(c(1L, NA, 2L), .Label = c("C01432", "C00160"
                         ), class = "factor")), row.names = c(NA, -3L), class = "data.frame")

#DATABASE functions and tests
#todo cache data in local sqlite
#check db before webservices call
#add to db after if new
library(RSQLite)

db_name<-'inst/ctsgetr.sqlite'


to_load<-transform_db(example) %>%
  mutate(key_index=db_key(.))

#create first time
dbWriteTable(mydb, "CTSgetR", to_load, overwrite=TRUE)

#create index
mydb <- dbConnect(RSQLite::SQLite(), db_name)
# query<-'CREATE UNIQUE INDEX key_index ON CTSgetR (source, source_id, target,target_id);'
# dbExecute(mydb, query)

#check data
query<-"SELECT * FROM CTSgetR"
dbGetQuery(mydb, query)


#get value from the db

id<-example$id
from<-example$from
to<-example$to
key<-example$key

#inverse of above
id<-example$key
from<-example$to
to<-example$from
key<-example$id

#info
db_stats()

#retrieve data from DB
db_get(id,from,to) 

#add data to db
db_add(id, from, to, key)


#combining db look ups and API calls
