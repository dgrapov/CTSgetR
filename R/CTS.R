
#' @title CTSgetR
#' @param id a vector of metabolite identifier(s) or name(s) to translate see \code{\link{valid_to}} and \code{\link{valid_from}}
#' @param from Database name describing \code{id} see \code{\link[CTSgetR]{valid_from}}
#' @param to Database name to translate \code{id} see \code{\link[CTSgetR]{valid_to}}
#' @param db_name string path for sqlite database to store cached results in see \code{\link[CTSgetR]{valid_to}}
#' @return data frame of results including \code{from} (fromIdentifier), \code{id} (searchTerm), \code{to} (toIdentifier) and translated values
#' @details Interface to CTS (http://cts.fiehnlab.ucdavis.edu/) for metabolite identifier translation between
#'  > 200 of the most common biological databases including: Chemical Name, InChIKey, PubChem CID,
#'  ChemSpider, BioCyc, ChEBI, CAS, HMDB, KEGG and LipidMAPS.
#' @seealso  \code{\link[CTSgetR]{single_CTSgetR}}
#' @export
#' @import httr dplyr purrr reshape2
#' @examples
#' \dontrun{
#' id<-c("C15973","C00026","C05381")
#' from<-"KEGG"
#' to<-"PubChem CID"
#' CTSgetR(id,from,to)
#' }
CTSgetR<-function(id, from, to, db_name = 'ctsgetr.sqlite', ...){
  

  if(is.null(db_name) || !file.exists(db_name)) {
    db_name<-'ctsgetr.sqlite'
    init_CTSgetR_db(db_name) # initialize schema and small sample
  }
  
  .id<-unique(id)
  args<-  to %>% map( ~ CTSgetR_format(.id, from, ., db_name=db_name,format=TRUE, key_split=TRUE))
  
  
  out<-args %>%
    map( ~ do.call('single_CTSgetR', .)) %>%
      do.call('rbind', .)
  
  rownames(out)<-NULL
  
  #add back any user supplied duplicates
  db_key<- paste(out$id,out$from,out$to,out$key, sep='_')
  melted<-dcast(out[!duplicated(db_key),],id~to,value.var = 'key')
  out<-left_join(data.frame(id=.id),melted)
  
  return(out)
  
}


single_CTSgetR <- function(id, from, to, db_name = NULL, ...) {
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
  
  if (from == 'Chemical Name' | to == 'Chemical Name') {
    #name --> inchikey --> to
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
    
  } 
  
  
  if(from != 'Chemical Name' & to != 'Chemical Name') {
      out <- CTSgetR_query(id, from, to, db_name)
    
  }
  
  #format
  
  return(out)
  
}


#' @export
CTSgetR_format<-function(id,from,to,db_name='ctsgetr.sqlite',key_split=FALSE,format=FALSE,...){
  


  if(!'data.frame' %in% class(id)){
    
    out<-expand.grid(id=id,from=from,to=to,db_name=db_name) 
    
  } else {
    
    out<-id
  }
  
  if(key_split){
    out<-out %>%
      tidyr::unite('split_key',c(from,to),remove=FALSE) %>%
      mutate('index'=rownames(.)) %>%
      split(.,.$'split_key')
  } 
  
  # #deal with duplicated entries --
  out<-lapply(out, function(x){
   x[!duplicated(x$id),]
  }) 
  

  
  #format for call
  if(format){
    out<-lapply(out, function(x){
      
      list(id=x$id,from=x$from[1],to=x$to[1],db_name=db_name)
      
    })
  }
  
  return(out %>% flatten())
  
}


# #convert name to inchikey and get top score
#' @export
#' @param chemical names as strings or a vector
#' @param algorithm string one of  'biological' or 'popularity' defaults to 'biological' see https://cts.fiehnlab.ucdavis.edu/services
#' @return inchikeys and scores or NA if absent for each chemical name
name_to_inchikey <- function(name, algorithm = 'biological') {
  f <- function(x) {
    x %>%
      tolower() %>%
      URLencode(.,reserved=TRUE) %>%
      paste0(
        'https://cts.fiehnlab.ucdavis.edu/rest/score/Chemical%20Name/',
        .,
        '/',
        algorithm
      )
    
  }
  
  url <- name %>%
    map(~ f(.)) %>%
    unlist()
  
  
  f <- function(url) {
    pb$tick()$print()
    GET(url) %>% content()
  }
  
  pb <- progress_estimated(length(url))
  
  res <- url %>%
    map( ~ f(.))
  
  #extract scores
  score_inchikey <- function(req) {
    f <- function(x) {
      data.frame(
        name = null_replace(x$searchTerm),
        inchikey = null_replace(x$result[[1]]$InChIKey),
        score = null_replace(x$result[[1]]$score)
      )
    }
    
    req %>%
      map( ~ f(.)) %>%
      do.call('rbind', .)
    
  }
  
  score <- score_inchikey(res)
  
  #convert to expected format
  data.frame(
    from = 'Chemical Name',
    to = 'InChIKey',
    id = score$name,
    key = score$inchikey
  )
  
}

#convert inchi to id
get_translation <- function(source_id, source, target) {
  f <- function(x, source, target) {
    x %>%
      paste('https://cts.fiehnlab.ucdavis.edu/rest/convert',
            source,
            target,
            .,
            sep = '/') %>%
      map( ~ URLencode(.)) %>%
      unlist()
    
  }
  
  url <- source_id %>%
    map(~ f(., source, target))
  
  
  f <- function(url) {
    pb$tick()$print()
    GET(url) %>% content()
  }
  
  pb <- progress_estimated(length(source_id))
  res <- url %>%
    map( ~ f(.))
  
  
  parse_response <- function(x) {
    tmp <- x %>%
      purrr::flatten(.)
    
    if (length(tmp$results) > 0) {
      tmp$results <- unlist(purrr::flatten(tmp$results))[1]
    }
    
    data.frame(
      id = null_replace(tmp$searchTerm),
      from = null_replace(tmp$fromIdentifier),
      to = null_replace(tmp$toIdentifier),
      key = null_replace(tmp$results)
    )
    
  }
  
  res %>%
    map( ~ parse_response(.)) %>%
    do.call('rbind', .) %>%
    mutate(id = id) # to returned normalized results in upon error
}

CTSgetR_query <-
  function(id, from, to, db_name = NULL) {
    in_db <- db_get(id, from, to, db_name)
    
    if (!is.null(in_db) & nrow(in_db)>0) {
      in_db <- db_get(id, from, to, db_name)
      
      have <- id[id %in% in_db$id]
      need <- id[!id %in% in_db$id]
    } else {
      have <- character()
      need <- id
      in_db <- data.frame()
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
      
      in_db <- in_db[as.character(in_db$id) %in% id, ]
      
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
    list("BioCyc", "CAS", "ChEBI", "Chemical Name", "Human Metabolome Database", 
         "InChIKey", "KEGG", "LMSD", "LipidMAPS", "PubChem CID", "SMILES", 
         "Pubchem SID", "ChemSpider", "ChemDB", "ChEMBL", "ChemBank", 
         "ZINC", "Broad Institute", "The Scripps Research Institute Molecular Screening Center", 
         "DrugBank", "MMDB", "ChemMol", "NIAID", "Southern Research Institute", 
         "NIH Clinical Collection", "Comparative Toxicogenomics Database", 
         "MolPort", "SMID", "ChemBridge", "GlaxoSmithKline (GSK)", 
         "NINDS Approved Drug Screening Program", "NIST", "SCRIPDB", 
         "Southern Research Specialized Biocontainment Screening Center", 
         "TimTec", "Tox21", "Web of Science", "ABI Chem", "Center for Chemical Genomics, University of Michigan", 
         "ChemIDplus", "Sigma-Aldrich", "ASINEX", "ChemBlock", "DiscoveryGate", 
         "Specs", "Ambinter", "Emory University Molecular Libraries Screening Center", 
         "MLSMR", "Vitas-M Laboratory", "AAA Chemistry", "ChemFrog", 
         "Isoprenoids", "NCGC", "Acesobio", "ALDRICH", "Ambit Biosciences", 
         "CLRI (CSIR)", "InFarmatik", "IS Chemical Technology", "MOLI", 
         "SIGMA", "Tetrahedron Scientific Inc", "Thomson Pharma", 
         "ABBLIS Chemicals", "Abbott Labs", "AbMole Bioscience", "Achemica", 
         "Acorn PharmaTech", "Active Biopharma", "Adooq BioScience", 
         "AKos Consulting & Solutions", "AK Scientific, Inc. (AKSCI)", 
         "Alagar Yadav, Karpagam University", "Alinda Chemical", "Alsachim", 
         "Amadis Chemical", "Amatye", "AmicBase - Antimicrobial Activities", 
         "Angene Chemical", "Angene International", "Ark Pharm, Inc.", 
         "BioChemPartner", "BroadPharm", "Chembase.cn", "Chembo", 
         "chemicalize.org by ChemAxon", "Fragmenta", "Anitha, Department of Bioinformatics, Karpagam University", 
         "Annker Organics", "Anward", "Apeiron Synthesis", "ApexBio Technology", 
         "Apexmol", "Aromsyn catalogue", "Aronis", "Aurora Fine Chemicals LLC", 
         "Aurum Pharmatech LLC", "Avanti Polar Lipids", "Beijing Advanced Technology Co, Ltd", 
         "Bertin Pharma", "Bhaskar Lab, Department of Zoology, Sri Venkateswara University", 
         "BIDD", "BIND", "BindingDB", "Biological Magnetic Resonance Data Bank (BMRB)", 
         "Bioprocess Technology Lab, Department of Microbiology, Bharathidasan University", 
         "Biosynth", "Burnham Center for Chemical Genomics", "Calbiochem", 
         "Cambridge Crystallographic Data Centre", "CAPOT", "Cayman Chemical", 
         "CC_PMLSC", "ChemExper Chemical Directory", "Chemical Biology Department, Max Planck Institute of Molecular Physiology", 
         "ChemScene", "ChemSynthesis", "ChemTik", "Chiralblock Biosciences", 
         "Circadian Research, Kay Laboratory, University of California at San Diego (UCSD)", 
         "CMLD-BU", "Columbia University Molecular Screening Center", 
         "Creasyn Finechem", "Department of Pharmacy, LMU", "DTP/NCI", 
         "EDASA Scientific Compounds June 2013", "EMD Biosciences", 
         "Enamine", "Ennopharm", "EPA DSSTox", "Excenen Pharmatech", 
         "Exchemistry", "FINETECH", "Finley and King Labs, Harvard Medical School", 
         "FLUKA", "ForeChem", "Georganics", "GLIDA, GPCR-Ligand Database", 
         "GNF / Scripps Winzeler lab", "Golm Metabolome Database (GMD), Max Planck Institute of Molecular Plant Physiology", 
         "Hangzhou APIChem Technology", "Hangzhou Trylead Chemical Technology", 
         "HDH Pharma", "HUMGENEX", "IBCH RAS", "IBM", "ICCB-Longwood/NSRB Screening Facility, Harvard Medical School", 
         "Immunology Lab, Department of Biotechnology, Calicut University", 
         "Inhibitor 2", "Insect Molecular Biology Lab, Department of Environmental Biotechnology, Bharathidasan University", 
         "iThemba Pharmaceuticals", "IUPHAR-DB", "Jamson Pharmachem Technology", 
         "Japan Chemical Substance Dictionary (Nikkaji)", "Johns Hopkins Ion Channel Center", 
         "Kingston Chemistry", "KUMGM", "LeadScope", "MedChemexpress MCE", 
         "MICAD", "MIC Scientific", "Milwaukee Institute for Drug Discovery", 
         "Molecular Libraries Program, Specialized Chemistry Center, University of Kansas", 
         "MP Biomedicals", "MTDP", "Nanjing Pharmaceutical Factory", 
         "Nantong Baihua Bio-Pharmaceutical Co., Ltd", "Nature Chemical Biology", 
         "Nature Chemistry", "Nature Communications", "NIST Chemistry WebBook", 
         "Nitric Oxide Research, National Cancer Institute (NCI)", 
         "NMMLSC", "NMRShiftDB", "NovoSeek", "Oakwood Products", "ORST SMALL MOLECULE SCREENING CENTER", 
         "P3 BioSystems", "PANACHE", "Paul Baures", "PCMD", "PDSP", 
         "PENN-ABS", "PennChem-GAM", "PFC", "P.Ravikumar, M.Jeyam and G.Shalini. Biochematics Division, Bharathiar University", 
         "priyadharshini sabarathinam angayarkanni murugesh palaniswamy", 
         "Prous Science Drugs of the Future", "Rangan Lab, Department of Biotechnology, IIT Guwahati", 
         "R&D Chemicals", "R.Sathishkumar, Phytomatics Laboratory, Department of Bioinformatics, Bharathiar University", 
         "RSChem, LLC", "SASTRA University, Quorum sensing and Peptidomimetics Laboratory", 
         "Selleckbio", "Selleck Chemicals", "SGCOxCompounds", "SGCStoCompounds", 
         "S.GURUDEEBAN, T.RAMANATHAN & K.SATYAVANI, Marine Medicinal Plant Biotechnology Laboratory, Faculty of Marine Sciences, Annamalai Universtiy", 
         "Shanghai Institute of Organic Chemistry", "Shanghai Sinofluoro Scientific Company", 
         "SLING Consortium", "SRMLSC", "Structural Genomics Consortium", 
         "SureChem", "SYNCHEM OHG", "Syntechem", "TCI (Tokyo Chemical Industry)", 
         "ten Dijke Lab, Leiden University Medical Center", "Therapeutic Targets Database", 
         "Total TOSLab Building-Blocks", "True PharmaChem", "Tyger Scientific", 
         "UCLA Molecular Screening Shared Resource", "UM-BBD", "UniCarbKB", 
         "University of Pittsburgh Molecular Library Screening Center", 
         "UPCMLD", "Vanderbilt Screening Center for GPCRs, Ion Channels and Transporters", 
         "Vanderbilt Specialized Chemistry Center", "Vanderbilt University Medical Center", 
         "VIT University", "Watec Laboratories", "Watson International Ltd", 
         "xPharm", "Zancheng Functional Chemicals", "zealing chemical")
  }
}


#' @export
#' @param update logical (default FALSE) if live or cached results should be returned
valid_to <- function(update = FALSE) {
  if (update) {
    url <- 'https://cts.fiehnlab.ucdavis.edu/rest/toValues'
    GET(url) %>% content()
    
  } else {
    list("BioCyc", "CAS", "ChEBI", "Chemical Name", "Human Metabolome Database", 
         "InChI Code", "InChIKey", "KEGG", "LMSD", "LipidMAPS", "PubChem CID", 
         "SMILES", "Pubchem SID", "ChemSpider", "ChemDB", "ChEMBL", 
         "ChemBank", "ZINC", "Broad Institute", "The Scripps Research Institute Molecular Screening Center", 
         "DrugBank", "MMDB", "ChemMol", "NIAID", "Southern Research Institute", 
         "NIH Clinical Collection", "Comparative Toxicogenomics Database", 
         "MolPort", "SMID", "ChemBridge", "GlaxoSmithKline (GSK)", 
         "NINDS Approved Drug Screening Program", "NIST", "SCRIPDB", 
         "Southern Research Specialized Biocontainment Screening Center", 
         "TimTec", "Tox21", "Web of Science", "ABI Chem", "Center for Chemical Genomics, University of Michigan", 
         "ChemIDplus", "Sigma-Aldrich", "ASINEX", "ChemBlock", "DiscoveryGate", 
         "Specs", "Ambinter", "Emory University Molecular Libraries Screening Center", 
         "MLSMR", "Vitas-M Laboratory", "AAA Chemistry", "ChemFrog", 
         "Isoprenoids", "NCGC", "Acesobio", "ALDRICH", "Ambit Biosciences", 
         "CLRI (CSIR)", "InFarmatik", "IS Chemical Technology", "MOLI", 
         "SIGMA", "Tetrahedron Scientific Inc", "Thomson Pharma", 
         "ABBLIS Chemicals", "Abbott Labs", "AbMole Bioscience", "Achemica", 
         "Acorn PharmaTech", "Active Biopharma", "Adooq BioScience", 
         "AKos Consulting & Solutions", "AK Scientific, Inc. (AKSCI)", 
         "Alagar Yadav, Karpagam University", "Alinda Chemical", "Alsachim", 
         "Amadis Chemical", "Amatye", "AmicBase - Antimicrobial Activities", 
         "Angene Chemical", "Angene International", "Ark Pharm, Inc.", 
         "BioChemPartner", "BroadPharm", "Chembase.cn", "Chembo", 
         "chemicalize.org by ChemAxon", "Fragmenta", "Anitha, Department of Bioinformatics, Karpagam University", 
         "Annker Organics", "Anward", "Apeiron Synthesis", "ApexBio Technology", 
         "Apexmol", "Aromsyn catalogue", "Aronis", "Aurora Fine Chemicals LLC", 
         "Aurum Pharmatech LLC", "Avanti Polar Lipids", "Beijing Advanced Technology Co, Ltd", 
         "Bertin Pharma", "Bhaskar Lab, Department of Zoology, Sri Venkateswara University", 
         "BIDD", "BIND", "BindingDB", "Biological Magnetic Resonance Data Bank (BMRB)", 
         "Bioprocess Technology Lab, Department of Microbiology, Bharathidasan University", 
         "Biosynth", "Burnham Center for Chemical Genomics", "Calbiochem", 
         "Cambridge Crystallographic Data Centre", "CAPOT", "Cayman Chemical", 
         "CC_PMLSC", "ChemExper Chemical Directory", "Chemical Biology Department, Max Planck Institute of Molecular Physiology", 
         "ChemScene", "ChemSynthesis", "ChemTik", "Chiralblock Biosciences", 
         "Circadian Research, Kay Laboratory, University of California at San Diego (UCSD)", 
         "CMLD-BU", "Columbia University Molecular Screening Center", 
         "Creasyn Finechem", "Department of Pharmacy, LMU", "DTP/NCI", 
         "EDASA Scientific Compounds June 2013", "EMD Biosciences", 
         "Enamine", "Ennopharm", "EPA DSSTox", "Excenen Pharmatech", 
         "Exchemistry", "FINETECH", "Finley and King Labs, Harvard Medical School", 
         "FLUKA", "ForeChem", "Georganics", "GLIDA, GPCR-Ligand Database", 
         "GNF / Scripps Winzeler lab", "Golm Metabolome Database (GMD), Max Planck Institute of Molecular Plant Physiology", 
         "Hangzhou APIChem Technology", "Hangzhou Trylead Chemical Technology", 
         "HDH Pharma", "HUMGENEX", "IBCH RAS", "IBM", "ICCB-Longwood/NSRB Screening Facility, Harvard Medical School", 
         "Immunology Lab, Department of Biotechnology, Calicut University", 
         "Inhibitor 2", "Insect Molecular Biology Lab, Department of Environmental Biotechnology, Bharathidasan University", 
         "iThemba Pharmaceuticals", "IUPHAR-DB", "Jamson Pharmachem Technology", 
         "Japan Chemical Substance Dictionary (Nikkaji)", "Johns Hopkins Ion Channel Center", 
         "Kingston Chemistry", "KUMGM", "LeadScope", "MedChemexpress MCE", 
         "MICAD", "MIC Scientific", "Milwaukee Institute for Drug Discovery", 
         "Molecular Libraries Program, Specialized Chemistry Center, University of Kansas", 
         "MP Biomedicals", "MTDP", "Nanjing Pharmaceutical Factory", 
         "Nantong Baihua Bio-Pharmaceutical Co., Ltd", "Nature Chemical Biology", 
         "Nature Chemistry", "Nature Communications", "NIST Chemistry WebBook", 
         "Nitric Oxide Research, National Cancer Institute (NCI)", 
         "NMMLSC", "NMRShiftDB", "NovoSeek", "Oakwood Products", "ORST SMALL MOLECULE SCREENING CENTER", 
         "P3 BioSystems", "PANACHE", "Paul Baures", "PCMD", "PDSP", 
         "PENN-ABS", "PennChem-GAM", "PFC", "P.Ravikumar, M.Jeyam and G.Shalini. Biochematics Division, Bharathiar University", 
         "priyadharshini sabarathinam angayarkanni murugesh palaniswamy", 
         "Prous Science Drugs of the Future", "Rangan Lab, Department of Biotechnology, IIT Guwahati", 
         "R&D Chemicals", "R.Sathishkumar, Phytomatics Laboratory, Department of Bioinformatics, Bharathiar University", 
         "RSChem, LLC", "SASTRA University, Quorum sensing and Peptidomimetics Laboratory", 
         "Selleckbio", "Selleck Chemicals", "SGCOxCompounds", "SGCStoCompounds", 
         "S.GURUDEEBAN, T.RAMANATHAN & K.SATYAVANI, Marine Medicinal Plant Biotechnology Laboratory, Faculty of Marine Sciences, Annamalai Universtiy", 
         "Shanghai Institute of Organic Chemistry", "Shanghai Sinofluoro Scientific Company", 
         "SLING Consortium", "SRMLSC", "Structural Genomics Consortium", 
         "SureChem", "SYNCHEM OHG", "Syntechem", "TCI (Tokyo Chemical Industry)", 
         "ten Dijke Lab, Leiden University Medical Center", "Therapeutic Targets Database", 
         "Total TOSLab Building-Blocks", "True PharmaChem", "Tyger Scientific", 
         "UCLA Molecular Screening Shared Resource", "UM-BBD", "UniCarbKB", 
         "University of Pittsburgh Molecular Library Screening Center", 
         "UPCMLD", "Vanderbilt Screening Center for GPCRs, Ion Channels and Transporters", 
         "Vanderbilt Specialized Chemistry Center", "Vanderbilt University Medical Center", 
         "VIT University", "Watec Laboratories", "Watson International Ltd", 
         "xPharm", "Zancheng Functional Chemicals", "zealing chemical")
    
  }
}

#' @export
#' @param cid compound identifier see https://pubchem.ncbi.nlm.nih.gov/
#' @details Check if valid CID; used to filter bad queries to PUG endpoints
check_cid<-function(cid){
  
  f<-function(cid){
    
    pb$tick()$print()
    
    url<-paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/',
                cid,
                '/property/MolecularFormula/JSON')
    
    res<-GET(url)
    status<-http_status(res)
    msg<-paste0(status$message,": ",cid)
    
    out<-list(status=status$category,message=msg)
    
    
    if(status$category != 'Success'){
      out$valid<-FALSE
    } else {
      out$valid<-TRUE
    }
    
    data.frame(out)
  }
  
  pb <- progress_estimated(length(cid))
  
  as.list(cid) %>%
    map( ~ f(.)) %>%
    do.call('rbind',.)
  
}


#' @noRd 
#' @export
CTSgetR_summary<-function(from=NULL,to=NULL,obj=NULL){
  
  f<-function(x){
    
    
    .len<-length(x)
    
    if(.len > 2){
      tmp<-paste0(x[1:(.len-1)],collapse=', ')
      tmp<-paste0(c(tmp,x[.len]),collapse=' and ')
    } else {
      tmp<-paste0(x,collapse=' and ')
    }
    
    tmp
  }
  
  .method<-NULL
  if(!is.null(to) & !is.null(from)){
    .method<-paste0('Metabolite ',from, 's were translated to ',f(to),' identifiers.')
  }
  
  
  #summarise missing translations
  if(!is.null(obj)){
    x<-obj %>% select(-id)
    missing<-apply(is.na(x) ,2,sum)
    
    if(any(missing)>0){
      
      id<-unlist(missing>0)
      info<-apply(matrix(c(colnames(x) ,paste0('(',unlist(missing),')')),nrow=ncol(x)),1,'paste', collapse=' ')
      
      obj<-paste0('The following translations could not be completed: ',f(info[id]),'.')
    }
  }
  
  paste(c(.method,obj),collapse=' ')
  
}



test<-function(){

  library(CTSgetR)  
  db_name<-'ctsgetr.sqlite'
  
  #from one to many
  id<-c("alanine",'foo','lactic acid','foo')
  from<-"Chemical Name"
  to<- c( "PubChem CID", "KEGG","Human Metabolome Database")
  
  (out<-CTSgetR(id,from,to,db_name=db_name))
  
  
  id<-out$`PubChem CID`
  from<-"PubChem CID"
  to<-"Chemical Name"
  
  out2<-CTSgetR(id,from,to,db_name=db_name)
  
  id<-out2$'InChIKey'
  from<-"InChIKey"
  to<- "Chemical Name"
  
  out3<-CTSgetR(id,from,to,db_name=db_name)
  
  
  #from many to many
  args <-structure(list(id = structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 
                                         3L, 4L, 4L), .Label = c("alanine", "foo", "lactic acid", "HMDB0000161"
                                         ), class = "factor"), from = structure(c(1L, 1L, 1L, 1L, 1L, 
                                                                                  1L, 1L, 1L, 1L, 2L, 2L), .Label = c("Chemical Name", "Human Metabolome Database"
                                                                                  ), class = "factor"), to = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 
                                                                                                                         3L, 3L, 3L, 2L, 1L), .Label = c("PubChem CID", "KEGG", "Human Metabolome Database"
                                                                                                                         ), class = "factor")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                    -11L))
  
  
  args %>%
    split(.,.$from) %>%
    map(~CTSgetR(.$id,.$from,.$to,db_name=db_name)) %>%
    bind_rows(.)
  
  
  #deal with duplicated left merge
  a<-data.frame(foo=c(1,2,1,3))
  b<-data.frame(foo=c(1,4))
  left_join(a,b)
  
  
  example_input<-function(){
    
    list(id = c("xylulose", "xylitol", "xanthosine", "xanthine", 
                      "valine", "uridine", "uridine", "uric acid + myo-inositol", "urea", 
                      "uracil", "tyrosine", "tryptophan", "trisaccharide", "triethanolamine", 
                      "trehalose", "trans-4-hydroxyproline", "tocopherol alpha", "thymine", 
                      "thymidine", "threonine", "threonic acid", "threitol", "taurine", 
                      "tagatose", "sulfuric acid", "sucrose", "succinic acid", "stearic acid", 
                      "sorbitol", "sophorose", "shikimic acid", "serine", "sarcosine", 
                      "saccharic acid", "ribulose-5-phosphate", "ribose", "ribitol", 
                      "rhamnose", "pyruvic acid", "pyrophosphate", "pyrazine 2,5-dihydroxy", 
                      "propane-1,3-diol", "proline", "pipecolic acid", "phosphoric acid", 
                      "phosphoethanolamine", "phenylethylamine", "phenylalanine", "phenylacetic acid", 
                      "pelargonic acid", "pantothenic acid", "palmitoleic acid", "palmitic acid", 
                      "oxoproline", "oxalic acid", "orotic acid", "ornithine", "oleic acid", 
                      "N-methylalanine", "nicotinic acid", "nicotinamide", "N-hexanoylglycine", 
                      "N-acetylglycine", "N-acetylglutamate", "N-acetyl-D-tryptophan", 
                      "N-acetyl-D-mannosamine", "N-acetyl-D-hexosamine", "methylhexadecanoic acid", 
                      "methionine sulfoxide", "methionine", "methanolphosphate", "melibiose", 
                      "maltose", "malic acid", "maleimide", "lyxose", "lysine", "linolenic acid", 
                      "linoleic acid", "levanbiose", "leucine", "lauric acid", "lactobionic acid", 
                      "lactic acid", "kynurenine", "isothreonic acid", "isoleucine", 
                      "isocitric acid", "inulotriose", "inositol allo-", "inosine", 
                      "indole-3-lactate", "indole-3-acetate", "icosenoic acid", "hypoxanthine + ornithine", 
                      "hydroxylamine", "hydroxycarbamate", "hydrocinnamic acid", "homoserine", 
                      "homocystine", "histidine", "hexuronic acid", "guanosine", "glycolic acid", 
                      "glycine", "glycerol-alpha-phosphate", "glycerol-3-galactoside", 
                      "glyceric acid", "glutaric acid", "glutamine", "glutamic acid", 
                      "glucuronic acid", "glucose-6-phosphate", "glucose", "gluconic acid", 
                      "glucoheptose", "galactose-6-phosphate", "galactose", "galactonic acid", 
                      "galactinol", "fumaric acid", "fucose + rhamnose", "fucose", 
                      "fructose-6-phosphate", "fructose", "ethanolamine", "erythritol", 
                      "dodecane", "digalacturonic acid", "cytidine-5'-diphosphate", 
                      "cystine", "cysteine", "creatinine", "conduritol beta epoxide", 
                      "citrulline", "citric acid", "cholic acid", "cholesterol", "cellobiotol", 
                      "caprylic acid", "capric acid", "butane-2,3-diol", "beta-alanine", 
                      "benzoic acid", "behenic acid", "aspartic acid", "asparagine", 
                      "arachidonic acid", "arachidic acid", "arabitol", "arabinose", 
                      "aminomalonic acid", "alpha ketoglutaric acid", "allantoin", 
                      "allantoic acid", "alanine", "adipic acid", "adenosine-5-phosphate", 
                      "adenosine", "aconitic acid", "5-methoxytryptamine", "5-hydroxyindole-3-acetic acid", 
                      "4-hydroxyproline", "4-hydroxyhippuric acid", "4-hydroxybutyric acid", 
                      "4-hydroxybenzoate", "3-ureidopropionate", "3-phosphoglycerate", 
                      "3-methyl-1-oxobutylglycine", "3-hydroxypropionic acid", "3-hydroxybutanoic acid", 
                      "3,6-dihydro-3,6-dimethyl-2,5-bishydroxypyrazine", "3,6-anhydrogalactose", 
                      "3,6-anhydro-D-hexose", "2-oxogluconic acid", "2-monoolein", 
                      "2-ketoisocaproic acid", "2-hydroxyvaleric acid", "2-hydroxyglutaric acid", 
                      "2-hydroxybutanoic acid", "2-hydroxy-2-methylbutanoic acid", 
                      "2-deoxytetronic acid", "2-aminoadipic acid", "2,3-dihydroxybutanoic acid", 
                      "1-monostearin", "1-monopalmitin", "1-monoolein", "1,5-anhydroglucitol"
    ), from = "Chemical Name", to = "KEGG", db_name = "ctsgetr.sqlite")
    
  }
  
  #new db
  db_name<- 'ctsgetr.sqlite'
  # init_CTSgetR_db(db_name)
  
  # db_name <- 'C:/Users/think/Dropbox/Software/dave/dave/dave.network/ctsgetr.sqlite'
  # db_name<- 'ctsgetr.sqlite'
  id<-c("uridine",'uridine','foo')
  from<-"Chemical Name"
  to<- c( "KEGG","BioCyc")
  
  CTSgetR_format(id, from, to, format=TRUE, key_split=TRUE)
  
  CTSgetR(id,from,to,db_name=db_name)
  x<-do.call('CTSgetR',list(id,from,to,db_name))
  
  missing<-apply(is.na(x),2,sum)
  if(any(missing)>0){
    paste0('There were',)
  }
  
  (x<-do.call('CTSgetR',example_input()))
  #spread
  library(reshape2)
  
  
  melted<-dcast(x[!duplicated(x),],id~to,value.var = 'key')
  left_join(data.frame(id=id),melted)
  
  db_stats(TRUE, db_name=db_name)
  #check data base
  db<-db_stats(TRUE, db_name=example_input()$db_name)
  
  db %>%
    filter(target_id == 'DRTQHJPVMGBUCF-XVFCMESISA-N')
  
  db %>%
    filter(source_id == 'DRTQHJPVMGBUCF-XVFCMESISA-N')
  
  #map: 
  id<-c("alanine",'lactic acid')
  from<-"Chemical Name"
  to<- c( "PubChem CID", "KEGG","Human Metabolome Database")
  
  CTSgetR(id,from,to)
  
  tmp1<-list(id=id,from=from,to=to)
  
  #many to
  #treat all as data frame input
  #map by row for consistency
  
  
  
  library(tidyr)
  
  
  (tmp1<- CTSgetR_format(id,from,to))
  
  tmp2 <-
    list(
      id = c('HMDB0000161'),
      from = 'Human Metabolome Database',
      to = c('KEGG', 'PubChem CID')
    ) %>% do.call('CTSgetR_format', .)
  
  
  
  CTSgetR(tmp2)
  
  tmp3<-rbind(tmp1, tmp2) 
    
  CTSgetR(tmp3)  
  
 
  
}
