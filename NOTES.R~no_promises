

#debug
library(CTSgetR)
CTSgetR::CTSgetR(id=c("C15973","C00026","C05381"), from=c("KEGG"), to = c("PubChem CID"))

nano #docker cred error
~/.docker/config.json # remove credsStore

want<-'sm'
possible_to<-unlist(valid_to())
possible_to[grepl(want,possible_to,ignore.case=TRUE)]
# CTSgetR::CTSgetR(id=c("C15973","C00026","C05381"), from=c("InChIKey"), to = c("PubChem CID"))

#inchi to smiles
webchem::get_csid  #https://docs.ropensci.org/webchem/reference/get_csid.html
rcdkTools::inchi2smiles   #https://rdrr.io/github/bachi55/rcdkTools/man/inchi2smiles.html - requires installing open babel



#TODO 
# add local copy and build to dockerfile
#change rstudio server login from opencpu 


#from WSL
cd /mnt/c/Users/think/Dropbox/Software/ARCHIVE/CTSgetR


#build base image from opencpu


#build 
export rstudio_pass=dave_777 # rstudio server password for user opencpu
docker-compose -f docker-compose.yml build --force-rm


#start service
export ctsgetr_db_mount=/mnt/c/Users/think/Dropbox/Software/ARCHIVE/CTSgetR
docker-compose -f docker-compose.yml up -d

#interactive debug
docker exec -ti ctsgetr bash

#might need to own mounted sqlite
sudo chown www-data /root/ctsgetr/.

#configure app armor
#http://bbroeksema.github.io/2014/11/20/using-sqlite-with-opencpu.html


#how to update the db?
#could use s3 to copy?

https://cts.fiehnlab.ucdavis.edu/rest/convert/KEGG/InchiKey/C15973,C00026


# Calling the API from R -----------------------------------------------------------------
library(ocpuclient)

base_url<-'http://localhost:8084/ocpu/'

endpoint<-'library/CTSgetR/R/heartbeat'
url<-paste0(base_url,endpoint)
curl http://localhost/ocpu/library/CTSgetR/R/heartbeat
post_ocpu(url=url)

#debug
endpoint<-'library/CTSgetR/R/api_debug'
url<-paste0(base_url,endpoint)

post_ocpu(url=url)

#db 
db_name<-'/root/ctsgetr/ctsgetr.sqlite'
# db_name<-'/ctsgetr/ctsgetr.sqlite'
# db_name<-'/ctsgetrdb/ctsgetr.sqlite'
endpoint<-'library/CTSgetR/R/db_stats'
url<-paste0(base_url,endpoint)
body<-list(data=FALSE, db_name=db_name)

post_ocpu(url=url,body=body)

#translate
endpoint<-'library/CTSgetR/R/CTSgetR'
url<-paste0(base_url,endpoint)

id <-
  c("C15973",
    "C00026")
from <- "KEGG"
to <- "PubChem CID"

body<-list(id=id,from=from,to=to,db_name=db_name)



post_ocpu(url=url,body=body)

do.call('CTSgetR',list(id=id,from=from,to=to))

#locking with a mounted db?
db_name<-'/ctsgetr/inst/ctsgetr.sqlite'

file.exists(db_name)
do.call('db_stats',list(db_name='/ctsgetrdb/ctsger.sqlite'))
db_stats(db_name=db_name1)

#tests
# endpoint<-'library/CTSgetR/R/api_echo'
endpoint<-'library/CTSgetR/R/CTSgetR'

url<-paste0(base_url,endpoint)


id <-
  c(
    "C15973",
    "C00026"
  )
from <- "KEGG"
to <- "PubChem CID"

#tests in R
body<-list(id=id,from=from,to=to)
body<-list(id=id,from=from,to=to,db_name='inst/ctsgetr.sqlite')
do.call('CTSgetR',body)

do.call('CTSgetR::db_stats',list())


#API
db_name<-'/ctsgetrdb/ctsgetr.sqlite'

db_fun<-function(){
  Sys.getenv('ctsgetr_db')
}


body<-list(id=id,from=from,to=to)

#client db path
body<-list(id=id,from=from,to=to,db_name=db_name)


#server db
body<-list(id=id,from=from,to=to,db_name=db_fun())

endpoint<-'library/CTSgetR/R/CTSgetR'
url<-paste0(base_url,endpoint)

post_ocpu(url=url,body=body)

#OLDER

(x<-do.call('api_echo',list(toJSON(body))))
str(x)
fromJSON(x)

curl(val)

res<-POST(url,body=body,encode='json',verbose())

res_headers<-httr:::parse_http_headers(res$headers)[[1]]$headers
res_url<-res_headers$location

locs<-readLines(curl(res_url))

all_out<-lapply(locs,function(i){
  
  url<-paste0(res_url,i)
  
  readLines(curl(url))
  
})

names(all_out)<-locs

all_out

tmp<-paste0(res_url,'R/.val')
(results<-fromJSON(readLines(curl(tmp))))

tmp<-paste0(res_url,'R/console')
(results<-fromJSON(readLines(curl(tmp))))



results<-GET(res_url)

(out<-cat(rawToChar(results$content)))

#get location
rawToChar(headers(res))


# fromJSON(readLines(rawToChar(results$content)))


GET('http://localhost:4000/ocpu/tmp/x02060018908b3d/R/.val')
