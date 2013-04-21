\name{CTS.getR}
\alias{CTS.getR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tranlate between chemical identifiers.
}
\description{
Use the Chemical Translation Service (CTS) to ranslate between the following molecular identifiers: "Chemical Name","InChIKey","InChI Code","PubChem CID","Pubchem SID","ChemDB","ZINC","Southern Research Institute","Specs","MolPort","ASINEX","ChemBank","MLSMR","Emory University Molecular Libraries Screening Center","ChemSpider","DiscoveryGate","Ambinter","Vitas-M Laboratory","ChemBlock".
}
\usage{
CTS.getR(id, from, to, progress = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
a vector of idenitifiers
}
  \item{from}{
type of object translating from
}
  \item{to}{
type of object translating to
}
  \item{progress}{
show progress
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a 3 column data frame containing [1] from, [2] to and [3] translated values
}
\references{
http://uranus.fiehnlab.ucdavis.edu:8080/cts-2.0-beta-2/
}
\author{
Dmitry Grapov
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#translate between PubChem IDs
from<-"PubChem CID"
#the following IDs
id<-c(14242, 5760)
#into InchIKeys
to<-"InChIKey"

#translate
CTS.getR(id,from,to)

## The function is currently defined as
function (id, from, to, progress = TRUE) 
{
    if (require(RCurl) == FALSE) {
        install.packages("RCurl")
        library(RCurl)
    }
    else {
        library(RCurl)
    }
    if (require(RJSONIO) == FALSE) {
        install.packages("RJSONIO")
        library(RJSONIO)
    }
    else {
        library(RJSONIO)
    }
    if (progress == TRUE) {
        pb <- txtProgressBar(min = 0, max = length(id), style = 3)
    }
    start <- "http://uranus.fiehnlab.ucdavis.edu:8080/cts-2.0-beta-2/service/convert"
    out <- lapply(1:length(id), function(i) {
        if (progress == TRUE) {
            setTxtProgressBar(pb, i)
        }
        url <- paste(start, from, to, id[i], sep = "/")
        url <- gsub("\\ ", "\%20", url)
        out <- as.character(unlist(tryCatch(getURL(url, ssl.verifypeer = FALSE), 
            error = function(e) {
                "error"
            })))
        tryCatch(data.frame(fromJSON(out))[, 1, drop = FALSE], 
            error = function(e) {
                data.frame(matrix("error", nrow = 3))
            })
    })
    tmp <- do.call("cbind", out)
    tmp <- cbind(from = id, to = to, result = as.character(unlist(tmp[4, 
        ])))
    colnames(tmp) <- c(paste("from", from, sep = " :"), "to:", 
        "RESULT")
    if (progress == TRUE) {
        close(pb)
    }
    return(tmp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
