% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CTS.R
\name{CTSgetR}
\alias{CTSgetR}
\title{CTSgetR}
\usage{
CTSgetR(id, from, to, db_name = "ctsgetr.sqlite", ...)
}
\arguments{
\item{id}{a vector of metabolite identifier(s) or name(s) to translate see \code{\link{valid_to}} and \code{\link{valid_from}}}

\item{from}{Database name describing \code{id} see \code{\link[CTSgetR]{valid_from}}}

\item{to}{Database name to translate \code{id} see \code{\link[CTSgetR]{valid_to}}}

\item{db_name}{string path for sqlite database to store cached results in see \code{\link[CTSgetR]{valid_to}}}
}
\value{
data frame of results including \code{from} (fromIdentifier), \code{id} (searchTerm), \code{to} (toIdentifier) and translated values
}
\description{
CTSgetR
}
\details{
Interface to CTS (http://cts.fiehnlab.ucdavis.edu/) for metabolite identifier translation between
 > 200 of the most common biological databases including: Chemical Name, InChIKey, PubChem CID,
 ChemSpider, BioCyc, ChEBI, CAS, HMDB, KEGG and LipidMAPS.
}
\examples{
\dontrun{
id<-c("C15973","C00026","C05381")
from<-"KEGG"
to<-"PubChem CID"
CTSgetR(id,from,to)
}
}
\seealso{
\code{\link[CTSgetR]{single_CTSgetR}}
}
