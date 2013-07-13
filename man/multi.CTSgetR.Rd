\name{multi.CTSgetR}
\alias{multi.CTSgetR}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Translate between multiple chemical identifiers.
}
\description{
Use the Chemical Translation System (CTS) to translate between > 200 of the most common biological database identifiers including: Chemical Name, InChIKey, InChI Code, PubChem CID, Pubchem SID, ChemDB, ChemSpider, Golm Metabolome Database (GMD), BioCyc, ChEBI, CAS, HMDB, KEGG, LipidMAPS.
}
\usage{
multi.CTSgetR(id,from,to,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
vector, matrix or dataframe of idenitifiers to translate 
}
  \item{from}{
single value stating databse name which for "id". See CTS.options() for all options.
}
  \item{to}{
a vector of one or more values describing identifiers or database names to translate id "to". See CTS.options() for all options.
}
  \item{...}{
see CTSgetR options 
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a 3 column data frame containing:
from, to and translated value
}
\references{
http://cts.fiehnlab.ucdavis.edu/
}
\author{
Dmitry Grapov
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
CTSgetR, CTS.options()
}
\examples{

id<-c(14242, 5760,1175)
from<-"PubChem CID"
to<-c("InChIKey", "BioCyc","HMDB", "KEGG","Chemical Name")
multi.CTSgetR(id,from,to)
}
