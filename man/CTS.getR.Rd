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
a 3 column data frame containing containning:
from,  to and translated values
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

#translate from PubChem CID to InChIKey
id<-c(14242, 5760)
from<-"PubChem CID"
to<-"InChIKey"
CTS.getR(id,from,to)

#translate from InchI to pubchem CID
id<- "ZPUCINDJVBIVPJ-LJISPDSOSA-N"
from<-"InChIKey"
to<-"PubChem CID"

CTS.getR(id,from,to) #go ahead and google it



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Chemical Translation Service}
\keyword{CTS}% __ONLY ONE__ keyword per line
\keyword{InchI}
\keyword{CID}
