\name{CTSgetR}
\alias{CTSgetR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tranlate between chemical identifiers.
}
\description{
Use the Chemical Translation System (CTS) to translate between > 250 of the most common biological database identifiers including: Chemical Name, InChIKey, InChI Code, PubChem CID, Pubchem SID, ChemDB, ChemSpider, Golm Metabolome Database (GMD), BioCyc, ChEBI, CAS, HMDB, KEGG, LipidMAPS.
}
\usage{
CTSgetR(id,from,to,parallel,limit.values,server)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
matrix or dataframe of idenitifiers to translate
}
  \item{from}{
single value describing type or databse name describing "id". See CTS.options() for all options.
}
  \item{to}{
single value describing type or databse name to translate id "to". See CTS.options() for all options.
}
  \item{parallel}{
calculate translations in parallel using multiple CPU cores limit.values
}
 \item{limit.values}{
limit translation returned values to a single value
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a 3 column data frame containing containning:
from, to and translated values
}
\references{
https://github.com/dgrapov/CTSgetR
}
\author{
Dmitry Grapov
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
CTS.options()
}
\examples{
#translate from PubChem CID to InChIKey
id<-c(14242, 5760)
from<-"PubChem CID"
to<-"InChIKey"
CTSgetR(id,from,to)

#translate from InchI to pubchem CID
id<- "ZPUCINDJVBIVPJ-LJISPDSOSA-N"
from<-"InChIKey"
to<-"PubChem CID"
CTSgetR(id,from,to) 
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ Chemical Translation Service}
%\keyword{CTS}% __ONLY ONE__ keyword per line
%\keyword{InchI}
%\keyword{CID}
