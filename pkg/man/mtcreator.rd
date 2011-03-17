\name{mtcreator}
\Rdversion{1.1}
\alias{mtcreator}
\title{Creating MAGE-TAB files using mtcreator}
\description{This package contains a program that supports users in creating files to comply the MIAME-standard compliant format MAGE-TAB. This format enables the user to upload his high dimensional and related metadata to the public database Array Express. The upload is required before you publish the data or results that has been generated using the data.}
\usage{
mtcreator( interact = FALSE,
              rawbool = FALSE,
              rawlocation = ".",
              expressionset,
              output_path = "",
              ...)
}
\arguments{
  \item{interact}{logical, use interactive mode or command line call.}
  \item{rawbool}{use rawdata or not}
  \item{rawlocation}{location of the raw data}
  \item{expressionset}{name of the expressionset in your workspace}
  \item{output_path}{path from current location to the folder where the programs output will be placed}
  \item{\dots}{additional input parameters(not yet implemented, will be available in a future version)}
}
\details{
The call runs the whole procedure. The files have to be checked afterwards manually
}
\references{
\url{http://mtcreator.r-forge.r-project.org/}
}
\author{
Fabian Grandke <ben4571@ibe.med.uni-muenchen.de>,
}
\examples{
\dontrun{
  mtcreator(expressionset = test,output_path="./out")
}

}