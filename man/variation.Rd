\name{variation}
\alias{variation}

\title{
Crop coverage variation (in potato plots with two rows)
}
\description{Estimation of plot variation, the ratio between crop coverage of two rows of potatoes, from a series of PNG images 
}
\usage{
variation(folderIn, R=TRUE, folderOut)
}

\arguments{
  \item{folderIn}{Path to folder where the PNG images are located }
  \item{R}{Indication whether output should be in R (TRUE) or in a seperate file (FALSE). Defaults to TRUE}
  \item{folderOut}{Path to folder where the output file should be placed}
}

\details{The funtion takes a folder of binary PNG images, i.e., crop coverage images from PlotCut. Each image is split in two equally sized images (the two rows of potatoes) and compared in terms of crop coverage.}

\value{
  A data set with three columns: The plotname, variation.between and variation.within. Variation.between is variation estimated from splitting the image in the two rows of potatoes. Variaiton.within is from splitting the image in the other direction. Values close to 1 indicates very low variation. 
}


\author{Signe M. Jensen}

%\note{}

%\seealso{}

%\examples{


%}
\keyword{plot variation}
\keyword{crop coverage}

\concept{phenotyping}