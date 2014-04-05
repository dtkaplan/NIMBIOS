\name{NHANES}
\alias{NHANES}
\docType{data}
\title{
Body shape, mortality, and related measurements from NHANES 1999-2004 in a friendlier format.
}
\description{
These data were collected as part of the NHANES 1999-2004 study.
}
\usage{data(NHANES)}
\format{
  A data frame with 31126 observations on the following 17 variables.
  \describe{
    \item{\code{sex}}{categorical}
    \item{\code{age}}{in years}
    \item{\code{pregnant}}{categorical}
    \item{\code{died}}{Whether the person is alive at the time of follow up.  Deaths are divided into cardiovascular and non-cardiovascular.}
    \item{\code{followup}}{months of follow up (for mortality data)}
    \item{\code{smoker}}{categorical}
    \item{\code{diabetic}}{categorical}
    \item{\code{heigt}}{height (meters)}
    \item{\code{weigt}}{weight (kilograms)}
    \item{\code{BMI}}{body mass index}
    \item{\code{trunkfat}}{percent trunk fat}
    \item{\code{percentfat}}{percent fat}
    \item{\code{leanBMI}}{lean-tissue only BMI}
    \item{\code{framingham}}{Framingham risk score}
    \item{\code{HDL}}{HDL cholesterol}
    \item{\code{LDL}}{cholesterol}
    \item{\code{systolic}}{systolic blood pressure, mmHg}
    \item{\code{diastolic}}{diastolic blood pressure, mmHg}
    \item{\code{income}}{ratio of family income to poverty threshold. 5 is ratio greater than or equal to 5}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
These data were assempled from NHANES 1999-2004 sources by Dr. Nir Krakauer at City College of New York.
}
\references{Nir Y. Krakauer, Jesse C. Krakauer, "A New Body Shape Index Predicts Mortality Hazard Independently of Body Mass Index" PloS one, 7 (7) PMID: 22815707, 2012.
\url{http://www.plosone.org/article/info:doi/10.1371/journal.pone.0039504}
}
\seealso{\code{nhanesOriginal}, which has some additional variables, including the body shape index proposed in the Krakauer reference.}
\examples{
data(NHANES)
str(NHANES)
}
\keyword{datasets}
