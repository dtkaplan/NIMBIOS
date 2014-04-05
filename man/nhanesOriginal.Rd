\name{nhanesOriginal}
\alias{nhanesOriginal}
\docType{data}
\title{
Body Shape and related measurements from NHANES 1999-2004 in their original format.
}
\description{
These data were collected as part of the NHANES 1999-2004 study.
}
\usage{data(nhanesOriginal)}
\format{
  A data frame with 31126 observations on the following 53 variables.
  \describe{
    \item{\code{seqn}}{matching code for NHANES mortality data}
    \item{\code{sex}}{2 is women}
    \item{\code{age}}{in years}
    \item{\code{preg}}{1 means pregnant}
    \item{\code{eth}}{ethnicity. 1=Mexican American, 2=Other Hispanic, 3=Non-Hispanic White, 4=Non-Hispanic Black, 5 = Other Race - Including Multi-Racial, .=Missing
}
    \item{\code{d}}{died.  Alive=0. 2=Cardiovascular cause of death. 1=other death}
    \item{\code{m}}{months of follow up (for mortality data)}
    \item{\code{smoke}}{1 means smoker}
    \item{\code{diab}}{1 means diabetic}
    \item{\code{hgt}}{height (meters)}
    \item{\code{wgt}}{weight (kilograms)}
    \item{\code{wst}}{waist circumference (meters)}
    \item{\code{wci}}{the proposed body shape index}
    \item{\code{bmi}}{body mass index}
    \item{\code{ptf}}{percent trunk fat}
    \item{\code{tf}}{mass of trunk fat}
    \item{\code{lf}}{limb fat}
    \item{\code{ll}}{limb lean tissue}
    \item{\code{lbmi}}{lean-tissue only BMI}
    \item{\code{fbmi}}{fat-only BMI}
    \item{\code{bbmi}}{bone BMI}
    \item{\code{pf}}{percent fat}
    \item{\code{bmd}}{bone mineral density}
    \item{\code{fmhm_other}}{Framingham risk score}
    \item{\code{hdl}}{HDL cholesterol}
    \item{\code{chol}}{cholesterol (LDL?)}
    \item{\code{bps}}{systolic blood pressure, mmHg}
    \item{\code{bpd}}{diastolic blood pressure, mmHg}
    \item{\code{income}}{ratio of family income to poverty threshold. 5 is ratio greater than or equal to 5}
    \item{\code{zhgt}}{z-score stratified by age and sex}
    \item{\code{zwgt}}{z-score stratified by age and sex}
    \item{\code{zwst}}{z-score stratified by age and sex}
    \item{\code{zwci}}{z-score stratified by age and sex}
    \item{\code{zbmi}}{z-score stratified by age and sex}
    \item{\code{zptf}}{z-score stratified by age and sex}
    \item{\code{ztf}}{z-score stratified by age and sex}
    \item{\code{zlf}}{z-score stratified by age and sex}
    \item{\code{zll}}{z-score stratified by age and sex}
    \item{\code{zlbmi}}{z-score stratified by age and sex}
    \item{\code{zfbmi}}{z-score stratified by age and sex}
    \item{\code{zbbmi}}{z-score stratified by age and sex}
    \item{\code{zpf}}{z-score stratified by age and sex}
    \item{\code{zbmd}}{z-score stratified by age and sex}
    \item{\code{zfmhm_other}}{z-score stratified by age and sex}
    \item{\code{zhdl}}{z-score stratified by age and sex}
    \item{\code{zchol}}{z-score stratified by age and sex}
    \item{\code{zbps}}{z-score stratified by age and sex}
    \item{\code{zbpd}}{z-score stratified by age and sex}
    \item{\code{zincome}}{z-score stratified by age and sex}
    \item{\code{pw}}{population weight, for adding up over the US population }
    \item{\code{psu}}{primary sampling unit}
    \item{\code{stratum}}{sampling stratum}
    \item{\code{zwh}}{z-score of waist/height ratio}
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
\examples{
data(nhanesOriginal)
## maybe str(nhanesOriginal)
}
\keyword{datasets}
