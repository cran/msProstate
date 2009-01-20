%%  WARNING: This file was automatically generated from the associated 
%%  Prostate2000.mid file. Do NOT edit this Rd file to make a change. 
%%  Instead, edit the Prostate2000.mid file in the project MID directory. 
%%  Once the Prostate2000.mid file has been updated, this Rd file, and all 
%%  other documentation (such as corresponding LaTeX, SGML and HTML 
%%  documentation) should be regenerated using the mid.pl Perl script. 

%% R documentation for the Prostate2000Peaks function

\name{Prostate2000Peaks}
\alias{Prostate2000Peaks}
\title{Prostate Cancer 2000 Peaks}
\concept{prostate cancer}
\description{A data object of class \code{msSet}, consisting	of 779 peaks
detected in 652 spectra (326 spectra in duplicate) from 2000 to 40000 Da.
The peak detection was performed by East Virginia Medical School.
Please see the references for more details.}

\references{
B.L. Adam, Y. Qu, J.W. Davis, M.D. Ward, M.A. Clements, L.H. Cazares,
O.J. Semmes, P.F. Schellhammer, Y. Yasui, Z. Feng, and G.L. Wright, Jr.,
``Serum protein fingerprinting coupled with a pattern-matching algorithm
distinguishes prostate cancer from benign prostate hyperplasia and healthy men,"
\emph{Cancer Research}, 62(13):3609--14, 2002.

Y. Qu, B.L. Adam, Y. Yasui, M.D. Ward, L.H. Cazares,
P.F. Schellhammer, Z. Feng, O.J. Semmes, and G.L. Wright Jr.,
``Boosted decision tree analysis of surface-enhanced laser desorption/ionization
mass spectral serum profiles discriminates prostate cancer from noncancer patients,"
\emph{Clinical Chemistry}, 48(10):1835--43, 2002.
}

\seealso{\code{\link{Prostate2000Raw}}.}

\examples{
if (require("msProcess")) {
if (!exists("Prostate2000Peaks")) data("Prostate2000Peaks", package="msProstate")

## plot a few spectra 
plot(Prostate2000Peaks, subset=1:16)

## image of a subset of spectra 
image(Prostate2000Peaks)
}
}
\keyword{datasets}
