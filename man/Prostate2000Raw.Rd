%%  WARNING: This file was automatically generated from the associated 
%%  Prostate2000.mid file. Do NOT edit this Rd file to make a change. 
%%  Instead, edit the Prostate2000.mid file in the project MID directory. 
%%  Once the Prostate2000.mid file has been updated, this Rd file, and all 
%%  other documentation (such as corresponding LaTeX, SGML and HTML 
%%  documentation) should be regenerated using the mid.pl Perl script. 

%% R documentation for the Prostate2000Raw function

\name{Prostate2000Raw}
\alias{Prostate2000Raw}
\title{Prostate Cancer 2000 Raw Spectra}
\concept{prostate cancer}
\description{A data object of class \code{msSet},
consisting of 654 mass spectra (327 spectra in duplicate) from 2000 to 20000 Da,
which were generated from patients with
prostate cancer, benign prostatic hypertrophy, and normal controls.
These spectra are already baseline corrected and normalized.
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

\seealso{\code{\link{Prostate2000Peaks}}.}

\examples{
if (require("msProcess")) {
if (!exists("Prostate2000Raw")) data("Prostate2000Raw", package="msProstate")

## plot a few spectra 
plot(Prostate2000Raw, subset=1:8)

## image of a subset of spectra 
image(Prostate2000Raw, subset=1:64)
}
}
\keyword{datasets}
