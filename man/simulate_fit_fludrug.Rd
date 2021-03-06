% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_fit_fludrug.R
\name{simulate_fit_fludrug}
\alias{simulate_fit_fludrug}
\title{Fitting a simple viral infection model with 2 types of drug mechanisms to influenza data}
\usage{
simulate_fit_fludrug(
  U = 1e+05,
  I = 0,
  V = 1,
  dI = 2,
  dV = 4,
  b = 0.001,
  blow = 1e-06,
  bhigh = 1000,
  p = 0.01,
  plow = 1e-06,
  phigh = 1000,
  g = 0.001,
  glow = 0,
  ghigh = 10000,
  e = 0.8,
  fitmodel = 1,
  iter = 100
)
}
\arguments{
\item{U}{: initial number of uninfected target cells : numeric}

\item{I}{: initial number of infected target cells : numeric}

\item{V}{: initial number of infectious virions : numeric}

\item{dI}{: rate at which infected cells die : numeric}

\item{dV}{: rate at which infectious virus is cleared : numeric}

\item{b}{: rate at which virus infects cells : numeric}

\item{blow}{: lower bound for infection rate : numeric}

\item{bhigh}{: upper bound for infection rate : numeric}

\item{p}{: rate at which infected cells produce virus : numeric}

\item{plow}{: lower bound for virus production rate : numeric}

\item{phigh}{: upper bound for virus production rate : numeric}

\item{g}{: unit conversion factor : numeric}

\item{glow}{: lower bound for unit conversion factor : numeric}

\item{ghigh}{: upper bound for unit conversion factor : numeric}

\item{e}{: drug efficacy (between 0-1) : numeric}

\item{fitmodel}{: fitting model 1 or 2 : numeric}

\item{iter}{: max number of steps to be taken by optimizer : numeric}
}
\value{
The function returns a list containing the best fit timeseries,
the best fit parameters, the data and the AICc for the model.
}
\description{
This function fits the simulate_virusandtx_ode model,
which is a compartment model
using a set of ordinary differential equations.
The model describes a simple viral infection system in the presence of drug treatment.
The user provides initial conditions and parameter values for the system.
The function simulates the ODE using an ODE solver from the deSolve package.
}
\details{
A simple compartmental ODE models describing an acute viral infection with drug treatment
mechanism/model 1 assumes that drug treatment reduces rate of new cell infection
mechanism/model 2 assumes that drug treatment reduces rate of new virus production.
}
\section{Warning}{
 This function does not perform any error checking. So if
  you try to do something nonsensical (e.g. specify negative parameter or starting values),
  the code will likely abort with an error message.
}

\examples{
# To run the code with default parameters just call the function:
\dontrun{result <- simulate_fit_fludrug()}
# To apply different settings, provide them to the simulator function, like such:
result <- simulate_fit_fludrug(iter = 5, fitmodel = 1)
}
\seealso{
See the Shiny app documentation corresponding to this
function for more details on this model.
}
\author{
Andreas Handel
}
