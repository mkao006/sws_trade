% Generated by roxygen2 (4.0.1): do not edit by hand
\name{balanceTradeQuantity}
\alias{balanceTradeQuantity}
\title{Function to balance trade quantity based on reliability}
\usage{
balanceTradeQuantity(data, import, export, reverseImport, reverseExport,
  reportingReliability, partnerReliability, pctTolerance)
}
\arguments{
\item{data}{The data table.}

\item{import}{The import quantity of the reporting country.}

\item{export}{The export quantity of the reporting country.}

\item{reverseImport}{The import quatity reported by the partner country.}

\item{reverseExport}{The export quantity reported by the partner country.}

\item{reportingReliability}{The column representing the calculated
reliability for the reporting country.}

\item{partnerReliability}{The column representing the calculated
reliability for the partner country.}

\item{pctTolerance}{Percentage, the tolerance level of discrepancy
between trading partner is deemd tolerable.}
}
\description{
If discrepency exists between trading partner, then the country
with higher reliability index will be taken.
}

