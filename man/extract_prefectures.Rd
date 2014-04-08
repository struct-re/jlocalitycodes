\name{extract_prefectures}
\alias{extract_prefectures}
\title{Extract prefecture information from JIS locality codes.}
\usage{
extract_prefectures(codes, as.names = FALSE, lang = "ja")
}
\arguments{
  \item{codes}{Vector of JIS locality codes.}

  \item{as.names}{Logical. If set to \code{TRUE}, this
  function returns prefecture names. By default, it returns
  two-digit JIS prefecture codes.}

  \item{lang}{Desired language for prefecture names.}
}
\value{
A vector with the prefecture of each code in \code{codes}.
}
\description{
Extract prefecture information from JIS locality codes.
}

