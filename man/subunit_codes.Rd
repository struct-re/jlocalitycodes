\name{subunit_codes}
\alias{subunit_codes}
\title{Get information on all sub-units of a locality}
\usage{
subunit_codes(code, date = Sys.Date(), class = NULL, sub.prefs = FALSE)
}
\arguments{
  \item{code}{A single JIS prefecture or municipality
  code.}

  \item{date}{A Date object.}

  \item{class}{Administrative class to filter for. Omit the
  ‘sacs:’ prefix.}

  \item{sub.prefs}{Logical. Whether to include
  sub-prefectures (shichou, shinkoukyoku) in the search.
  They are excluded by default.}
}
\value{
A data frame.
}
\description{
Get information on all sub-units of a locality
}

