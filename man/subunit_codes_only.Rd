\name{subunit_codes_only}
\alias{subunit_codes_only}
\title{Get numeric codes for all sub-units of a locality}
\usage{
subunit_codes_only(code, date = Sys.Date(), class = NULL,
  sub.prefs = FALSE)
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
A vector.
}
\description{
Get numeric codes for all sub-units of a locality
}

