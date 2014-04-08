\name{lookup_names}
\alias{lookup_names}
\title{Look up JIS prefecture or municipality codes matching a set
of place names.}
\usage{
lookup_names(names, date = Sys.Date())
}
\arguments{
  \item{names}{Vector of \code{character}. Locality names
  can be given in Japanese or Roman script (macrons
  omitted!) as long as the suffix is included, eg
  ‘Akiruno-shi‘.}

  \item{date}{Date object. If omitted, today’s date will be
  used.}
}
\value{
A data frame.
}
\description{
Look up JIS prefecture or municipality codes matching a set
of place names.
}
\details{
Look up the local authority codes for each place name in a
vector at a given date (default: today). If more than one
code is found for a given place name, they are all included
in the return value, but a warning is output. To enable
filtering, provide unambiguous place names in Japanese
script, including the prefecture and where applicable, the
district (gun) or designated city (see
\code{\link{desig_city_codes}}).
}

