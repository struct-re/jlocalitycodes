\name{desig_city_codes}
\alias{desig_city_codes}
\title{Get the JIS codes and names for all Japanese 'designated
cities' at a given date.

Designated cities (seireishiteitoshi) are the major cities, as
designated by a government ordinance, that are subdivided into
non-autonomous wards (ku). Tokyo is not a designated city, but a
prefecture comprising (among other municipalities) 23 city-level
so-called special wards.}
\usage{
desig_city_codes(date = Sys.Date())
}
\arguments{
  \item{date}{a Date object.}
}
\value{
JIS codes and names of all Japanese "ordinance-designated
cities" (a list of major cities other than Tokyo) at the
given date.
}
\description{
Get the JIS codes and names for all Japanese 'designated
cities' at a given date.

Designated cities (seireishiteitoshi) are the major cities,
as designated by a government ordinance, that are
subdivided into non-autonomous wards (ku). Tokyo is not a
designated city, but a prefecture comprising (among other
municipalities) 23 city-level so-called special wards.
}

