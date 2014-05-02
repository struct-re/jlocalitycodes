\name{is_valid_jlocalitycode}
\alias{is_valid_jlocalitycode}
\title{Tests that its argument is a syntactically valid JIS
municipality or prefecture code

This function does not check whether the code is really in use,
just whether it is well-formed (i.e., that it has the right number
of digits and a prefecture number between 1 and 47).}
\usage{
is_valid_jlocalitycode(code)
}
\arguments{
  \item{code}{Character or numeric code}
}
\value{
Logical
}
\description{
Tests that its argument is a syntactically valid JIS
municipality or prefecture code

This function does not check whether the code is really in
use, just whether it is well-formed (i.e., that it has the
right number of digits and a prefecture number between 1
and 47).
}

