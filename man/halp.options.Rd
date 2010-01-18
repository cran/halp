\name{halp.options} 
\alias{halp.options}

\title{Get/set options for the halp-package.}
\description{Get/set options for the \code{halp}-package.}
\usage{halp.options(x = NULL, ...)}
\arguments{
   \item{x}{a character string holding an option name.}
   \item{...}{any options can be defined using \code{name = value}.}
}
\details{\code{halp.options} can be used to either get one option, or to set some options. Note that only one option can be retrieved at a time (in the case that \code{x} is not \code{NULL}), however setting multiple options in one call is possible. Also, getting and setting options is not possible in one call.

Following options are currently used: \code{pattern}, which denotes the comment pattern (\code{^[[:space:]]*#![[:space:]]} by default), \code{screen.pre} the prefix for \code{halp} comment lines displayed on the screen (\code{" :: "} by default), \code{pager.minLines} the minimum number of \code{halp} comment lines for which the pager will be used for the display instead of the screen (\code{10} by default) and \code{pager.pre}, which is the line prefix for pager display (\code{""} by default).}

\value{If a value is retrieved, then \code{halp.options} will return the value for the key given as character string in \code{x}. Otherwise, the list with all options is returned invisibly. If \code{halp.options} is called with no argument at all, the list with all options is returned.}

\author{Daniel Haase (\email{dh@haase-zm.de}).}

\seealso{\code{\link{halp}} for examples and general package informations.}