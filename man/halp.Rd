\name{halp} 
\alias{halp}

\title{Show certain function comments}
\description{\code{halp} allows to quickly show a certain type of comments from function bodies as a \dQuote{live-help}.}
\usage{halp(fun, use.pager = logical(0), character.only = FALSE, ...)}
\arguments{
   \item{fun}{the name of a function given as a name or literal character string, or a character string, depending on whether \code{character.only} is \code{FALSE} (default) or \code{TRUE}.}
   \item{use.pager}{a logical indicating whether the output should be displayed on the pager (\code{TRUE}) instead of the screen (\code{FALSE}). If \code{NULL} is given, this will be decided based on the length of the halp text.}
   \item{character.only}{a logical indicating whether \code{fun} can be assumed to be a character string.}
   \item{...}{additional arguments (e.g. an environment) for the \code{get}-function used to fetch the function object.}
}

\details{This package is intented to easily provide some basic documentation system for functions which are not (yet) part of a package. During the development of a larger project there will naturally be more and more functions around created by the user. However, such functions often lack basic documentation, as this is only possible for in-package functions. The idea of this package is to also provide non-package functions with a simple help function similar to \code{help} a.k.a. \code{?} which is accessible via R command line. This can be done by adding special comment lines (starting with \code{#!} by default) into function definitions. When called, \code{halp} then checks the function body of a given function for those special comments and displays them, filtering the rest (i.e. also normal comments) out. However being simple, developers may find this method useful as some well-formulated \code{halp} comment lines can give all the information for the correct usage, important notes etc. for the function without having to go through all the source code again.

The settings for this package may be changed by the function \code{halp.options}.}

\value{\code{halp} (invisibly) returns a character vector containing the lines with the halp comments.}

\author{Daniel Haase (\email{dh@haase-zm.de}).}

\note{Note that \code{halp} only works for functions for which the original source code is known (attribute \code{source} of the function, see \code{attr}). Also, \code{options("keep.source")} must be \code{TRUE} (which it is by default for interactive sessions).}

\seealso{\code{\link{halp.options}} for some settings and \code{options("keep.source")} to check if function sources are kept (\code{TRUE} by default for interactive sessions).}

\examples{
# this is set to TRUE by default for interactive R sessions
options("keep.source" = TRUE)

# let us define a new function
yetAnotherFunction <- function(x, y) {
   #! This function calculates 2 * x - y but returns a random number!
   # This is a normal comment which will not be displayed by 'halp'
   2 * x - y
   runif(1)
}

# and check its 'halp' comments starting with '#!'
halp(yetAnotherFunction)
halp("yetAnotherFunction")
fun.name <- "yetAnotherFunction"
halp(fun.name, character.only = TRUE)

# now let's change some settings ...
screen.pre.bak <- halp.options("screen.pre")
halp.options(screen.pre = "~")

# ... and have a look at the differences
halp(yetAnotherFunction)

# so how do we change the indicator to tell 'halp'
# which comments to display?
pattern.bak <- halp.options("pattern")
halp.options(pattern = '^[[:space:]]*###[[:space:]]*')

# note that now no comments from 'yetAnotherFunction' will be displayed,
# as we just changed the 'halp' comment pattern
halp(yetAnotherFunction)

# so let's try another function with this new pattern
againAnotherFunction <- function(z) {
   # normal comment, will not be shown by 'halp'
   #! this will also not be shown, as we just changed the pattern
   z <- z^2 + 1
   ### however, this line will be shown by 'halp' now
   # note, that you can also mix code lines, normal comments and
   # 'halp' comments
   z
   ### see?
}

halp(againAnotherFunction)

# let's reset the options to the previous settings
halp.options(screen.pre = screen.pre.bak)
halp.options(pattern = pattern.bak)

}

\keyword{documentation}