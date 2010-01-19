".halp.Options" <- list(
                      "pattern" = '^[[:space:]]*#![[:space:]]*',
                      "screen.pre" = "",
                      "pager.minLines" = 10,
                      "pager.pre" = ""
                   )

"halp.options" <- function(x = NULL, ...) {
   opt <- get(".halp.Options", envir = as.environment("package:halp"), mode = "list", inherits = FALSE)
   if (!is.null(x)) {
      # get option
      opt[[x]]
   } else {
      values <- list(...)
      if (length(values) == 0) {
         # return entire option list
         opt
      } else {
         # set options
         keys <- names(values)
         for (i in 1:length(values)) {
            opt[[keys[i]]] <- unlist(values[i], use.names = FALSE)
            assign(".halp.Options", opt, envir = as.environment("package:halp"), inherits = FALSE)
         }
      }
   }
}

"halp" <- function(fun, use.pager = logical(0), character.only = FALSE, ...) {
   if (!is.logical(use.pager)) {
      stop("Argument \'use.pager\' must be of type logical")
   }
   
   if (!is.logical(character.only)) {
      stop("Argument \'character.only\' must be of type logical")
   }
   
   # get the function name
   if (is.character(fun) || all(character.only)) {
      fun.name <- fun
   } else {
      fun.name <- deparse(substitute(fun))
   }
   
   # get the function object
   fun.obj <- get(fun.name, mode = "function", ...)
   
   # get the function source code
   fun.source <- attr(fun.obj, "source")
   
   # get the halp option function object
   opt <- get("halp.options", envir = as.environment("package:halp"), mode = "function", inherits = FALSE)
   
   # check if pattern is null
   if (is.null(opt("pattern"))) {
      stop("Invalid comment pattern for the \'halp\' package, see \'?halp.options\'")
   }
   
   # source code may not be available for internal functions etc., or if
   # options("keep.source") was not set to TRUE when function was defined
   if (is.null(fun.source)) {
      stop("Source code for function \'", fun.name,
           "\' is not available\n", sep = "")
   }
   
   # this is a character vector containing all the halp lines
   # (each line is one vector element)
   fun.halp.raw <- gsub(opt("pattern"), "", fun.source[grep(opt("pattern"), fun.source)])
   
   if (length(fun.halp.raw) > 0) {
      if ((length(use.pager) > 0 && all(use.pager)) || (length(use.pager) == 0 && length(fun.halp.raw) >= opt("pager.minLines"))) {
         # show the halps in the pager (must be done via temporary file)
         files <- tempfile()
         if (is.null(files)) {
            stop("Could not acquire temporary filename for pager display")
         }
         file <- files[1]
         cat(paste(opt("pager.pre"), fun.halp.raw, collapse = "\n", sep = ""), "\n", sep = "", file = file)
         file.show(file)
      } else {
         # show the halps on the screen
         cat(paste(opt("screen.pre"), fun.halp.raw, collapse = "\n", sep = ""), "\n", sep = "")
      }
   } else {
      warning("The are no halp comments for function \'", fun.name, "\'", call. = FALSE)
   }
   
   invisible(fun.halp.raw)
}