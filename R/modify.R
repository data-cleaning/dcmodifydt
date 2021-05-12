#' Modify records in a data.table
#'
#' Modify records in a data.table using modification rules specified
#' in a modifier object.
#'
#' This is a more efficient implementation then coercing the data.table to a
#' data.frame and use that implementation.
#' @param dat [data.table()] object
#' @param x `dcmodify::modifier` object.
#' @param copy if `TRUE` modify copy of table.
#' @param sequential if `TRUE` (default), steps will be executed in sequence, so order matters.
#' @param ... unused
#' @example ./example/modify.R
#' @importFrom dcmodify modify
#' @import data.table
#' @family modify
#' @export
setMethod("modify", signature("data.table", "modifier")
         , function(dat, x, copy = NULL, sequential = TRUE, ...){
  modify.data.table(dat = dat, x = x, copy = copy, sequential = sequential, ...)
})

modify.data.table <- function(dat, x, copy, sequential, ...){
  if (is.null(copy)){
    warning("`copy` not specified, setting to `TRUE`.\n"
           ,"Specify `copy` to remove this warning."
           , call. = FALSE
           )
    copy <- TRUE
  }

  if (isTRUE(copy)){
    dat <- data.table::copy(dat)
  }

  if (!isTRUE(sequential)){
    warning("`sequential=FALSE` switches to data.frame method.\n"
           , "Working on copy of data.table."
           , call. = FALSE
           )
    dat <- data.table::setDF(dat)
    dat <- dcmodify::modify(dat, x, sequential = sequential, ...)
    dat <- data.table::setDT(dat)
    return(dat)
  }

  dt_assigns <- lapply(x$assignments(), dt_assign)

  for (a in dt_assigns){
    # changes `dat` "in place"...
    tryCatch({
      eval(a)
    }, error=function(e){
      warning("'", dt_assign_char(a), "' failed and skipped: \n"
             , 'error: "', e$message, '"'
             , call. = FALSE
             )
    })
  }

  dat
}

#' modifies data.table in place
#'
#' modifies data.table in place, alias for `modify` with `copy=TRUE` and
#' `sequential=TRUE`. It follows the naming convention in `data.table` to
#' prefix methods that change objects (byreference) with `set`.
#' @param dat [data.table()] object
#' @param x `dcmodify::modifier` object.
#' @param ... not used
#' @export
#' @family modify
#' @example ./example/modify.R
setmodify <- function(dat, x, ...){
  modify.data.table(dat = dat, x = x, copy = FALSE, sequential = TRUE, ...)
  invisible(dat)
}
