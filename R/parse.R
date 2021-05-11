guard <- function(x){
  attr(x, "guard", TRUE)
}

is_assignment <- function(e){
  is.call(e) && as.character(e[[1]]) %in% c("<-", ":=", "=")
}

is_select <- function(e){
  is.call(e) && e[[1]] == "["
}

dt_assign <- function(e){
  if (!is_assignment(e)){
    return(TRUE)
  }

  g <- guard(e)
  v <- e[[2]]
  if (is.call(v)){
    if (!is_select(v)){
      stop("Invalid assignment: '", deparse(e), "'")
    }
    g <- Reduce(function(e1, e2){bquote(.(e1) & .(e2))}, c(g, v[[3]]))
    e[[2]] <- v[[2]]
  }
  e[[1]] <- as.symbol(":=")

  if (is.null(g)){
    bquote(dat[, .(e)])
  } else {
    bquote(dat[.(g), .(e)])
  }
}

dt_assign_char <- function(e){
  s <- deparse(e)
  r <- regexec("(.*)`\\:\\=`\\(([^,]+), (.+)\\)\\]", s)
  m <- regmatches(s, r)[[1]]
  paste0(m[2], m[3], " := ", m[4], "]")
}
