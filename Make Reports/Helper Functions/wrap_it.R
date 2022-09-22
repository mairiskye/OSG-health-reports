#wrap axis labels 
wrap_it <- function(strings, width) {
  sapply(strings,function(y) paste(strwrap(y, width),
                                   collapse = "\n"),
         USE.NAMES = FALSE)
}
