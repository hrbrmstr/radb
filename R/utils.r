read_rec <- function(con) {

  resp <- vector()

  repeat {
    line <- readLines(con, n=1L)
    # message(sprintf("[%s]", line))
    if (grepl("^[CDEF].*$", line)) break;
    resp <- c(resp, line)
  }

  if (grepl("^A[[:digit:]]", resp[1])) resp <- resp[-1]

  resp

}