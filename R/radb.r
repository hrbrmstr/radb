#' Open a persistent connection to the Merit RADb server
#'
#' @md
#' @param timeout timeout in seconds
#' @param server server to connect to. Should not need to be modified.
#' @references [http://www.radb.net/support/query2.php](http://www.radb.net/support/query2.php)
#' @export
radb_open <- function(timeout = 3600, server = "whois.radb.net") {

  con <- sc(host=server, port=43L, blocking=TRUE, open="rw+", timeout=timeout)

  if (is.null(con$result)) {
    message("Error opening connection to Merit RADb server")
    return(NULL)
  }

  radb_obj <- list(con = con$result)
  class(radb_obj) <- "radb"

  cat("!!\n", file=radb_obj$con, sep="")
  cat("!nradb-R-package\n", file=radb_obj$con, sep="")

  junk <- read_rec(radb_obj$con)

  radb_obj <- radb_version(radb_obj)

  radb_obj

}

#' Get routes by origin
#'
#' @md
#' @param rdb `radb` object created with [radb_open]()
#' @param asn autonomous system. Can be a bare integer or prefixed with "`AS`" or "`as`"
#' @param ipv6 if `TRUE` then get IPv6 routes. Default `FALSE`
#' @references [http://www.radb.net/support/query2.php](http://www.radb.net/support/query2.php)
#' @export
radb_routes_by_origin <- function(rdb, asn, ipv6=FALSE) {

  asn <- gsub(" ", "", asn)
  asn <- gsub("[^[:digit:]]", "", asn)

  if (asn == "") return(rdb)

  asn <- sprintf("as%s", asn)

  ipv <- if (ipv6) "!6" else "!g"

  cat(sprintf("%s%s\n", ipv, asn), file=rdb$con, sep="")

  resp <- read_rec(rdb$con)

  rdb$rbo[[asn]] <- strsplit(resp, " ")[[1]]

  rdb

}

#' Match an object of the specified type with the specified key
#'
#' @md
#' @param rdb `radb` object created with [radb_open]()
#' @param key,value what to search by
#' @references [http://www.radb.net/support/query2.php](http://www.radb.net/support/query2.php)
#' @export
radb_match <- function(rdb, key, value) {

  key <- gsub(" ", "", key)
  value <- gsub(" ", "", value)
  query <- sprintf("%s,%s", key, value)
  cat(sprintf("!m%s\n", query), file=rdb$con, sep="")

  resp <- read_rec(rdb$con)

  read.csv(text=paste0(sub(":\ *", "\t", trimws(resp)), collaspe="\n"),
           sep="\t", header = FALSE, stringsAsFactors=FALSE) -> resp
  resp <- setNames(resp, c("key", "value"))
  resp$query_key <- key
  resp$query_val <- value

  rdb$match_res <- tibble::as_tibble(rbind.data.frame(rdb$match_res, resp))

  rdb

}

#' Perform route searches.
#'
#' @md
#' @param rdb `radb` object created with [radb_open]()
#' @param route CIDR"
#' @param option one of "`o`", "`l`", "`L`", "`M`". `o` - return origin of exact
#'        match(es); `l` - one-level less specific; `L` - all less specific; `M` -
#'        all more specific.
#' @references [http://www.radb.net/support/query2.php](http://www.radb.net/support/query2.php)
#' @export
radb_route_search <- function(rdb, cidr, option=c("o", "l", "L", "M")) {

  cidr <- gsub(" ", "", cidr)

  option <- match.arg(option, c("o", "l", "L", "M"))

  query <- sprintf("%s,%s", cidr, option)

  cat(sprintf("!r%s\n", query), file=rdb$con, sep="")

  resp <- read_rec(rdb$con)

  read.csv(text=paste0(sub(":\ *", "\t", trimws(resp)), collaspe="\n"),
           sep="\t", header = FALSE, stringsAsFactors=FALSE) -> resp
  resp <- setNames(resp, c("key", "value"))
  resp$cidr <- cidr
  resp$option <- option

  rdb$search_res <- tibble::as_tibble(rbind.data.frame(rdb$search_res, resp))

  rdb

}

#' Get as-set or route-set
#'
#' @md
#' @param rdb `radb` object created with [radb_open]()
#' @param as_set `as-set` to retrieve with optional "`AS-`" prefix
#' @param recursive recursively expand members?
#' @references [http://www.radb.net/support/query2.php](http://www.radb.net/support/query2.php)
#' @export
radb_route_set <- function(rdb, as_set, recursive=FALSE) {

  as_set <- gsub(" ", "", as_set)
  as_set <- tolower(as_set)
  as_set <- gsub("^as-", "", as_set)
  as_set <- sprintf("as-%s", as_set)

  as_set_p <- if (recursive) sprintf("%s,1", as_set) else as_set
  as_set_p <- sprintf("!i%s\n", as_set_p)

  cat(as_set_p, file=rdb$con, sep="")

  resp <- read_rec(rdb$con)

  rdb$as_set[[as_set]] <- strsplit(resp, " ")[[1]]

  rdb

}

#' Get RADb version
#'
#' Displays RADb version and adds it to the `radb` returned object.
#'
#' @md
#' @param rdb `radb` object created with [radb_open]()
#' @references [http://www.radb.net/support/query2.php](http://www.radb.net/support/query2.php)
#' @export
radb_version <- function(rdb) {

  cat("!v\n", file=rdb$con, sep="")
  vers <- read_rec(rdb$con)
  rdb$version <- vers
  rdb

}

#' Close the connection to the Merit RADb server
#'
#' @md
#' @param rdb `radb` object created with [radb_open]()
#' @references [http://www.radb.net/support/query2.php](http://www.radb.net/support/query2.php)
#' @export
radb_close <- function(rdb) {
  cat("!q\n", file=rdb$con, sep="")
  close(rdb$con)
  rdb$con <- NULL
  rdb
}
