
#' @title Parsing Cost
#' @aliases costs hybrid_costs is_hybrid
#' @description `costs` returns an object of class costs.
#' These are list columns where each row is a vector corresponding
#' to numeric symbols (0-99), coloured mana symbols (W, U, B, R, G)
#' or multicolour/hybrid symbols (wu, ub, etc.).
#' @param x An object
#' @return costs object (list) with attribute is_hybrid.
#' @export
#' @examples
#' c1 <- c("RR", "2R", "RGU", "11")
#' cc1 <- costs(x = c1)
#' cc1
#' d1 <- data.frame(cost = c1, stringsAsFactors = FALSE)
#' d1$costs <- cc1
#' # hybrid mana
#' costs(x = c("RR", "2wu", "urur", "11", "wub", "X1RR"))

costs <- function(x) {
    cost <- str_split(string = x,
        # (?<=y)x Lookbehind: match blank if there are chars before (i.e. don't match start)
        # x(?=y) Lookahead: match blank if there are chars after
        # (?<![0-9])[0-9] match a number as long as there isn't a preceding number
        pattern = "(?<=[wubrg]{2}|[0-9]|[WUBRGX])(?=[wubrg]{2,5}|(?<![0-9])[0-9]|[WUBRGX])")
    cost <- lapply(X = cost, FUN = str_replace_all, pattern = "X", replacement = "1")
    attr(cost, which = "is_hybrid") <- vapply(X = cost,
        FUN = function(x) { any(str_detect(string = x, pattern = "[wubrg]")) },
        FUN.VALUE = logical(1L))
    class(cost) <- "costs"
    cost
}

#' @describeIn costs Hybrid costs are costs with the non-hybrid
#' (non-wubrg) symbols removed.
#' @export
#' @examples
#' hybrid_costs(x = c("RR", "2wu", "urur", "11", "wub", "X1RR"))
#' hybrid_costs(x = costs(x = c("RR", "2wu", "urur", "11", "wub", "X1RR")))

hybrid_costs <- function(x) {
    if (is.character(x)) { costs <- costs(x) } else { costs <- x }
    cost <- lapply(costs, FUN = str_extract, pattern = "[wubrg]+")
    cost <- lapply(cost, FUN = function(x) if (all(is.na(x))) { character(0)} else { x[!is.na(x)] })
    attr(cost, which = "is_hybrid") <- attr(costs, which = "is_hybrid")
    class(cost) <- "costs"
    cost
}

#' @method as.data.frame costs
#' @export

as.data.frame.costs <- function(x, ...) {
    structure(x, row.names = rep("", times = length(x)), class = "data.frame")
}

#' @method print costs
#' @export

print.costs <- function(x, n = 10, ...) {
    decor <- ""
    nx <- length(x)
    if (n > nx) { n <- nx }
    if (n < nx) { decor <- "\n...\n" }
    prn <- vapply(x[seq_len(n)], FUN = function(xi) { paste0(xi, collapse = ", ")  }, FUN.VALUE = "")
    cat(paste0(paste0(prn, collapse = "\n"), decor))
    invisible(x)
}

#' @describeIn costs `is_hybrid` works for character cost and costs.
#' @export
#' @noRd

is_hybrid <- function(x) { UseMethod("is_hybrid") }

#' @describeIn costs `is_hybrid` performs string parsing on character cost.
#' @examples
#' is_hybrid(c("R", "1G", "wuurrg"))
#' @method is_hybrid character
#' @export
#' @noRd

is_hybrid.character <- function(x) {
    str_detect(string = x, pattern = "[wubrg]")
}

#' @describeIn costs Where possible use `is_hybrid` with costs to avoid string parsing.
#' @examples
#' is_hybrid(costs(c("R", "1G", "wuurrg")))
#' @method is_hybrid costs
#' @export
#' @noRd

is_hybrid.costs <- function(x) {
    attr(x, which = "is_hybrid")
}

