
#' @title Convert mana cost to mana value
#' @description Calculate mana value, ignoring coloured mana.
#' @param cost character vector containing coloured mana requirements
#' (W, U, B, R, G)
#' @return numeric vector
#' @importFrom stringr str_detect str_extract str_replace_all str_split str_count
#' @export
#' @examples
#' cost_to_mana_value(cost = c("RR", "2R", "RGU", "11"))

cost_to_mana_value <- function(cost) {
    # handle mana cost with digits greater than 9.
    is_two_digits <- str_detect(string = cost, pattern = "[1-9][0-9]")
    if (any(is_two_digits)) {
        for (two in which(is_two_digits)) {
            two_digits <- as.numeric(str_extract(string = cost[two], pattern = "[1-9][0-9]"))
            num_fives <- two_digits %/% 5
            num_remainder <- two_digits %% 5
            cost[two] <- paste0(c(rep(5, times = num_fives), num_remainder), collapse = "")
        }
    }
    cost <- str_replace_all(string = cost, pattern = "[WUBRGX]", replacement = "1")
    cost_list <- str_split(string = cost, pattern = "")
    vapply(X = cost_list, FUN = function(x) { sum(as.numeric(x), na.rm = TRUE) }, FUN.VALUE = numeric(1))
}

#' @title Have lands with sufficient coloured mana been played?
#' @description Is coloured mana requirement met?
#' @param cost character vector containing coloured mana requirements
#' (W, U, B, R, G)
#' @param resource numeric vector with quantity of coloured mana resource available
#' @return logical vector of length cost
#' @examples
#' curvefishing:::has_resource(cost = c("RR", "2R", "RGU", "11"), resource = c(0, 0, 0, 1, 0))

has_resource <- function(cost, resource) {
    cost_df <- get_mana(data.frame(cost = cost, stringsAsFactors = FALSE))
    has_each <- as.matrix(cost_df[-1]) <= matrix(resource, ncol = 5, nrow = nrow(cost_df), byrow = TRUE)
    apply(X = has_each, MARGIN = 1L, FUN = all)
}
