
#' @title Calculate Opportunities to play cards given lands played so far
#' @description Adds opportunities to play non-land cards available this turn
#' @param deck data frame with columns, "type", "cost",
#' and optionally number, describing deck to be analysed
#' @return A deck data.frame with columns named type and cost.
#' @importFrom dplyr filter select slice n
#' @export
#' @examples
#' d1 <- shuffle_deck(sligh)
#' d1$lands_this_turn <- seq_len(nrow(d1)) == which(d1$type == "land")[1]
#' d1$turn <- NA
#' d1 <- play_land(deck = d1)
#' d1 <- play_land(deck = d1, turn = 2, whichland = which(d1$type == "land")[2])
#' d1$cards_this_turn <- seq_len(nrow(d1)) %in% 1:8
#' opportunities(deck = d1)

opportunities <- function(deck) {
    stopifnot(is_deck(deck))
    stopifnot("turn" %in% colnames(deck))
    if (!"cards_this_turn" %in% colnames(deck)) {
        deck$cards_this_turn <- TRUE
    }
    if (!"mana_value" %in% colnames(deck)) {
        deck$mana_value <- cost_to_mana_value(cost = deck$cost)
    }
    if (!"opportunities" %in% colnames(deck)) {
        deck$opportunities <- 0
    }
    if (!"is_tapped" %in% colnames(deck)) {
        deck$is_tapped <- FALSE
    }
    possibilities <- get_combinations(deck = deck)
    for (poss in seq_along(possibilities)) {
        possibilities[[poss]] <- get_opportunities(deck = possibilities[[poss]])
    }
    possibilities[[which.max(vapply(
        X = possibilities,
        FUN = function(x) sum(x$opportunities),
        FUN.VALUE = numeric(1L)))]]
}

#' @examples
#' d1 <- data.frame(cost = c("R", "rw", "uw"),
#'     cards_this_turn = TRUE,
#'     mana_value = 1,
#'     turn = c(1, NA, NA),
#'     stringsAsFactors = FALSE)
#' curvefishing:::get_combinations(deck = d1)

get_combinations <- function(deck, pattern = "[wubrg]{2,5}?") {
    stopifnot(all(c("turn", "cards_this_turn", "is_tapped", "cost", "mana_value") %in% colnames(deck)))
    is_hybrid_cost <- is_hybrid(deck$cost) &
        deck$cards_this_turn &
        deck$mana_value <= sum(!is.na(deck$turn[deck$cards_this_turn]))
    if (any(is_hybrid_cost)) {
        hybrid_cost <- deck$cost[is_hybrid_cost]
        hybrid <- str_extract_all(string = hybrid_cost, pattern = pattern)
        combs <- as.matrix(expand.grid(
            lapply(
                X = str_split(string = hybrid, pattern = ""),
                FUN = casefold, upper = TRUE), stringsAsFactors = FALSE))
        possibilities <- lapply(
            X = seq_len(nrow(combs)),
            FUN = function(x) {
                dd <- deck
                dd[which(is_hybrid_cost), "cost"] <- combs[x, , drop = TRUE]
                dd
            })
    } else {
        possibilities <- list(deck)
    }
    possibilities
}

#' @examples
#' d1 <- shuffle_deck(sligh)
#' d1$lands_this_turn <- seq_len(nrow(d1)) == which(d1$type == "land")[1]
#' d1$turn <- NA
#' d1 <- play_land(deck = d1)
#' d1 <- play_land(deck = d1, turn = 2, whichland = which(d1$type == "land")[2])
#' d1$cards_this_turn <- seq_len(nrow(d1)) %in% 1:8
#' curvefishing:::get_opportunities(deck = d1)

get_opportunities <- function(deck) {
    stopifnot(all(c("turn", "cards_this_turn", "is_tapped", "type", "mana_value", "opportunities") %in% colnames(deck)))
    deck <- get_mana(deck = deck)
    rows <- deck[!is.na(deck$turn) & deck$cards_this_turn & !deck$is_tapped, ]
    indx <- deck$cards_this_turn & deck$type != "land" & deck$mana_value <=
        sum(rows$mana_value, na.rm = TRUE) & has_resource(
            cost = deck$cost,
            resource = c(sum(rows$w), sum(rows$u), sum(rows$b), sum(rows$r), sum(rows$g)))
    deck[indx, "opportunities"] <- deck[indx, "opportunities"] + 1L
    deck[!is.na(deck$turn) & deck$cards_this_turn & deck$is_tapped,
         "is_tapped"] <- FALSE
    return(deck)
}

globalVariables(c("cards_this_turn", "mana_value", "turn", "is_tapped", "number"))
