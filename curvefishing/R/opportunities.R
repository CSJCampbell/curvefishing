
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
    is_hybrid_cost <- is_hybrid(deck$cost[deck$cards_this_turn]) &
        deck$mana_value[deck$cards_this_turn] <= sum(!is.na(deck$turn[deck$cards_this_turn]))
    # if (any(is_hybrid_cost)) {  }
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
