
#' @title Calculate Opportunities to play cards given lands played so far
#' @description Adds opportunities to play non-land cards available this turn
#' @param deck data frame with columns, "type", "cost",
#' and optionally number, describing deck to be analysed
#' @return A deck data.frame with columns named type and cost.
#' @importFrom dplyr summarise filter across all_of full_join
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
    cols <- c("W", "U", "B", "R", "G")
    deck <- get_mana(deck = deck)
    resource <- unlist(summarise(
        filter(deck, !is.na(turn) & cards_this_turn & !is_tapped),
        total = sum(mana_value, na.rm = TRUE),
        across(all_of(casefold(cols, upper = FALSE)), sum)))
    indx <- deck$cards_this_turn &
        deck$type != "land" &
        deck$mana_value <= resource["total"] &
        has_resource(cost = deck$cost, resource = resource[-1])
    deck[indx, "opportunities"] <- deck[indx, "opportunities"] + 1L
    deck[!is.na(deck$turn) & deck$cards_this_turn & deck$is_tapped, "is_tapped"] <- FALSE
    return(deck)
}

globalVariables(c("cards_this_turn", "mana_value", "turn", "is_tapped"))
