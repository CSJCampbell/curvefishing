
#' @title Calculate Opportunities to play cards given lands played so far
#'
#' @description Adds opportunities to play non-land cards available this turn.
#'
#' This function checks all combinations of coloured mana produced by lands,
#' and all combinations of hybrid mana of spells. The combination which
#' produces the most opportunities to play spells this turn will be selected.
#' In the event of a tie, the first combination with the most opportunities
#' will be selected.
#'
#' @param deck data frame with columns, "type", "cost", "turn"
#' and optionally: \itemize{
#'   \item costs, a costs column of cost split into tokens
#'   \item hybrid_costs, a costs column containing only hybrid or
#'   alternative mana production tokens ("wu", "urb", etc.)
#'   \item cards_this_turn, a logical column of cards available to play
#'   \item mana_value, a numeric column to converted mana cost
#'   \item opportunities, a numeric column counting the number of opportunities to play each card so far
#'   \item is_tapped, a logical column indicating lands that cannot be used to pay costs on the turn played
#'   \item is_search_basic, a logical column indicating lands that fetch basic lands from the deck
#'   \item is_basic, a logical column indicating lands that have the basic subtype
#' }
#' @param updateall single logical update all columns used? (default TRUE)
#' @return A deck data.frame with columns named type and cost.
#' @importFrom dplyr filter select slice n
#' @export
#' @seealso [`get_combinations`]
#' @examples
#' d1 <- shuffle_deck(sligh)
#' d1$lands_this_turn <- seq_len(nrow(d1)) == which(d1$type == "land")[1]
#' d1$turn <- NA
#' d1 <- play_land(deck = d1)
#' d1 <- play_land(deck = d1, turn = 2, whichland = which(d1$type == "land")[2])
#' d1$cards_this_turn <- seq_len(nrow(d1)) %in% 1:8
#' opportunities(deck = d1)

opportunities <- function(deck, updateall = TRUE) {
    stopifnot(is_deck(deck))
    if (!is.character(deck$cost)) { deck$cost <- as.character(deck$cost) }
    stopifnot("turn" %in% colnames(deck))
    if (!"cards_this_turn" %in% colnames(deck)) {
        deck$cards_this_turn <- TRUE
    }
    if (!"costs" %in% names(deck)) {
        deck$costs <- costs(deck$cost)
    }
    if (!"hybrid_costs" %in% names(deck)) {
        deck$hybrid_costs <- hybrid_costs(deck$costs)
    }
    if (!"mana_value" %in% colnames(deck)) {
        deck$mana_value <- cost_to_mana_value(cost = deck$costs)
    }
    if (!"opportunities" %in% colnames(deck)) {
        deck$opportunities <- 0
    }
    if (!"is_tapped" %in% colnames(deck)) {
        deck$is_tapped <- FALSE
    }
    if (!"is_search_basic" %in% colnames(deck)) {
        deck$is_search_basic <- FALSE
    }
    if (!"is_basic" %in% colnames(deck)) {
        deck$is_basic <- deck$type == "land" &
            deck$cost %in% c("W", "U", "B", "R", "G") &
            !deck$is_tapped
    }
    possibilities <- get_combinations(deck = deck)
    for (poss in seq_along(possibilities)) {
        possibilities[[poss]] <- get_opportunities(deck = possibilities[[poss]])
    }
    dp <- possibilities[[which.max(vapply(
        X = possibilities,
        FUN = function(x) sum(x$opportunities),
        FUN.VALUE = numeric(1L)))]]
    if (!updateall) {
        cols <- c("is_tapped", "opportunities", "turn", "cards_this_turn")
        # when a sac land has been used, keep the updated cost
        cols_cost <- c("cost", "costs", "mana_value")
        rows_cost <- dp$cost == "0" & dp$is_search_basic & !is.na(dp$turn)
        du <- deck
        du[, cols] <- dp[, cols]
        if (sum(rows_cost) > 0) {
            du[rows_cost, cols_cost] <- 0
            du$hybrid_costs[[which(rows_cost)]] <- character(0)
            attr(du$hybrid_costs, which = "is_hybrid")[rows_cost] <- FALSE
        }
        dp <- du
    }
    return(dp)
}

#' @title Internal functions for `opportunities`
#' @description [`opportunities`] needs to perform multiple operations
#' even to calculate which cards can be played to handle the various
#' features of several common mechanics.
#' @describeIn get_combinations calculates hybrid mana combinations.
#' This function uses row id, so rownames will be removed.
#' @inheritParams opportunities
#' @return `get_combinations` returns a list of decks with each combination
#' of hybrid, alternative or searchable mana
#' @examples
#' d1 <- data.frame(cost = c("R", "rw", "uw"),
#'     type = c("land", "spell", "spell"),
#'     cards_this_turn = TRUE,
#'     is_tapped = FALSE,
#'     mana_value = 1L,
#'     turn = c(1L, NA, NA),
#'     is_basic = c(TRUE, FALSE, FALSE),
#'     is_search_basic = FALSE,
#'     stringsAsFactors = FALSE)
#' curvefishing:::get_combinations(deck = d1)

get_combinations <- function(deck) {
    stopifnot(all(c("type", "cost",
            "turn", "cards_this_turn", "is_tapped",
            "mana_value", "is_search_basic", "is_basic") %in% colnames(deck)))
    rownames(deck) <- NULL
    if (!"costs" %in% colnames(deck)) {
        deck$costs <- costs(deck$cost)
    }
    if (!"hybrid_costs" %in% colnames(deck)) {
        deck$hybrid_costs <- hybrid_costs(deck$costs)
    }
    if (!"is_hybrid_this_turn" %in% colnames(deck)) {
        deck$is_hybrid_this_turn <- is_hybrid(deck$hybrid_costs) &
            deck$cards_this_turn &
            deck$mana_value <= sum(!is.na(deck$turn))
    }
    if (any(deck$is_hybrid_this_turn)) {
        # search for basic lands in deck
        hybrid <- get_searchable(deck = deck)
        for (ii in seq_along(hybrid)) {
            if (!deck$is_hybrid_this_turn[ii]) {
                hybrid[[ii]] <- character(0)
            }
        }
        is_hybrid_inc_search <- vapply(hybrid,
            FUN = function(x) length(x) > 0L, FUN.VALUE = logical(1L))
        if (sum(is_hybrid_inc_search) > 0L) {
            # all combinations of hybrid mana for lands and spells
            # for cards that are playable this turn
            combs <- list_to_permutation_matrix(
                    lapply(
                        X = hybrid[is_hybrid_inc_search],
                        FUN = get_converted_hybrid_cost)
                )
            colnames(combs) <- seq_along(hybrid)[deck$is_hybrid_this_turn &
                    is_hybrid_inc_search]
            possibilities <- vector(mode = "list", length = nrow(combs))
            for (i in seq_len(nrow(combs))) {
                # i, a permutation index to analyse
                    dd <- deck
                    # determine which search lands can find a basic matching their colours
                    # j, a hybrid index to update
                    for (j in colnames(combs)) {
                        hj <- as.numeric(j)
                        # TODO handle multiple hybrid?
                        ijk <- dd$costs[[hj]] %in% dd$hybrid_costs[[hj]]
                        costs_hj <- dd$costs[[hj]]
                        # handle multiple hybrid symbols per cost
                        if (length(costs_hj) > 1L) {
                            costs_hj[ijk] <- str_split(combs[i, j, drop = TRUE], pattern = "")[[1L]]
                        } else {
                            costs_hj[ijk] <- combs[i, j, drop = TRUE]
                        }
                        dd$costs[[hj]] <- costs_hj
                        if (dd[j, "is_search_basic", drop = TRUE]) {
                            # find first basic land still in deck to search for
                            ind <- which.max(dd$cost %in% combs[i, j, drop = TRUE] &
                                dd$type == "land" &
                                dd$is_basic &
                                !dd$cards_this_turn)
                            # put basic into pool of playable cards, tapped, and play as turn's drop
                            dd[ind, c("cards_this_turn", "is_tapped", "turn")] <- list(TRUE, TRUE, max(dd$turn, na.rm = TRUE))
                            # flag search basic as used
                            dd[j, c("cost", "costs", "mana_value", "cards_this_turn")] <- list("0", "0", 0, TRUE)
                            dd$hybrid_costs[[hj]] <- character(0)
                            attr(dd$hybrid_costs, which = "is_hybrid")[hj] <- FALSE
                        }
                    }
                    possibilities[[i]] <- dd
                }
        } else {
            possibilities <- list(deck)
        }
    } else {
        possibilities <- list(deck)
    }
    possibilities
}

# list_to_permutation_matrix(x = list(c("U", "R"), c("R", "G")))

list_to_permutation_matrix <- function(x) {
    as.matrix(
        expand.grid(x,
        stringsAsFactors = FALSE))
}

# get_converted_hybrid_cost(x = c("U", "R")) == c("U", "R")
# get_converted_hybrid_cost(x = "ur") == c("U", "R")
# get_converted_hybrid_cost(x = c("ur", "rg")) == c("UR", "RR", "UG", "RG")

get_converted_hybrid_cost <- function(x) {
    out <- vector(mode = "list", length = length(x))
    if (all(x %in% c("W", "U", "B", "R", "G"))) {
        out <- x
    } else {
        for (i in seq_along(x)) {
            out[[i]] <- casefold(str_split(string = x[[i]], pattern = "")[[1L]], upper = TRUE)
        }
    }
    apply(X = list_to_permutation_matrix(out), MARGIN = 1, FUN = paste0, collapse = "")
}

#' @describeIn get_combinations calculates the mana identities of search lands,
#' hybrid lands and hybrid spells.
#'
#' A card with TRUE 'is_search_basic'
#' can be played if TRUE 'cards_this_turn',
#' and another land with FALSE 'cards_this_turn'
#' has a cost that matches the card's cost.
#' @return `get_searchable` returns a list of length nrow deck (hybrid costs)
#' which is character(0) where cost is not hybrid,
#' a hybrid cost (e.g. `"wub"`),
#' or a vector or searchable basic mana types (e.g. `c("W", "U")`)
#' @examples
#' d5 <- data.frame(cost = c("rw", "R", "R", "R"),
#'     type = c("land", "spell", "spell", "land"),
#'     cards_this_turn = c(TRUE, TRUE, TRUE, FALSE),
#'     is_tapped = c(TRUE, FALSE, FALSE, FALSE),
#'     turn = c(1L, NA, NA, NA),
#'     is_search_basic = c(TRUE, FALSE, FALSE, FALSE),
#'     is_basic = c(FALSE, FALSE, FALSE, TRUE),
#'     stringsAsFactors = FALSE)
#' curvefishing:::get_searchable(deck = d5)
#' d6 <- rbind(d5, list("W", "land", FALSE, FALSE, NA, FALSE, TRUE))
#' curvefishing:::get_searchable(deck = d6)

get_searchable <- function(deck) {
    stopifnot(all(c("cost", "turn", "cards_this_turn", "is_tapped",
            "is_search_basic", "is_basic") %in% colnames(deck)))
    if (!"costs" %in% colnames(deck)) {
        deck$costs <- costs(deck$cost)
    }
    if (!"hybrid_costs" %in% colnames(deck)) {
        deck$hybrid_costs <- hybrid_costs(deck$costs)
    }
    hybrid <- deck$hybrid_costs
    deck$is_search_this_turn <- deck$is_search_basic & deck$cards_this_turn &
        # do not re-use search lands
        deck$cost != "0"
    if (any(deck$is_search_this_turn)) {
        for (sb in seq_len(sum(deck$is_search_this_turn))) {
            basic_types <- casefold(str_split(
                    string = hybrid[deck$is_search_this_turn][sb], pattern = "")[[1]],
                upper = TRUE)
            hybrid[deck$is_search_this_turn][[sb]] <- basic_types[basic_types %in%
                deck$cost[deck$type == "land" & deck$is_basic & !deck$cards_this_turn]]
        }
    }
    hybrid
}

#' @describeIn get_combinations calculates opportunities. The mana value of lands played
#' satisfies the mana value of each spell this turn.
#' @inheritParams goldfish
#' @return `get_opportunities` returns deck with opportunities column updated
#' @examples
#' d1 <- shuffle_deck(sligh)
#' d1$lands_this_turn <- seq_len(nrow(d1)) == which(d1$type == "land")[1]
#' d1$turn <- NA
#' d1 <- play_land(deck = d1)
#' d1 <- play_land(deck = d1, turn = 2, whichland = which(d1$type == "land")[2])
#' d1$cards_this_turn <- seq_len(nrow(d1)) %in% 1:8
#' d1$opportunities <- 0
#' d1$mana_value <- cost_to_mana_value(cost = d1$cost)
#' curvefishing:::get_opportunities(deck = d1)

get_opportunities <- function(deck) {
    stopifnot(all(c("cost", "turn", "cards_this_turn", "is_tapped",
            "type", "mana_value", "opportunities") %in% colnames(deck)))
    if (!"costs" %in% colnames(deck)) {
        deck$costs <- costs(deck$cost)
    }
    if (!"hybrid_costs" %in% colnames(deck)) {
        deck$hybrid_costs <- hybrid_costs(deck$costs)
    }
    if (any(is_hybrid(deck$costs)) || !all(c("w", "u", "b", "r", "g") %in% colnames(deck))) {
        deck <- get_mana(deck = deck)
    }
    # played land
    rows <- deck[!is.na(deck$turn) & deck$cards_this_turn & !deck$is_tapped, ]
    # compare costs with played land coloured mana totals
    indx <- deck$cards_this_turn & deck$type != "land" & deck$mana_value <=
        sum(rows$mana_value, na.rm = TRUE) & has_resource(
            deck = deck,
            resource = c(sum(rows$w), sum(rows$u), sum(rows$b), sum(rows$r), sum(rows$g)))
    deck[indx, "opportunities"] <- deck[indx, "opportunities"] + 1L
    deck[!is.na(deck$turn) & deck$cards_this_turn & deck$is_tapped,
         "is_tapped"] <- FALSE
    return(deck)
}

globalVariables(c("cards_this_turn", "mana_value", "turn", "is_tapped", "number"))
