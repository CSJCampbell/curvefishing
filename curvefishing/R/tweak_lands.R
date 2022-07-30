

#' @title Replace lands with other lands
#' @description Helper to allow search through land combinations.
#' Lands of type provided by landnumber will be updated with these replacement values.
#' @param decklist data frame which is a decklist
#' @param landnumber named numeric vector with elements "W", "U", "B", "R", "G"
#' @return decklist with updated land numbers
#' @export
#' @examples
#' tweak_lands(decklist = sligh, landnumber = c("R" = 0, "B" = 12))

tweak_lands <- function(decklist,
    landnumber = c("W" = NA, "U" = NA, "B" = NA, "R" = NA, "G" = NA)) {
    stopifnot(is_decklist(decklist))
    stopifnot("is_tapped" %in% names(decklist))
    cols <- c("W", "U", "B", "R", "G")
    lands <- c("Plains", "Island", "Swamp", "Mountain", "Forest")
    landnumber <- landnumber[cols]
    for (land in seq_along(landnumber)) {
        if (!is.na(landnumber[land])) {
            basic_land_ind <- decklist$type == "land" &
                cols[land] == decklist$cost &
                !decklist$is_tapped
            if (landnumber[land] > 0L && sum(basic_land_ind) > 1L) {
                warning(paste(cols[land], "found", sum(basic_land_ind), "times"))
            }
            if (landnumber[land] > 0L && sum(basic_land_ind) < 1L) {
                new_row <- decklist[1L, , drop = FALSE]
                new_row$name <- lands[land]
                new_row$type <- "land"
                new_row$cost <- cols[land]
                new_row$is_tapped <- FALSE
                decklist <- rbind(decklist, new_row)
                basic_land_ind <- decklist$type == "land" &
                    cols[land] == decklist$cost &
                    !decklist$is_tapped
            }
            decklist$number <- replace(
                x = decklist$number,
                list = basic_land_ind,
                values = landnumber[land])
        }
    }
    decklist
}
