
test_that("check goldfish", {
    d1 <- data.frame(cost = c("R", "R", "1", "RR"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d2 <- goldfish(deck = d1)
    expect_equal(d2$opportunities, c(0, 1, 1, 0))
    d3 <- data.frame(cost = c("W", "W", "W", "ub", "WW"),
        type = c("land", "land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA, NA),
        lands_this_turn = c(TRUE, TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d4 <- goldfish(d3)
    expect_equal(d4$opportunities, c(0L, 0L, 2L, 0L, 1L))
    d3 <- data.frame(cost = c("rw", "uw", "rw", "ub", "WW"),
        type = c("land", "land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA, NA),
        lands_this_turn = c(TRUE, TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d4 <- goldfish(d3)
    expect_equal(d4$opportunities, c(0L, 0L, 2L, 1L, 0L))
})
