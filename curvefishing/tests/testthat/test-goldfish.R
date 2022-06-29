
test_that("check goldfish", {
    d1 <- data.frame(cost = c("R", "R", "1", "RR"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d2 <- goldfish(deck = d1)
    expect_equal(d2$opportunities, c(0, 1, 1, 0))
})
