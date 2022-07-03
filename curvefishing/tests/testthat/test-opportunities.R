
test_that("check opportunities", {
    d1 <- data.frame(cost = c("R", "R", "1", "RR"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d2 <- opportunities(d1)
    expect_equal(d2$opportunities, c(0, 1, 1, 0))
    d1$is_tapped <- d1$lands_this_turn
    d2 <- opportunities(d1)
    expect_equal(d2$opportunities, c(0, 0, 0, 0))
    expect_true(all(!d2$is_tapped))
    d3 <- data.frame(
        name = 1:20, cost = "R", type = c("spell", "land"),
        turn = c(NA, 1, NA, 2, rep(NA, times = 16)),
        cards_this_turn = rep(c(TRUE, FALSE), times = c(10, 10)),
        stringsAsFactors = FALSE)
    expect_equal(opportunities(deck = d3)$opportunities,
        expected = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    d3$cost <- c("R", "G")
    expect_equal(opportunities(deck = d3)$opportunities,
        expected = rep(0L, times = 20))
    d4 <- data.frame(cost = c("R", "R", "rw", "brbg"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d5 <- opportunities(d4)
    expect_equal(d5$opportunities, c(0, 1, 1, 0))
})
