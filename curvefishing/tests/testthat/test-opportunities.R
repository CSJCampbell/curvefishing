
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
})
