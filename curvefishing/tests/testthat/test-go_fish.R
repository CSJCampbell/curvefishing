
test_that("check go_fish", {
    d1 <- data.frame(cost = c("R", "R", "1", "RR"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d2 <- go_fish(d1, nsim = 2)
    expect_equal(c(d2), 2)
    expect_equal(attr(d2, which = "fishing"), c(2, 2))
})
