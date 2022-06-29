
test_that("check play_land", {
    d1 <- d2 <- data.frame(cost = "1", type = "land", turn = NA, stringsAsFactors = FALSE)
    d2$turn <- 1L
    d2$lands_this_turn <- TRUE
    expect_equal(play_land(d1, whichland = 1), d2)
    # if whichland not provided, lands_this_turn column required
    expect_error(suppressWarnings(play_land(d1)))
    # turn should be NA
    expect_error(suppressWarnings(play_land(d2)))
    d1$lands_this_turn <- TRUE
    d1$turn <- NA
    expect_equal(play_land(d1), d2)
})
