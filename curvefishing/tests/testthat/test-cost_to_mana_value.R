
test_that("check cost_to_mana_value", {
    expect_equal(cost_to_mana_value(cost = c("RR", "2R", "RGU", "11")),
        expected = c(2, 3, 3, 11))
    expect_equal(cost_to_mana_value(cost = "11B"),
        expected = 12)
    expect_equal(cost_to_mana_value(cost = c("wu", "ub", "ub", "ur", "br", "bg", "rg", "rw", "gw", "gu")),
        expected = rep(1, times = 10))
    expect_equal(cost_to_mana_value(cost = c("rw", "2rw", "1bgbg", "1brRrg")),
        expected = c(1, 3, 3, 4))
})

test_that("check has_resource", {
    expect_equal(curvefishing:::has_resource(cost = c("RR", "2R", "RGU", "11"),
             resource = c(0, 0, 0, 1, 0)),
        expected = c(FALSE, TRUE, FALSE, TRUE))
})
