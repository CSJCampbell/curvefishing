
test_that("check cost_to_mana_value", {
    expect_equal(cost_to_mana_value(cost = c("RR", "2R", "RGU", "11")),
        expected = c(2, 3, 3, 11))
})

test_that("check has_resource", {
    expect_equal(curvefishing:::has_resource(cost = c("RR", "2R", "RGU", "11"),
             resource = c(0, 0, 0, 1, 0)),
        expected = c(FALSE, TRUE, FALSE, TRUE))
})
