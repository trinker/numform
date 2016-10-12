context("Checking f_bills")

test_that("f_bills formats correctly",{

    expect_equivalent(f_bills(1234567891), "1B")

})

test_that("f_mills formats correctly",{

    expect_equivalent(f_mills(1234567), "1M")

})

test_that("f_thous formats correctly",{

    expect_equivalent(f_thous(1234), "1K")

})


test_that("f_bills formats correctly with rounding",{

    expect_equivalent(
        f_bills(12345678918, digits = -10),
        f_bills(12345678918, relative = -1)
    )

})

test_that("f_mills formats correctly with rounding",{

    expect_equivalent(
        f_mills(12345678, digits = -5),
        f_mills(12345678, relative = 1)
    )
})

test_that("f_thous formats correctly with rounding",{

    expect_equivalent(f_thous(1234), "1K")

})
