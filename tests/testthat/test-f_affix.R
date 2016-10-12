context("Checking f_affix")

test_that("f_affix adds leading and trailing",{

    expect_equivalent(f_affix(1, "-", "%"), '-1%')

})

test_that("f_prefix adds leading",{

    expect_equivalent(f_prefix(1, "-"), '-1')

})

test_that("f_suffix adds trailing",{

    expect_equivalent(f_suffix(1, "%"), '1%')

})
