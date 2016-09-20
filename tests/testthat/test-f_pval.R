context("Checking p_val")

test_that("p_val gives =/< and p.value/alpha.",{

    expect_equal(f_pval(.0513), "p = .051")
    expect_equal(f_pval(.049), "p < .05")
})

test_that("p throws proper warnings.",{

    expect_warning(f_pval(.2, c(.05, .01)))
    expect_warning(f_pval(.2, 1.1))
    expect_warning(f_pval(c(.3, .5)))
})

