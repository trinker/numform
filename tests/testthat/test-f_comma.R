context("Checking f_comma")

test_that("f_comma has the correct number of digits and commas",{

    expect_true(nchar(f_comma(123456789)) == 11)

})

