context("Checking f_ordinal")

test_that("f_ordinal adds correct suffixes",{

    expect_equivalent(f_ordinal(1:10),
        c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
    )
})


test_that("f_ordinal throws warning below zero",{

    expect_warning(f_ordinal(0))
})
