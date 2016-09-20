context("Checking f_num")

test_that("f_num strips leading zeros and formats decimal places",{

    x <- f_num(c(0.0, 0, .2, -00.02, 1.122222, pi, "A"), digits = 3)
    expect_equal(x, c(".000", ".000", ".200", "-.020", "1.122", "3.142", NA))

    expect_warning(f_num(c(0.0, 0, .2, -00.02, 1.122222, pi, "A")))
})

test_that("f_num throws proper warnings.",{

    expect_warning(f_num(.3, c(2, 3)))

})

test_that("f_num with s works",{

    expect_true(all(grepl("%$", f_num(c(30, 33.45, .1), 1, s="%"))))

})


test_that("f_num with p works",{

    expect_true(all(grepl("^\\$", f_num(c(30, 33.45, .1), 2, p="$"))))
})
