test_that("ad_nm_expr_normalize converts ** to ^", {
  expect_equal(ad_nm_expr_normalize("A ** 2"), "A ^ 2")
  expect_equal(ad_nm_expr_normalize("(DISC)**0.5"), "(DISC)^0.5")
})
