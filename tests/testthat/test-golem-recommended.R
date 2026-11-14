test_that("app ui", {
  ui <- app_ui()
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  expect_true("request" %in% names(formals(app_ui)))
})

test_that("app server", {
  expect_type(app_server, "closure")
  # Check that formals have not been removed
  expect_true(all(c("input", "output", "session") %in% names(formals(app_server))))
})

test_that("app_sys works", {
  expect_true(app_sys("golem-config.yml") != "")
})

test_that("app launches", {
  golem::expect_running(sleep = 5)
})
