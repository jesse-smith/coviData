test_that("weekday schedule matches original", {
  expect_snapshot_output(
    asg_schedule_weekdays(start = "2021-01-01", end = "2021-01-31")
  )
})

test_that(
  "4-2 schedule matches original", {
  expect_snapshot_output(
    asg_schedule_42(
      start = "2021-01-01",
      end = "2021-01-31",
      anchor = "2020-12-31"
    )
  )
})

test_that(
  "(5-2)-(5-3)-(6-2)x4-(6-3) schedule matches original", {
  expect_snapshot_output(
    asg_schedule_42(
      start = "2021-01-01",
      end = "2021-01-31",
      anchor = "2020-12-31"
    )
  )
})
