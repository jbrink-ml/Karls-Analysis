test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

usethis::use_git_config(
  user.name = "Johnathan Brink",
  user.email = "jbrink98762@gmail.com"
)

usethis::use_github(private=TRUE)

usethis::create_github_token()

gitcreds::gitcreds_set()
