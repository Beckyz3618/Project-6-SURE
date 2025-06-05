install.packages("usethis")
usethis::use_git_config(user.name = "Karim Zaggoti", 
                        user.email = "karimzaggoti@gmail.com")
usethis::create_github_token()
install.packages("gitcreds")
gitcreds::gitcreds_set()
