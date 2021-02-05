# create project
devtools::create("bankr")

# use test structure
usethis::use_test("Bankkonto-sol")

# change the description
options(usethis.full_name = "Marc Johler")
usethis::use_mit_license()
usethis::use_package("R6")
usethis::use_package("checkmate")
usethis::use_package("testthat", type = "Suggests")
# ... change the rest manually

# write documentation manually
devtools::document()

# check test coverage
covr_obj <- covr::package_coverage(getwd())
covr::report(covr_obj)
# it's not 100% - what is the cause?
covr::zero_coverage(covr_obj)
# print functions are not tested - this is not necessary

# check package for completeness
devtools::check()
