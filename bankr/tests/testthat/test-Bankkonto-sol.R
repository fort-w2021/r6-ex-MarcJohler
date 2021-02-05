testthat::context("Account")

# Class Account and its methods work
testthat::test_that("basic implementation is correct", {
  account1 <- Account$new("Maria Musterfrau", 10000)
  account1$deposit(5000)
  testthat::expect_equivalent(
    account1$balance,
    15000
  )
  account2 <- Account$new("Max Mustermann", 5000)
  account2$withdraw(10000)
  testthat::expect_equivalent(
    account2$balance,
    -5000
  )
})

testthat::context("GiroAccount")

# Overdrafting for a GiroAccount causes a fee
testthat::test_that("Overdraft fees work", {
  giro_account1 <- GiroAccount$new("Maria Musterfrau", 10000, 99999, 0.05)
  giro_account1$withdraw(15000)
  testthat::expect_equivalent(
    giro_account1$balance,
    -5000 * 1.05
  )
})

# Withdrawing is not allowed if overdraft maximum would be exceeded
testthat::test_that("Overdraft limit works", {
  giro_account2 <- GiroAccount$new("Max Mustermann", 1000, 10000, 0.05)
  testthat::expect_error(giro_account2$withdraw(10900))
  # balance must stay untouched
  testthat::expect_equivalent(
    giro_account2$balance,
    1000
  )
})

testthat::context("SafeAccount")

# Private attributes are not overwriteable from outside the class
testthat::test_that("Balance and transaction logs are read-only", {
  safe_account1 <- SafeAccount$new("Maria Musterfrau", 10000)
  testthat::expect_error(safe_account1$balance <- 9999999)
  # balance must stay untouched
  testthat::expect_equivalent(
    safe_account1$balance,
    10000
  )
  # same for transaction log
  old_transaction_log <- safe_account1$transaction_log
  testthat::expect_error(safe_account1$transaction_log <- "illegal")
  # balance must stay untouched
  testthat::expect_equivalent(
    safe_account1$transaction_log,
    old_transaction_log
  )
})

# Account is locked for overdrafting
testthat::test_that("Overdrafting disabled", {
  safe_account2 <- SafeAccount$new("Max Mustermann", 1000)
  testthat::expect_error(safe_account2$withdraw(5000))
  # balance must stay untouched
  testthat::expect_equivalent(
    safe_account2$balance,
    1000
  )
})

# Transaction log

# New entries can be made without overwriting the old entries
testthat::test_that("Transaction log works manually", {
  transaction_log <- TransactionLog$new(
    as.POSIXct("2021-01-01 00:00:05"),
    20000
  )
  testthat::expect_equal(
    nrow(transaction_log$transactions),
    1 # opening the account
  )
  transaction_log$new_entry(
    date = as.POSIXct("2021-01-02 12:00:05"),
    old_balance = 20000,
    amount = 5000,
    type = "withdraw",
    success = TRUE,
    new_balance = 15000
  )
  testthat::expect_equal(
    nrow(transaction_log$transactions),
    2 # opening the account + 1 entry for the transaction
  )
})

# Transaction log works for every kind of event:
# Deposit, successful Withdraw, unsuccessful Withdraw
testthat::test_that("Transaction log works automatically on transactions", {
  safe_account3 <- SafeAccount$new("M. M.", 10000)
  try(safe_account3$deposit(5000), silent = TRUE)
  try(safe_account3$withdraw(10000), silent = TRUE)
  try(safe_account3$withdraw(20000), silent = TRUE)
  # check if every method call caused a log entry
  testthat::expect_equal(
    nrow(safe_account3$transaction_log$transactions),
    3 + 1 # 3 events + 1 entry for opening the account
  )
})
