#' General Account Class
#'
#' Class for a general bank account which enables the deposit and withdraw of money
#'
#' @field account_holder name of the person owning the bank account
#' @field balance current available credit on bank account
#'
#' @param account_holder name of the person owning the bank account
#' @param balance starting credit of the bank account
#' @param amount amount of money which should be transferred
Account <- R6::R6Class("Account",
  public = list(
    account_holder = NA,
    balance = 0,
    initialize = function(account_holder, balance) {
      # account_holder must be a valid name
      checkmate::assert_character(account_holder,
        min.chars = 1,
        len = 1,
        any.missing = FALSE
      )
      # check if balance is a valid amount of money
      # (assuming one cannot start right with debts)
      checkmate::assert_number(balance, lower = 0)
      # assign values to Account
      self$account_holder <- account_holder
      self$balance <- balance
    },
    print = function() {
      cat("Account holder: ", self$account_holder, "\n")
      cat("Balance: ", self$balance)
    },
    deposit = function(amount) {
      # check amount is a valid amount of money
      # only positive values allowed
      checkmate::assert_number(amount, lower = 0)
      # compute the new balance
      self$balance <- self$balance + amount
      cat("New balance: ", self$balance)
    },
    withdraw = function(amount) {
      # check amount is a valid amount of money
      # only positive values allowed
      checkmate::assert_number(amount, lower = 0)
      # compute the new balance
      self$balance <- self$balance - amount
      cat("New balance: ", self$balance)
    }
  )
)
##### for some reason the docu adds account_holder and balance as active binding, why?
#' Giro Account Class
#'
#' Class for a giro bank account which enables the deposit and withdraw of money within a certain overdraft limit
#'
#' @field account_holder name of the person owning the bank account
#' @field balance current available credit on bank account
#' @field overdraft_max limit of negative credit the user is allowed to have
#' @field overdraft_fee_rate each time a withdrawing results in a negative credit, 'overdraft_fee_rate' times the resulting credit is additionally subtracted from the 'balance'
#'
#' @param account_holder name of the person owning the bank account
#' @param balance starting credit of the bank account
#' @param amount amount of money which should be transferred
#' @param overdraft_max limit of negative credit the user is allowed to have
#' @param overdraft_fee_rate each time a withdrawing results in a negative credit, 'overdraft_fee_rate' times the resulting credit is additionally subtracted from the 'balance'
GiroAccount <- R6::R6Class("GiroAccount",
  inherit = Account,
  public = list(
    overdraft_max = NA,
    overdraft_fee_rate = NA,
    initialize = function(account_holder,
                          balance,
                          overdraft_max,
                          overdraft_fee_rate) {
      # account_holder must be a valid name
      checkmate::assert_character(account_holder,
        min.chars = 1,
        len = 1,
        any.missing = FALSE
      )
      # check if balance is a valid amount of money
      # (assuming one cannot start right with debts)
      checkmate::assert_number(balance, lower = 0)
      # check if overdraft max is a valid amount of money
      checkmate::assert_number(overdraft_max, lower = 0)
      # check if overdraft_fee_rate is a valid percentage
      checkmate::assert_number(overdraft_fee_rate, lower = 0)
      # assign values to Account
      self$account_holder <- account_holder
      self$balance <- balance
      self$overdraft_max <- overdraft_max
      self$overdraft_fee_rate <- overdraft_fee_rate
    },
    withdraw = function(amount) {
      # check amount is a valid amount of money
      # only positive values allowed
      checkmate::assert_number(amount, lower = 0)
      # check if overdraft_max would be exceeded
      new_balance <- self$balance - amount
      if (new_balance * (-1 - self$overdraft_fee_rate) > self$overdraft_max) {
        stop("Transaction not allowed: Overdraft maximum would be exceeded")
      }
      # compute the new balance
      self$balance <- self$balance - amount
      # if balance is negative subtract the overdraft fee
      if (self$balance < 0) {
        self$balance <- self$balance * (1 + self$overdraft_fee_rate)
      }
      cat("New balance: ", self$balance)
    }
  )
)

#' Safe Account Class
#'
#' Class for a safe bank account which enables the deposit and withdraw of money without overdrafting and prevents overwriting of sensible information
#'
#' @field account_holder name of the person owning the bank account
#' @field balance current available credit on bank account
#' @field transaction_log object of class 'TransactionLog' listing all the transactions made with this bank account
#'
#' @param account_holder name of the person owning the bank account
#' @param balance starting credit of the bank account
#' @param amount amount of money which should be transferred
SafeAccount <- R6::R6Class("SafeAccount",
  private = list(
    .balance = NA,
    .transaction_log = NA
  ),
  public = list(
    account_holder = NA,
    initialize = function(account_holder, balance) {
      # account_holder must be a valid name
      checkmate::assert_character(account_holder,
        min.chars = 1,
        len = 1,
        any.missing = FALSE
      )
      # check if balance is a valid amount of money
      # (assuming one cannot start right with debts)
      checkmate::assert_number(balance, lower = 0)
      # assign values to Account
      self$account_holder <- account_holder
      private$.balance <- balance
      private$.transaction_log <- TransactionLog$new(Sys.time(), balance)
    },
    deposit = function(amount) {
      # check amount is a valid amount of money
      # only positive values allowed
      checkmate::assert_number(amount, lower = 0)
      # save old balance
      old_balance <- self$balance
      # compute the new balance
      private$.balance <- old_balance + amount
      # save transaction in log
      private$.transaction_log$new_entry(
        Sys.time(),
        old_balance,
        amount,
        "deposit",
        TRUE,
        self$balance
      )
      # what is the new balance?
      cat("New balance: ", self$balance)
    },
    withdraw = function(amount) {
      # check amount is a valid amount of money
      # only positive values allowed
      checkmate::assert_number(amount, lower = 0)
      # save old balance
      old_balance <- self$balance
      # assumption: it's not allowed to overdraft
      new_balance <- old_balance - amount
      if (new_balance < 0) {
        # save failed transaction in log
        private$.transaction_log$new_entry(
          Sys.time(),
          old_balance,
          amount,
          "withdraw",
          FALSE,
          old_balance
        )
        stop("Transaction not allowed: Not enough credit on account")
      }
      # overwrite balance
      private$.balance <- new_balance
      # save transaction in log
      private$.transaction_log$new_entry(
        Sys.time(),
        old_balance,
        amount,
        "withdraw",
        TRUE,
        new_balance
      )
      # what is the new balance?
      cat("New balance: ", new_balance)
    },
    print = function(show_transactions = FALSE) {
      cat("Account holder: ", self$account_holder, "\n")
      cat("Balance: ", self$balance)
      if (show_transactions) {
        cat("\n Transactions: \n")
        print(self$transaction_log$transactions)
      }
    }
  ),
  active = list(
    balance = function(amount) {
      # if function gets an argument, it is an attempt to overwrite the balance
      # --> show an error
      if (!missing(amount)) {
        stop("'$balance' is read only", call. = FALSE)
      }
      # otherwise just return the current balance
      private$.balance
    },
    transaction_log = function(value) {
      # if function gets an argument, it is an attempt to overwrite
      # the transaction_log --> show an error
      if (!missing(value)) {
        stop("'$transaction_log' is read only", call. = FALSE)
      }
      # otherwise just return the transaction_log
      private$.transaction_log
    }
  )
)

# Welche Vorteile hat es `balance` als `active binding` zu einem `private` Feld statt als `public` Feld zu implementieren?
#
# ANTWORT: Der Vorteil von SafeAccount gegenüber Account ist, dass der Kontostand
# nach der Erstellung eines Accounts nur noch mittels deposit() und withdraw()
# verändert werden kann, d.h. dass der Kontostand nicht einfach ohne Transaktion
# geändert werden kann.

#' Transaction log Class
#'
#' Class for a transaction log which tracks all of the attempted transactions of a bank account
#'
#' @field transactions 'data.frame'-object, listing all the transactions made, including opening the corresponding bank account
#'
#' @param date date and time of the transaction
#' @param starting_balance amount of money with which the account is opened
#' @param old_balance account balance before transaction
#' @param amount amount of money which was attempted to be transferred
#' @param type type of transaction. Must be either "deposit" or "withdraw"
#' @param success was the transaction successful? Either TRUE or FALSE
#' @param new_balance account balance after transaction
TransactionLog <- R6::R6Class("TransactionLog",
  public = list(
    transactions = NA,
    initialize = function(date, starting_balance) {
      # Sys.time() has no attribute 'tzone',
      # that's why it must be removed from date before assertion
      attributes(date)$tzone <- NULL
      # check if date is a realistic date
      checkmate::assert_posixct(date, upper = Sys.time())
      # check if amount is a valid starting credit
      checkmate::assert_number(starting_balance, lower = 0)
      # first transaction: account opening
      self$transactions <- data.frame(
        date = date,
        old_balance = 0,
        amount = starting_balance,
        type = "open account",
        success = TRUE,
        new_balance = starting_balance
      )
    },
    new_entry = function(date,
                         old_balance,
                         amount, type,
                         success,
                         new_balance) {
      # Sys.time() has no attribute 'tzone',
      # that's why it must be removed from date before assertion
      attributes(date)$tzone <- NULL
      # check if date is a realistic date
      checkmate::assert_posixct(date, upper = Sys.time())
      # check if old_balance is a number
      checkmate::assert_number(old_balance)
      # check if amount is a positive number
      checkmate::assert_number(amount, lower = 0)
      # check if type is either "deposit" or "withdraw"
      # (not solved with a binary variable, since there could be
      # more types in future extensions of the bank system)
      checkmate::assert_choice(type, c("deposit", "withdraw"))
      # check if transaction is a single logical variable
      checkmate::assert_flag(success)
      # check if new_balance is a number
      checkmate::assert_number(new_balance)
      self$transactions <- rbind(
        self$transactions,
        list(
          date,
          old_balance,
          amount,
          type,
          success,
          new_balance
        )
      )
    }
  )
)
