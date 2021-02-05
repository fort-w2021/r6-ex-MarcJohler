# class for a bavarian card deck
deck <- R6::R6Class("deck",
  public = list(
    card_stack = NA,
    initialize = function() {
      # reset deck (see below)
      self$reset()
    },
    print = function() {
      cat("Cards of the deck", self$deck_id, "(top-down-order): ")
      cat(self$card_stack)
    },
    draw = function(n) {
      cards_remaining <- length(self$card_stack)
      # n must be between 0 and the number of remaining cards
      checkmate::assert_number(n, lower = 0, upper = cards_remaining)
      # early exit for n == 0
      if (n == 0) {
        print("no cards have been drawn")
        return(invisible(NULL))
      }
      # sample n cards randomly
      drawn_indizes <- sample(seq_along(self$card_stack), n)
      drawn_cards <- self$card_stack[drawn_indizes]
      # remove cards from stack
      self$card_stack <- self$card_stack[-drawn_indizes]
      # return drawn cards
      drawn_cards
    },
    reset = function() {
      # define the valid cards of a bavarian deck
      farbe <- c("G", "H", "E", "S")
      wert <- c(6:10, "U", "O", "K", "A")
      cards <- paste0(rep(farbe, each = 9), rep(wert, times = 4))
      # shuffle cards
      self$card_stack <- sample(cards)
    },
    # take n cards from the stack and put them below the other cards
    cut = function(n) {
      cards_remaining <- length(self$card_stack)
      # n must be between 0 and the number of remaining cards
      checkmate::assert_number(n, lower = 0, upper = cards_remaining)
      # only do this if cards are drawn
      if (n > 0) {
        # cards removed from top
        drawn <- self$card_stack[seq_len(n)]
        # re-arrange card-stack
        self$card_stack <- c(self$card_stack[-seq_len(n)], drawn)
      }
      # print new arrangement
      self$print()
    }
  )
)

# Was wäre hier wesentlich anders/komplizierter wenn Sie diese Klasse und Methoden
# mit S3 oder S4 implementieren würden?
#
# ANTWORT: sämtliche Methoden, die etwas am card_stack ändern, müssten in der Form:
# object <- function(object, ...) verwendet werden. Wenn eine Änderung am
# card_stack gefordert ist und zudem ein bestimmter Rückgabewert erwartet wird
# (z.B. bei draw), wird dies syntaktisch noch komplizierter, da der neue Zustand
# des card_stacks sowie der zusätzlich erwartete Wert als Liste zurückgegeben
# werden muss (z.B. die gezogenen Karten bei draw):
# list <- function(object, ...)
# object <- list[[1]] --> Änderung des Zustands
# output <- list[[2]] --> was interessiert den User eigentlich?
#
# statt einfach:
#
# output <- object$methode(...)
