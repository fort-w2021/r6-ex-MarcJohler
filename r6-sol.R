# object for a bavarian card deck
deck <- R6::R6Class("deck",
  public = list(
    deck_id = NA,
    card_stack = NA,
    initialize = function(deck_id) {
      # deck_id must be an integerish single number
      checkmate::assert_count(deck_id)
      # set name for deck
      self$deck_id <- deck_id
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
