# Name: Tess Steplyk    
# Email: steplyk@wisc.edu

rm(list = ls())

# Implement Connect Four in the same manner that we
# implemented Tic-tac-toe in class. Start by implementing
# the helper functions, below, and testing them by running
#   source("hw3test.R")
# Then write code for the game itself.
#
# We'll test your code by running
#   source("hw3.R")
# We might also play Connect Four, read your code, and do other tests.

# Returns TRUE if vector v (of character strings) contains
# (at least) four in a row of player (character string). e.g.
#   four.in.a.row("X", c("O","X","X","X","X","O"))
# is TRUE, while
#   four.in.a.row("O", c("O","X","X","X","X","O"))
# is FALSE.
four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  for (i in 1:length(v)) {
    num <- 0
    if (v[i] != player) {
      next
    }
    for (j in i:length(v)) {
      if (v[j] == player) {
        num <- num+1
        if (num >= 4) {
          return(TRUE)
        } 
      } 
      else {
        break
      }
    }
  }
  return(FALSE)
}


# Returns TRUE if (matrix) board (of character strings)
# contains at least four in a row of (string) player, who
# just played in position (r, c). (Here "r" means "row" and
# "c" means "column").
#
# Hint: this function should call four.in.a.row() four times.
won = function(player, board, r, c, debug=FALSE) {
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  directions = rep(F, 4) #row col diag anti
  directions[1] <- four.in.a.row(player, board[r,])
  directions[2] <- four.in.a.row(player, board[,c])
  directions[3] <- four.in.a.row(player, get_diag(r, c, board))
  directions[4] <- four.in.a.row(player, get_anti(r, c, board))
  if (any(directions) == T) {
    return(TRUE)
  } 
  else {
    return(FALSE)
  } 
  return(FALSE)
}


# Returns largest index of an empty position in column col
# of (matrix) board. If there is no such empty position in
# board, return value is NULL.
largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  column = board[, col]
  i <- 0
  if (any(column == "")) {
    for (item in column) {
      if (item == "") {
        i <- i+1
      } else {
        return(i)
      }
    }
  }
  if (i == 0) {
    return(NULL)
  }
  return(i)
}


# ... your code to implement Connect Four using the
# functions above ...
get_anti = function(r, c, board) {
  anti <- NULL
  for (i in (min(r, length(board[1,])-c+1) - 1):0) { 
    anti <- c(anti, board[r-i, c+i])
    print(board[r-i, c+i])
  }
  anti <- anti[-length(anti)] 
  for (j in 0:(min((length(board[,1])-r+1), c) - 1)) { 
    anti = c(anti, board[r+j, c-j])
    print(board[r+j, c-j])
  }
  return(anti)
}

get_diag <- function(r, c, board) {
  diag <- NULL
  for (i in (min(r,c) - 1):0) { 
    diag <- c(diag, board[r-i, c-i])
  }
  diag = diag[-length(diag)] 
  for (j in 0:(min((length(board[,1])-r), 
                   (length(board[1,])-c)))) { 
    diag <- c(diag, board[r+j, c+j])
  }
  return(diag)
}

# Hint: this program is modeled on the tic-tac-toe program we did in
# class, so studying the latter program is worthwhile.

# Note that a user click in a column indicates that a checker should
# go to that column's lowest empty row (unless the column is full).

# Note that you should implement a computer player. At the least, it
# should choose randomly from among the non-full columns. (Feel free
# to do much more! If your computer player beats me on its first try,
# you will earn a package of M&Ms. This is a hard task. Feel free to
# ask for tips.)
