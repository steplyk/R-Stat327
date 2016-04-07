Matrix (4)

#1
m = matrix(data=1:12, nrow=3, ncol=4, byrow=TRUE)
kids = matrix(data=c(c(1,2,6,7,9,11), c(1,5,100,100,100,100)),
              nrow=2, ncol=6, byrow=TRUE,
              dimnames=list(c("Age","#Toys"),
                            c("Teresa","Margaret","Monica","Andrew","Mary","Philip")
              )
)
m
cbind(m, 101:103)
rbind(m, 101:104)
#cbind(m[, 1:2], 101:103, )

------------------------------

#Connect Four

#rows: (4 subrows)(6 rows) = 24
#col: (3)(7) = 21
#diag/reverse diag: (2)(3)(12) = 72 ... but can they win in that diag

#won(board, player, r, c) .. check for win thru (r,c) only by checking 4 vectors
#four.in.a.row(v, player)
#largestemptyrow() 

m
row(m) == col(m)
m[row(m) == col(m)] # main diagonal
r = 2; c = 3 #in connect four, placed in r2, c3
m[row(m) - col(m) == r - c] # diagonal through (r, c)
m[row(m) - col(m) == r + c] # diagonal through (r, c)
m[ ______________________ ] # reverse diagonal through (r, c)
