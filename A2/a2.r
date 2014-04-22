source("a2funcs.r")

trainx <- read.table("trn.txt", header = FALSE)
trainx <- as.matrix (trainx)

trainy <- read.table("trnlab.txt", header = FALSE)
trainy <- as.matrix (trainy)

testx <- read.table("tst.txt", header = FALSE)
testx <- as.matrix(testx)

testy <- read.table("tstlab.txt", header = FALSE)
testy <- as.matrix(testy)


start <- 1
end <- 5
j <- 1
err <- matrix (NA, end-start+1, 2)
err  <- err_calc (trainx, trainy, testx, testy)

