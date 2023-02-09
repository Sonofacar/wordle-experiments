#setup
source('~/docs/woRdle/search-functions.R')

# Setting up the simulation
iter <- 100000
standard.table <- data.frame("Pre.length" = numeric(iter),
	"Post.length" = numeric(iter),
	"Ratio" = numeric(iter),
	"Greens" = numeric(iter),
	"Yellows" = numeric(iter),
	"Score" = numeric(iter),
	"Word" = character(iter),
	"First.guess" = character(iter),
	"Second.guess" = character(iter))
blend.table <- data.frame("Pre.length" = numeric(iter),
	"Post.length" = numeric(iter),
	"Ratio" = numeric(iter),
	"Greens" = numeric(iter),
	"Yellows" = numeric(iter),
	"Score" = numeric(iter),
	"Word" = character(iter),
	"First.guess" = character(iter),
	"Second.guess" = character(iter))
orthogonal.table <- data.frame("Pre.length" = numeric(iter),
	"Post.length" = numeric(iter),
	"Ratio" = numeric(iter),
	"Greens" = numeric(iter),
	"Yellows" = numeric(iter),
	"Score" = numeric(iter),
	"Word" = character(iter),
	"First.guess" = character(iter),
	"Second.guess" = character(iter))

# Standard guess simulation
for(i in 1:iter){
	word <- sample(all.words,1)
	guess <- sample(all.words,1)
	green.vec <- is.green(word,guess)
	green.i <- green.vec |> as.numeric() |> paste(sep = '')
	green <- paste(green.i[1],green.i[2],green.i[3],green.i[4],green.i[5],sep = "")
	yellow.vec <- is.yellow(word,guess)
	yellow.i <- yellow.vec |> as.numeric() |> paste(sep = '')
	yellow <- paste(yellow.i[1],yellow.i[2],yellow.i[3],yellow.i[4],yellow.i[5],sep = "")
	word.list <- green.yellow(all.words,guess,green,yellow)
	l <- length(word.list)
	new.guess <- standard.guess(word.list)
	green.vec <- is.green(word,new.guess)
	green.i <- green.vec |> as.numeric() |> paste(sep = '')
	green <- paste(green.i[1],green.i[2],green.i[3],green.i[4],green.i[5],sep = "")
	yellow.vec <- is.yellow(word,new.guess)
	yellow.i <- yellow.vec |> as.numeric() |> paste(sep = '')
	yellow <- paste(yellow.i[1],yellow.i[2],yellow.i[3],yellow.i[4],yellow.i[5],sep = "")
	word.list <- green.yellow(word.list,new.guess,green,yellow)
	new.l <- length(word.list)
	ratio <- new.l / l
	green.n <- sum(green.vec)
	yellow.n <- sum(yellow.vec)
	score <- score.calc(green.n,yellow.n)
	standard.table[i,1:6] <- c(l,new.l,ratio,green.n,yellow.n,score)
	standard.table[i,7:9] <- c(word,guess,new.guess)
}
# Blended orthogonal guess simulation
for(i in 1:iter){
	word <- sample(all.words,1)
	guess <- sample(all.words,1)
	guess.letters <- strsplit(guess,'')[[1]]
	green.vec <- is.green(word,guess)
	green.i <- green.vec |> as.numeric() |> paste(sep = '')
	green <- paste(green.i[1],green.i[2],green.i[3],green.i[4],green.i[5],sep = "")
	yellow.vec <- is.yellow(word,guess)
	yellow.i <- yellow.vec |> as.numeric() |> paste(sep = '')
	yellow <- paste(yellow.i[1],yellow.i[2],yellow.i[3],yellow.i[4],yellow.i[5],sep = "")
	word.list <- green.yellow(all.words,guess,green,yellow)
	l <- length(word.list)
	green.letters <- guess.letters[green.vec] |> paste(collapse = '')
	if(length(green.letters) == 0){
		new.guess <- blend.orthog(word.list,guess,yellow)
	} else {
		new.guess <- blend.orthog(word.list,guess,yellow,green.letters)
	}
	green.vec <- is.green(word,new.guess)
	green.i <- green.vec |> as.numeric() |> paste(sep = '')
	green <- paste(green.i[1],green.i[2],green.i[3],green.i[4],green.i[5],sep = "")
	yellow.vec <- is.yellow(word,new.guess)
	yellow.i <- yellow.vec |> as.numeric() |> paste(sep = '')
	yellow <- paste(yellow.i[1],yellow.i[2],yellow.i[3],yellow.i[4],yellow.i[5],sep = "")
	word.list <- green.yellow(word.list,new.guess,green,yellow)
	new.l <- length(word.list)
	ratio <- new.l / l
	green.n <- sum(green.vec)
	yellow.n <- sum(yellow.vec)
	score <- score.calc(green.n,yellow.n)
	blend.table[i,1:6] <- c(l,new.l,ratio,green.n,yellow.n,score)
	blend.table[i,7:9] <- c(word,guess,new.guess)
}
# Orthogonal guess simulation
for(i in 1:iter){
	word <- sample(all.words,1)
	guess <- sample(all.words,1)
	green.vec <- is.green(word,guess)
	green.i <- green.vec |> as.numeric() |> paste(sep = '')
	green <- paste(green.i[1],green.i[2],green.i[3],green.i[4],green.i[5],sep = "")
	yellow.vec <- is.yellow(word,guess)
	yellow.i <- yellow.vec |> as.numeric() |> paste(sep = '')
	yellow <- paste(yellow.i[1],yellow.i[2],yellow.i[3],yellow.i[4],yellow.i[5],sep = "")
	word.list <- green.yellow(all.words,guess,green,yellow)
	l <- length(word.list)
	new.guess <- strict.orthog(word.list)
	green.vec <- is.green(word,new.guess)
	green.i <- green.vec |> as.numeric() |> paste(sep = '')
	green <- paste(green.i[1],green.i[2],green.i[3],green.i[4],green.i[5],sep = "")
	yellow.vec <- is.yellow(word,new.guess)
	yellow.i <- yellow.vec |> as.numeric() |> paste(sep = '')
	yellow <- paste(yellow.i[1],yellow.i[2],yellow.i[3],yellow.i[4],yellow.i[5],sep = "")
	word.list <- green.yellow(word.list,new.guess,green,yellow)
	new.l <- length(word.list)
	ratio <- new.l / l
	green.n <- sum(green.vec)
	yellow.n <- sum(yellow.vec)
	score <- score.calc(green.n,yellow.n)
	orthogonal.table[i,1:6] <- c(l,new.l,ratio,green.n,yellow.n,score)
	orthogonal.table[i,7:9] <- c(word,guess,new.guess)
}
write.csv(standard.table,'~/docs/woRdle/standard-guess-method.csv')
write.csv(blend.table,'~/docs/woRdle/blend-guess-method.csv')
write.csv(orthogonal.table,'~/docs/woRdle/orthogonal-guess-method.csv')
