response <- NULL
tries <- 0
while(is.null(response)){
	Sys.sleep(.1)
	response <- readLines(pipe_out)
	if(length(response) < 2){
		response <- NULL
	}
	tries <- tries + 1
	if(tries == 50){
		response <- 'hanging'
	}
}
if(length(response) == 1){
	if(response == 'hanging'){
		message('Game is hanging, restarting...')
		response <- NULL
		break
	}
}
green   <- response[1]
greens  <- green |> strsplit('') |> unlist() |> as.numeric() |> sum()
yellow  <- response[2]
yellows <- yellow |> strsplit('') |> unlist() |> as.numeric() |> sum()
if(quiet == F){
	message(guess)
	message(green)
	message(yellow)
}
Pre.length                   <- length(possible.words)
possible.words               <- green.yellow(possible.words,guess,green,yellow)
Post.length                  <- length(possible.words)
sub.method                   <- guessr(greens, yellows)
guess.table.tmp$Method       <- method
guess.table.tmp$Sub.method   <- sub.method
guess.table.tmp$Guess.number <- i
guess.table.tmp$Guess        <- guess
guess.table.tmp$Ratio        <- Post.length / Pre.length
guess.table.tmp$Greens       <- greens
guess.table.tmp$Yellows      <- yellows
guess.table.tmp$Post.score   <- score.calc(greens,yellows)
write.table(guess.table.tmp,'~/data/woRdle/guess-data.csv',row.names = F,append = T,sep = ',',col.names = F)
if(green == '11111'){
	if(quiet == F){
		message('Correct!!!!!')
	}
	game.table.tmp$Method         <- method
	game.table.tmp$Word           <- guess
	game.table.tmp$Number.guesses <- i
	game.table.tmp$Win[1]         <- TRUE
	write.table(game.table.tmp,'~/data/woRdle/game-data.csv',row.names = F,append = T,sep = ',',col.names = F)
	break
}
if(i == 6){
	if(quiet == F){
		message('You lost :(')
		message('Here is the word:')
		message(response[4])
	}
	game.table.tmp$Method         <- method
	game.table.tmp$Word           <- response[4]
	game.table.tmp$Number.guesses <- i
	game.table.tmp$Win[1]         <- FALSE
	write.table(game.table.tmp,'~/data/woRdle/game-data.csv',row.names = F,append = T,sep = ',',col.names = F)
	break
}
green.logic   <- green |> strsplit('') |> unlist() |> as.numeric() |> as.logical()
guess.vector  <- guess |> strsplit('') |> unlist() 
green.letters <- guess.vector[green.logic] |> paste(collapse = '')
if(sub.method == 'standard'){
	guess <- standard.guess(possible.words)
}
if(sub.method == 'blend'){
	guess <- blend.orthog(possible.words,guess,yellow,green.letters)
}
if(sub.method == 'orthogonal'){
	guess <- strict.orthog(possible.words)
}
writeLines(guess, pipe_in)
flush(pipe_in)

