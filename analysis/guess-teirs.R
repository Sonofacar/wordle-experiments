# Setup
source('~/docs/woRdle/search-functions.R')
tier1 <- all.words |> grep('(?=.*s)',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier2 <- all.words |> grep('(?=.*s)(?=.*e)',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier3 <- all.words |> grep('(?=.*s)(?=.*e)(?=.*a)',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier4 <- all.words |> grep('(?=.*s)(?=.*e)(?=.*a)(?=.*r)',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier5 <- all.words |> grep('(?=.*s)(?=.*e)(?=.*a)(?=.*r)(?=.*o)',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier6 <- all.words |> grep('[rosael]{5}',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier7 <- all.words |> grep('[rosaeli]{5}',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier8 <- all.words |> grep('[rosaelit]{5}',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier9 <- all.words |> grep('[rosaelitn]{5}',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier10 <- all.words |> grep('[rosaelitnd]{5}',x = _,perl = T,value = T) |> 
	grep('(.).*\\1',x = _,perl = T,value = T,invert = T)
tier4 <- setdiff(tier4,tier5)
tier3 <- setdiff(tier3,tier4)
tier2 <- setdiff(tier2,tier3)
tier1 <- setdiff(tier1,tier2)

# Simulation
reps <- 10000
tierlist <- list(tier1,tier2,tier3,tier4,tier5,tier6,tier7,tier8,tier9,tier10)
guess.data <- data.frame("Tier" = numeric(1000), "Yellow" = numeric(1000), "Green" = numeric(1000))
temp.data <- data.frame("Tier" = numeric(10), "Yellow" = numeric(10), "Green" = numeric(10))
for(x in 1:reps){
	for(i in 1:10){
		# Get the word to guess, then make a vector of the letters it contains
		the.word <- sample(all.words,1)
		the.word.letters <- NULL
		for(j in 1:5){the.word.letters[j] <- substr(the.word,j,j)}

		# Get the guess and do the same
		guess <- sample(tierlist[[i]],1)
		guess.letters <- NULL
		for(j in 1:5){guess.letters[j] <- substr(guess,j,j)}

		# Compare the the two words to get the amount of green and yellow letters
		green <- the.word.letters == guess.letters
		yellow <- NULL
		yellow[1] <- guess.letters[1] == the.word.letters[2] | guess.letters[1] == the.word.letters[3] | guess.letters[1] == the.word.letters[4] | guess.letters[1] == the.word.letters[5]
		yellow[2] <- guess.letters[2] == the.word.letters[1] | guess.letters[2] == the.word.letters[3] | guess.letters[2] == the.word.letters[4] | guess.letters[2] == the.word.letters[5]
		yellow[3] <- guess.letters[3] == the.word.letters[1] | guess.letters[3] == the.word.letters[2] | guess.letters[3] == the.word.letters[4] | guess.letters[3] == the.word.letters[5]
		yellow[4] <- guess.letters[4] == the.word.letters[1] | guess.letters[4] == the.word.letters[2] | guess.letters[4] == the.word.letters[3] | guess.letters[4] == the.word.letters[5]
		yellow[5] <- guess.letters[5] == the.word.letters[1] | guess.letters[5] == the.word.letters[2] | guess.letters[5] == the.word.letters[3] | guess.letters[5] == the.word.letters[4]
		for(j in 1:5){
			if(green[j] == T){
				yellow[j] <- F
			}
		}
		y.count <- sum(yellow)
		g.count <- sum(green)
		temp.data[i,] = c(i,y.count,g.count)
	}
	start <- (x * 10) - 9
	end <- x * 10
	guess.data[start:end,] <- temp.data[1:10,]
}

write.csv(guess.data,'~/docs/woRdle/guess-table.csv')
