dict <- read.table("/usr/share/dict/words")
all.words <- dict[nchar(dict$"V1") == 5,] |>
	grep("^[a-z]",x = _,ignore.case = F,perl = T,value = T) |>
	grep("\'",x = _,perl = T,value = T,invert = T) |>
	tolower()

contains <- function(list,letters){
	# This function will take a string containing the desired letters
	# then it will put out all the words in the given list containing those letters
	regex <- ""
	letters.vec <- strsplit(letters,"")[[1]]
	for(i in 1:length(letters.vec)){
		regex <- paste(regex,'(?=.*',letters.vec[i],')',sep = "")
	}
	if(letters == ""){
		regex <- "....."
	}
	new.list <- grep(regex,list,perl = T,value = T)
	return(new.list)
}

no.doubles <- function(list){
	# Takes a list and outputs the same list with no words
	# that contain two of the same letters
	new.list <- grep('(.).*\\1',list,perl = T,value = T,invert = T)
	return(new.list)
}

does.not.contain <- function(list,letters){
	# Takes out the words that contain the desired letters
	# outputs the resulting list
	regex <- paste('[',letters,']',sep = "")
	if(letters == ""){
		new.list <- list
	} else {
		new.list <- grep(regex,list,perl = T,value = T,invert = T)
	}
	return(new.list)
}

green.in <- function(list,guess,green.vec){
	# guess is a string of the guess that resulted in the green vector
	# green.vec is the vector (of 0s and 1s) of which letter is in which location
	# make sure that green.vec is input as a single character string (ex. '01011')
	# this function will give out a list of all words that fit the parameters
	regex <- "" 
	for(i in 1:5){
		log <- as.logical(as.numeric(substr(green.vec,i,i)))
		if(log == T){
			regex <- paste(regex,substr(guess,i,i),sep = "")
		} else {
			regex <- paste(regex,'.', sep = "")
		}
	}
	new.list <- grep(regex,list,perl = T,value = T)
	return(new.list)
}
		
yellow.in <- function(list,guess,yellow.vec){
	# processes the information given by the yellow letters
	# first, it will make sure the resulting list contains the 
	# right letters, then it will make sure they are not in spots they
	# shouldn't be
	letters <- ""
	regex <- ""
	for(i in 1:5){
		log <- as.logical(as.numeric(substr(yellow.vec,i,i)))
		if(log == T){
			regex <- paste(regex,'[^',substr(guess,i,i),']',sep = "")
			letters <- paste(letters,substr(guess,i,i),sep = "")
		} else {
			regex <- paste(regex,'.',sep = "")
		}
	}
	if(!(regex == "")){
		new.list <- grep(regex,list,perl = T,value = T)
		new.list <- contains(new.list,letters)
	} else {
		new.list <- contains(new.list,letters)
	}
	return(new.list)
}

most.common <- function(list){
	# this finds the list of letters that is most common from the list
	new.list <- ""
	for(i in 1:length(list)){
		letters <- strsplit(list[i],"")[[1]]
		new.list[(i * 5 - 4)] <- letters[1]
		new.list[(i * 5 - 3)] <- letters[2]
		new.list[(i * 5 - 2)] <- letters[3]
		new.list[(i * 5 - 1)] <- letters[4]
		new.list[(i * 5)] <- letters[5]
	}
	freq.list <- as.data.frame(table(new.list),row.names = NULL)
	top.ten <- head(freq.list[order(freq.list$Freq,decreasing = T),],n = 10L)
	return(top.ten)
}

green.yellow <- function(list,guess,green,yellow){
	# This function references both green.in and yellow.in
	# it will process both results from a guess and output
	# a list with the possible words
	new.list <- green.in(list,guess,green) |> yellow.in(guess,yellow)
	not.letters <- ""
	guess.vec <- strsplit(guess,'')[[1]]
	green.vec <- strsplit(green,'')[[1]] |> as.numeric() |> as.logical()
	yellow.vec <- strsplit(yellow,'')[[1]] |> as.numeric() |> as.logical()
	green.letters <- guess.vec[green.vec]
	yellow.letters <- guess.vec[yellow.vec]
	for(i in 1:5){
		log1 <- green.vec[i]
		log2 <- yellow.vec[i]
		log3 <- (sum(green.letters %in% guess.vec[i]) + (sum(yellow.letters %in% guess.vec[i]))) > 0
		if((log1 == F) & (log2 == F) & (log3 == F)){
			not.letters <- paste(not.letters,substr(guess,i,i),sep = "")
		}
	}
	new.list <- does.not.contain(new.list,not.letters)
	return(new.list)
}

strict.orthog <- function(list){
	# This function will look at the list given to it, see which letters
	# are common to all words, then search the dictionary for words that
	# don't contain those letters, but contain the other most common ones.
	# If there are multiple, then it will output a random one of those words.
	if(length(list) == 1){
		return(list)
	}
	if(length(no.doubles(list)) == 0){
		l <- list |> length()
		mc.table <- list |> most.common()
	} 
	if(length(no.doubles(list)) > 0){
		l <- list |> no.doubles() |> length()
		mc.table <- list |> no.doubles() |> most.common()
	}
	inter.table <- mc.table[mc.table$Freq < l,]
	letters.vec <- inter.table$new.list[1:5]
	letters <- letters.vec |> paste(collapse = '')
	inter.table <- mc.table[mc.table$Freq >= l,]
	not.letters <- inter.table$new.list[1:dim(inter.table)[1]] |> paste(collapse = '')
	options <- no.doubles(all.words) |> contains(letters) |> does.not.contain(not.letters)
	opt.l <- length(options)
	if(opt.l == 0){
		options <- all.words |> contains(letters) |> does.not.contain(not.letters)
		opt.l <- length(options)
	}
	while(opt.l == 0){
		letters <- substr(letters,1,nchar(letters) - 1)
		options <- no.doubles(all.words) |> contains(letters) |> does.not.contain(not.letters)
		opt.l <- length(options)
		if(opt.l == 0){
			options <- all.words |> contains(letters) |> does.not.contain(not.letters)
			opt.l <- length(options)
		}
	}
	if(nchar(letters) == 0){
		return(sample(list,1))
	} else {
		guess <- sample(options,1)
		return(guess)
	}
}

blend.orthog <- function(list,guess,yellow.vec,green.letters = ''){
	# this function behaves mostly the same as strict.orthog, but it will
	# only get rid of those letters which we know the position of (green
	# letters).
	if(length(list) == 1){
		return(list)
	}
	if(length(green.letters) == 0){
		green.letters <- ''
	}
	if(length(no.doubles(list)) == 0){
		mc.table <- list |> most.common()
	} else {
		mc.table <- list |> no.doubles() |> most.common()
	}
	if(green.letters == ''){
		green.vec <- ''
	} else {
		green.vec <- strsplit(green.letters,"")[[1]]
	}
	inter.table <- mc.table[!(mc.table$new.list %in% green.vec),]
	letters <- inter.table$new.list[1:5] |> paste(collapse = '')
	options <- no.doubles(all.words) |> yellow.in(guess,yellow.vec) |> contains(letters) |> does.not.contain(green.letters)
	opt.l <- length(options)
	while(opt.l == 0){
		letters <- substr(letters,1,nchar(letters) - 1)
		options <- no.doubles(all.words) |> contains(letters) |> does.not.contain(green.letters)
		opt.l <- length(options)
		if(opt.l == 0){
			options <- all.words |> contains(letters) |> does.not.contain(green.letters)
		}
		opt.l <- length(options)
	}
	guess <- sample(options,1)
	return(guess)
}

standard.guess <- function(list){
	# This function will take the given list, find the most common letters,
	# and filter by that to find a word that has all of the most common
	# letters (or the most it can find). If there are multiple words in
	# the result, it will output a random one.
	mc.table <- list |> most.common()
	letters.vec <- mc.table$new.list[1:5]
	letters <- paste(letters.vec[1],letters.vec[2],letters.vec[3],letters.vec[4],letters.vec[5],sep = "")
	options <- no.doubles(list) |> contains(letters)
	if(length(options) == 0 ){
		options <- contains(list,letters)
	}
	opt.l <- length(options)
	while(opt.l == 0){
		letters <- substr(letters,1,nchar(letters) - 1)
		options <- list |> contains(letters)
		opt.l <- length(options)
	}
	guess <- sample(options,1)
	return(guess)
}

score.calc <- function(green,yellow){
	# This function simply calculates a score based on the number
	# of greens and yellows in the results of a guess. This is 
	# simply for grading purposes. Out of 100 (100 is a win).
	# This equation is not scientifically calculated, though it is
	# roughly equivalent to theory.
	if(yellow != 0){
		remaining <- 4 - green
		vec <- remaining:0
		yellow.subscore <- sum(2^(.8 - vec[1:yellow]) + .1)
	} else {
		yellow.subscore <- 0
	}
	score <- 20 * (green + yellow.subscore)
	return(score)
}

is.green <- function(word,guess){
	word.vec <- strsplit(word,'')[[1]]
	guess.vec <- strsplit(guess,'')[[1]]
	comparison <- outer(guess.vec,word.vec,FUN = Vectorize(identical))
	green.vec <- NULL
	for(i in 1:5){
		green.vec[i] <- comparison[i,i]
	}
	return(green.vec)
}

is.yellow <- function(word,guess){
	word.vec <- strsplit(word,'')[[1]]
	guess.vec <- strsplit(guess,'')[[1]]
	comparison <- outer(guess.vec,word.vec,FUN = Vectorize(identical))
	yellow.vec <- NULL
	for(j in 1:5){
		if(comparison[j,j] == T){
			n <- 1:5
			n <- n[n != j]
			comparison[j,n] <- FALSE
			comparison[n,j] <- FALSE
		}
		if(sum(comparison[,j]) > 1){
			n <- which(comparison[,j] %in% TRUE)
			for(k in 2:length(n)){
				comparison[n[k],j] <- FALSE
			}
		}
		if(sum(comparison[j,]) > 1){
			n <- which(comparison[j,] %in% TRUE)
			for(k in 2:length(n)){
				comparison[j,n[k]] <- FALSE
			}
		}
		if(comparison[j,j] == T){
			yellow.vec[j] <- F
		} else {
			x <- sum(comparison[j,])
			if(x == 0){
				y <- F
			} else {
				y <- T
			}
			yellow.vec[j] <- y
		}
	}
	return(yellow.vec)
}

vector.is.green <- Vectorize(is.green)

vector.is.yellow <- Vectorize(is.yellow)
