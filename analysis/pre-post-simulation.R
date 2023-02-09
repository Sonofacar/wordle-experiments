library(magrittr)
source('~/docs/woRdle/search-functions.R')
standard.table <- read.csv('~/docs/woRdle/standard-guess-method.csv')
blend.table <- read.csv('~/docs/woRdle/blend-guess-method.csv')
orthogonal.table <- read.csv('~/docs/woRdle/orthogonal-guess-method.csv')


Pre.greens.table <- data.frame(
			       Method = character(300000),
			       Length = numeric(300000),
			       Greens = character(300000),
			       Number.greens = numeric(300000),
			       Score = numeric(300000)
)
Post.greens.table <- data.frame(
			        Method = character(300000),
				Length = numeric(300000),
				Greens = character(300000),
				Number.greens = numeric(300000),
				Score = character(300000)
)
Pre.greens.table$Length <- c(standard.table$Pre.length, blend.table$Pre.length, orthogonal.table$Pre.length)
Post.greens.table$Length <- c(standard.table$Post.length, blend.table$Post.length, orthogonal.table$Post.length)
Post.greens.table$Number.greens <- c(standard.table$Greens, blend.table$Greens, orthogonal.table$Greens)
Post.greens.table$Score <- c(standard.table$Score, blend.table$Score, orthogonal.table$Score)
Pre.greens.table[1:100000,1] <- 'Standard' -> Post.greens.table[1:100000,1]
Pre.greens.table[100001:200000,1] <- 'Blend' -> Post.greens.table[100001:200000,1]
Pre.greens.table[200001:300000,1] <- 'Orthogonal' -> Post.greens.table[200001:300000,1]
vector.tmp <- c(standard.table$Word, blend.table$Word, orthogonal.table$Word)
vector.tmp2 <- c(standard.table$Second.guess, blend.table$Second.guess, orthogonal.table$Second.guess)
table.tmp <- vector.is.green(vector.tmp,vector.tmp2)
for(i in 1:300000){
	Post.greens.table$Greens[i] <- table.tmp[,i] |> as.numeric() |> paste(collapse = '')
}
vector.tmp2 <- c(standard.table$First.guess, blend.table$First.guess, orthogonal.table$First.guess)
table.tmp <- vector.is.green(vector.tmp,vector.tmp2)
for(i in 1:300000){
	Pre.greens.table$Greens[i] <- table.tmp[,i] |> as.numeric() |> paste(collapse = '')
}
table.tmp <- vector.is.yellow(vector.tmp,vector.tmp2)
vector.tmp <- Pre.greens.table$Greens |> strsplit('')
for(i in 1:300000){
	Pre.greens.table$Number.greens[i] <- vector.tmp %>% .[[i]] |> as.numeric() |> sum()
	Yellow <- table.tmp[,i] |> as.numeric() |> sum()
	Pre.greens.table$Score[i] <- score.calc(Pre.greens.table$Number.greens[i],Yellow)
}
combined.table <- data.frame(
			     Method = Pre.greens.table$Method,
			     Pre.length = Pre.greens.table$Length,
			     Pre.greens = Pre.greens.table$Greens,
			     Pre.number.greens = Pre.greens.table$Number.greens,
			     Pre.score = Pre.greens.table$Score,
			     Post.length = Post.greens.table$Length,
			     Post.greens = Post.greens.table$Greens,
			     Post.number.greens = Post.greens.table$Number.greens,
			     Post.score = Post.greens.table$Score
)
vector.tmp <- combined.table$Pre.greens |> strsplit('') |> unlist() |> as.numeric() |> as.logical()
vector.tmp1 <- combined.table$Post.greens |> strsplit('') |> unlist() |> as.numeric()
for(i in 1:300000){
	interval <- ((i - 1) * 5 + 1):(i * 5)
	vector.tmp2 <- vector.tmp[interval]
	pre.sum <- vector.tmp2 |> sum()
	vector.tmp3 <- vector.tmp1[interval]
	post.sum <- vector.tmp3[!vector.tmp2] |> sum()
	combined.table$Total.greens[i] <- pre.sum + post.sum
}
write.csv(combined.table,'~/docs/woRdle/pre-post-simulation-greens.csv',row.names = F)

#Same thing with only yellows
Pre.yellows.table <- data.frame(
			       Method = character(300000),
			       Length = numeric(300000),
			       Yellows = character(300000),
			       Number.yellows = numeric(300000),
			       Score = numeric(300000)
)
Post.yellows.table <- data.frame(
			        Method = character(300000),
				Length = numeric(300000),
				Yellows = character(300000),
				Number.yellows = numeric(300000),
				Score = character(300000)
)
Pre.yellows.table$Length <- c(standard.table$Pre.length, blend.table$Pre.length, orthogonal.table$Pre.length)
Post.yellows.table$Length <- c(standard.table$Post.length, blend.table$Post.length, orthogonal.table$Post.length)
Post.yellows.table$Number.yellows <- c(standard.table$Yellows, blend.table$Yellows, orthogonal.table$Yellows)
Post.yellows.table$Score <- c(standard.table$Score, blend.table$Score, orthogonal.table$Score)
Pre.yellows.table[1:100000,1] <- 'Standard' -> Post.yellows.table[1:100000,1]
Pre.yellows.table[100001:200000,1] <- 'Blend' -> Post.yellows.table[100001:200000,1]
Pre.yellows.table[200001:300000,1] <- 'Orthogonal' -> Post.yellows.table[200001:300000,1]
vector.tmp <- c(standard.table$Word, blend.table$Word, orthogonal.table$Word)
vector.tmp2 <- c(standard.table$Second.guess, blend.table$Second.guess, orthogonal.table$Second.guess)
table.tmp <- vector.is.yellow(vector.tmp,vector.tmp2)
for(i in 1:300000){
	Post.yellows.table$Yellows[i] <- table.tmp[,i] |> as.numeric() |> paste(collapse = '')
}
vector.tmp2 <- c(standard.table$First.guess, blend.table$First.guess, orthogonal.table$First.guess)
table.tmp <- vector.is.yellow(vector.tmp,vector.tmp2)
for(i in 1:300000){
	Pre.yellows.table$Yellows[i] <- table.tmp[,i] |> as.numeric() |> paste(collapse = '')
}
table.tmp <- vector.is.green(vector.tmp,vector.tmp2)
vector.tmp <- Pre.yellows.table$Yellows |> strsplit('')
for(i in 1:300000){
	Pre.yellows.table$Number.yellows[i] <- vector.tmp %>% .[[i]] |> as.numeric() |> sum()
	Greens <- table.tmp[,i] |> as.numeric() |> sum()
	Pre.yellows.table$Score[i] <- score.calc(Greens,Pre.yellows.table$Number.yellows[i])
}
combined.table2 <- data.frame(
			     Method = Pre.yellows.table$Method,
			     Pre.length = Pre.yellows.table$Length,
			     Pre.yellows = Pre.yellows.table$Yellows,
			     Pre.number.yellows = Pre.yellows.table$Number.yellows,
			     Pre.score = Pre.yellows.table$Score,
			     Post.length = Post.yellows.table$Length,
			     Post.yellows = Post.yellows.table$Yellows,
			     Post.number.yellows = Post.yellows.table$Number.yellows,
			     Post.score = Post.yellows.table$Score
)
vector.tmp <- combined.table2$Pre.yellows |> strsplit('') |> unlist() |> as.numeric() |> as.logical()
vector.tmp1 <- combined.table2$Post.yellows |> strsplit('') |> unlist() |> as.numeric()
for(i in 1:300000){
	interval <- ((i - 1) * 5 + 1):(i * 5)
	vector.tmp2 <- vector.tmp[interval]
	pre.sum <- vector.tmp2 |> sum()
	vector.tmp3 <- vector.tmp1[interval]
	post.sum <- vector.tmp3[!vector.tmp2] |> sum()
	combined.table2$Total.yellows[i] <- pre.sum + post.sum
}
write.csv(combined.table2,'~/docs/woRdle/pre-post-simulation-yellows.csv',row.names = F)
