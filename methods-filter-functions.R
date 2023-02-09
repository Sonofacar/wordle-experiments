if(method == 'ratio-based'){
	guessr <- function(greens, yellows){
		if(greens == 0){
			return('standard')
		}
		if(yellows == 0){
			return('blend')
		}
		if(yellows == 1){
			return('orthogonal')
		}
		if(greens == 1){
			return('standard')
		}
		if(yellow == 2){
			return('orthogonal')
		}
		if(yellow == 3){
			return('standard')
		}
		if(green == 2){
			return('orthogonal')
		}
		if(yellow == 4){
			return('orthogonal')
		}
		if(green == 3){
			return('orthogonal')
		}
		if(green == 4){
			return('blend')
		}
		return('standard')
	}
}
if(method == 'standard'){
	guessr <- function(greens, yellows){
		return('standard')
	}
}
if(method == 'blend'){
	guessr <- function(greens, yellows){
		return('blend')
	}
}
if(method == 'orthogonal'){
	guessr <- function(greens, yellows){
		return('orthogonal')
	}
}
if(method == 'max-yellows-added'){
	guessr <- function(greens, yellows){
		if(yellows >= 0 & yellows <= 3){
			return('blend')
		}
		if(yellows > 3){
			return('standard')
		}
		return('standard')
	}
}
if(method == 'random'){
	guessr <- function(greens, yellows){
		random.method <- sub.methods.vector |> sample(1)
		return(random.method)
	}
}
