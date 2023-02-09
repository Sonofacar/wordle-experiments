minutes <- commandArgs(trailingOnly=T) |> as.numeric()

seconds <- 60 * minutes

for(i in 1:seconds){
	Sys.sleep(1)
	message(seconds - i)
}
