mae <- function(orig, pred){
  return(sum(abs(orig - pred))/length(orig))
}

sd_error <- function(orig, pred){
  return(sd((orig - pred)))
}
