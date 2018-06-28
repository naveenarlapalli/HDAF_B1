rhymes<-function(){
a <- c("ten", "nine", "eight", "seven", "six", "five",
            "four", "three", "two", "one")
for (i in a) {
  cat(stringr::str_c("There were ", i, " in the bed\n"))
  cat("and the little one said\n")
  if (i == "one") {
    cat("I'm lonely...")
    cat("\n")
  } 
  else {
    cat("Roll over, roll over\n")
    
  }
  cat("So they all rolled over and one fell out.\n")
  cat("\n")
}
}
