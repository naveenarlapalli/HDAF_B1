funct <- function(x = numeric(), y){
  even <- numeric();
  if(y == "even_series"){
    for (i in x) {
      if (i %% 2 == 0){
       
        even <- append(even,i)
      }
    }
    print(even);
  }
  if( y=="last_but_one"){
    result <- x[1: length(x)-1]
    print(result);
  }
  if(y == "stats"){
    result<-summary(x)
    print(result)
  }
  if(y == "NA_summary"){
    return(which(is.na(x)))
  }
  if(y == "check_skew"){
    res<-summary(x)
    if(res[4]>res[3]){
      print("Right skew")
    }
    else if(res[4]<res[3]){
      print("Left skew")
    }
    else{
      print("Mean = Median")
    }
    
  }
}

