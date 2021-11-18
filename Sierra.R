Sierra<- function(numClass = 35,odds = 2,countoDo = 637){
  
  days=1:365
  count=countoDo
  repeatn=numClass
  varstor=numeric()
  Sierra = 0
  for (i in 1:countoDo) {
    sam=sample(days,size=numClass,replace = T)
    y=duplicated(sam)
    if (sum(y)>0) Sierra=Sierra+1
    else Sierra = Sierra - odds
  }
  return(Sierra)
}