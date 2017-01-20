toGroup <- function(x, grp) {
  if (grp %in% c("Age-5", "Age-10")) {
    Int <- as.numeric(regmatches(grp, regexpr("[0-9]*$", grp)))
    y <- paste(floor(x/Int)*Int, floor(x/Int)*Int + Int -1, sep = "_")
    if (grepl("^100", y)) {y <- "100_plus"}
  } else {
    if (x <= 34) {
      y <- "0_34"
    } else if (x >= 35 & x <= 49) {
      y <- "35_49"
    } else if (x >= 50 & x <= 69) {
      y <- "50_69"
    } else if (x >= 70) {
      y <- "70_plus"
    }
  }
  return(y)
}

code2str5 <- function(x) {
  if (x == "1")  {return("Public screening")}
  else if (x == "2")  {return("Private screening")}
  else if (x == "3")  {return("Symptoms")}
  else if (x == "4")  {return("Concern without symptoms")}
  else if (x == "5")  {return("High-risk group")}
  else if (x == "6")  {return("Hormone substitution")}
  else if (x == "7")  {return("Metastasis")}
  else if (x == "8")  {return("Random findings")}
  else if (x == "9")  {return("Control for previous cancer")}
  else if (x == "10") {return("Runtine control at GP")}
  else if (x == "99") {return("Unknown")}
}

code2str6 <- function(x) {
  if (x == "1")  {return("Referred to further examinations")}
  else if (x == "2")  {return("Treatment at same institution")}
  else if (x == "3")  {return("Treatment at different institution")}
  else if (x == "4")  {return("No treatment without metastasis")}
  else if (x == "5")  {return("No treatment with metastasis")}
  else if (x == "99") {return("Unknown")}
}

## Function for plotting Highcharts;
List <- function(n) {
  if (length(n) == 1) {
    return(list(as.character(n)))
  } else {
    return(as.character(n))
  }
}


Round <- function(x) {
  if (is.data.frame(x)) {
    n <- ncol(x)
    for(i in 1:n) {
      if (is.numeric(x[, i])) {
        if (!is.integer(x[, i])) {
          x[, i] <- round(x[, i], 3) 
        }
      }
    }
    return(x)
  } else {
    return(x)
  }
}

## Color tables;
SKDEColor <- c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#C6DBEF",     
               "#CCCCCC", "#A6A6A6",   "#737373","#4D4D4D", "#FF7260")

