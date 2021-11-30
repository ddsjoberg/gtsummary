## ---- eval=FALSE, engine="Rcpp"------------------------------------------
## //[[Rcpp::depends(triebeard)]]
## #include <radix.h>

## ---- eval=FALSE, engine="Rcpp"------------------------------------------
## radix_tree<type1, type2> radix;

## ---- eval=FALSE, engine="Rcpp"------------------------------------------
## radix_tree<std::string, std::string> radix;
## radix["turnin"] = "entry the first";
## radix["turin"] = "entry the second";
## 
## radix_tree<std::string, std::string>::iterator it;
## 
## it = radix.longest_match("turing");
## 
## if(it = radix.end()){
##   printf("No match was found :(");
## } else {
##   std::string result = "Key of longest match: " + it->first + " , value of longest match: " + it->second;
## }

## ---- eval=FALSE, engine="Rcpp"------------------------------------------
## radix_tree<std::string, std::string> radix;
## radix["turnin"] = "entry the first";
## radix["turin"] = "entry the second";
## 
## std::vector<radix_tree<std::string, std::string>::iterator> vec;
## std::vector<radix_tree<std::string, std::string>::iterator>::iterator it;
## 
## it = radix.prefix_match("tur");
## 
## if(it == vec.end()){
##   printf("No match was found :(");
## } else {
##   for (it = vec.begin(); it != vec.end(); ++it) {
##     std::string result = "Key of a prefix match: " + it->first + " , value of a prefix match: " + it->second;
##   }
## }

