pkg <- "RcppEigen"

# load this package
require( pkg, character.only = TRUE )

#load RUnit
runit <- "RUnit" ; require( runit, character.only = TRUE )
if( file.exists( "unitTests-results" ) ){ unlink("unitTests-results", recursive = TRUE ) }
dir.create( "unitTests-results" ) 

path <- system.file("unitTests", package = pkg)
testSuite <- defineTestSuite(name=paste(pkg, "unit testing"), dirs = path)
tests <- runTestSuite(testSuite)
printHTMLProtocol(tests, fileName= sprintf( "unitTests-results/%s-unitTests.html" , pkg ) )
printTextProtocol(tests, fileName= sprintf( "unitTests-results/%s-unitTests.txt"  , pkg ) )
if( file.exists( "/tmp" ) ){
	file.copy( sprintf( "unitTests-results/%s-unitTests.txt" , pkg ) , "/tmp", overwrite = TRUE )
	file.copy( sprintf( "unitTests-results/%s-unitTests.html", pkg ) , "/tmp", overwrite = TRUE )
}

