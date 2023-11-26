
all : TypeChecker

TypeChecker :
	cd src && cabal build && cd ..
	cp src/dist-newstyle/build/x86_64-linux/ghc-8.8.4/Latte-language-compiler-0.1.0.0/x/LatteTypeChecker/build/LatteTypeChecker/LatteTypeChecker ./

clean :
	rm LatteTypeChecker
	rm -r src/dist-newstyle
