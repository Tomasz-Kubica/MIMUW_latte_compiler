
all : TypeChecker Compiler

TypeChecker :
	cd src && cabal build LatteTypeChecker && cd ..
	cp src/dist-newstyle/build/x86_64-linux/ghc-8.8.4/Latte-language-compiler-0.1.0.0/x/LatteTypeChecker/build/LatteTypeChecker/LatteTypeChecker ./

Compiler :
	cd src && cabal build LatteCompiler && cd ..
	cp src/dist-newstyle/build/x86_64-linux/ghc-8.8.4/Latte-language-compiler-0.1.0.0/x/LatteCompiler/build/LatteCompiler/LatteCompiler ./

clean :
	rm LatteTypeChecker
	rm LatteCompiler
	rm -r src/dist-newstyle
