cppCompiler := g++-7
appName := Simplex
version := -std=c++14
includeFlag := -I/usr/local/Cellar/nlohmann_json/3.1.2/include

compile:
	@echo "Compiling.."
	$(cppCompiler) $(version) $(includeFlag) $(appName).cpp \
	-o $(appName)
	@echo "Compiled"

execute:
	./$(appName)

compileAndExecute:
	$(cppCompiler) $(version) $(includeFlag) $(appName).cpp \
	-o $(appName)
	./$(appName)
	
