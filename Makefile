FC = gfortran
FFLAGS = -std=legacy -Wall
GENERATOR_EXEC = generator/a.exe 
MAIN_EXEC = main/a.exe
CONVERSION_EXEC = conversion/a.exe

GENERATOR_SRC = generator/generator.for
MAIN_SRC = main/main.for
CONVERSION_SRC = conversion/conversion.for

all:
	@echo "make run1 - Compiling generator"
	@echo "make run2 - Compiling main"
	@echo "make run3 - Compiling conversion"

$(GENERATOR_EXEC): $(GENERATOR_SRC)
	@echo "Compiling generator..."
	$(FC) $(FFLAGS) -o $(GENERATOR_EXEC) $(GENERATOR_SRC)
	@echo "Generator compiled successfully."

$(MAIN_EXEC): $(MAIN_SRC)
	@echo "Compiling main program..."
	$(FC) $(FFLAGS) -o $(MAIN_EXEC) $(MAIN_SRC)
	@echo "Main program compiled successfully."

$(CONVERSION_EXEC): $(CONVERSION_SRC)
	@echo "Compiling conversion program..."
	$(FC) $(FFLAGS) -o $(CONVERSION_EXEC) $(CONVERSION_SRC)
	@echo "Conversion program compiled successfully."

run1: $(GENERATOR_EXEC)
	@echo "Running generator..."
	./$(GENERATOR_EXEC)

run2: $(MAIN_EXEC)
	@echo "Running main program..."
	./$(MAIN_EXEC)

run3: $(CONVERSION_EXEC)
	@echo "Running conversion program..."
	./$(CONVERSION_EXEC)

clean:
	@echo "Cleaning up..."
	powershell -Command "Remove-Item -Path $(GENERATOR_EXEC),$(MAIN_EXEC),$(CONVERSION_EXEC) -Force"
	rm -rf $(GENERATOR_EXEC),$(MAIN_EXEC),$(CONVERSION_EXEC)
	@echo "Cleanup complete."

.PHONY: all run1 run2 run3 clean
