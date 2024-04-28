BIN_NAME = flp23-log
SRCS = cube.pl input2.pl main.pl ids_solver.pl
ZIP_NAME = flp-log-xdvora3o.zip

$(BIN_NAME): *.pl
	swipl --goal=main --toplevel=halt -G16g --stand_alone=true -o $(BIN_NAME) -c main.pl


run: $(BIN_NAME)
	./$(BIN_NAME)

test:
	./test-moves.sh

zip:
	zip -r $(ZIP_NAME) Makefile README.md tests/* examples/* test-moves.sh $(SRCS)

clean:
	rm -f $(BIN_NAME) $(ZIP_NAME)
