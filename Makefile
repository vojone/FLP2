BIN_NAME = flp23-log
SRCS = cube.pl input2.pl main.pl ids_solver.pl
ZIP_NAME = flp-log-xdvora3o.zip

$(BIN_NAME): *.pl
	swipl  --stack_limit=16g --goal=main --stand_alone=true -o $(BIN_NAME) -c main.pl

run: flp23-log
	./flp23-log

test:
	./test-moves.sh

zip:
	zip -r $(ZIP_NAME) Makefile README.md tests/* test-moves.sh $(SRCS)

clean:
	rm -f $(BIN_NAME) $(ZIP_NAME)
