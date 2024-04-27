all:
	swipl  --stack_limit=16g --goal=main --stand_alone=true -o flp23-log -c solver.pl

zip:
	zip -r flp-log-xdvora3o.zip *.pl Makefile README.md tests/* test-moves.sh

clean:
	rm -f flp23-log flp-log-xdvora3o.zip
