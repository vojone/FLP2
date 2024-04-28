flp23-log:
	swipl  --stack_limit=16g --goal=main --stand_alone=true -o flp23-log -c main.pl

run: flp23-log
	./flp23-log

zip:
	zip -r flp-log-xdvora3o.zip *.pl Makefile README.md tests/* test-moves.sh

clean:
	rm -f flp23-log flp-log-xdvora3o.zip
