all:
	swipl  -G16g --goal=main --stand_alone=true -o flp23-log -c solver.pl

zip:
	zip -r xdvora3o.zip *.pl Makefile README.md tests/*

clean:
	rm -f flp23-log xdvora3o.zip
