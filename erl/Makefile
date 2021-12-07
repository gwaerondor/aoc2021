day%:
	erlc ./main.erl
	erlc ./$@.erl
	$(MAKE) run$*

run%:
	erl -pa . -noshell -eval "main:run($*)" -run init stop
