# Just a cleaner

.PHONY: clean veryclean

clean:
	@rm -f *fasl
	@rm -f *~ TAGS

veryclean: clean
	@rm -f *.ldat
