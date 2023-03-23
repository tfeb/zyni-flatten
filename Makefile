PLOTTERS = plot-20230319.rkt plot-20230323.rkt

.PHONY: clean veryclean svg

clean:
	@rm -f *fasl
	@rm -f *~ TAGS

veryclean: clean
	@rm -f *.ldat
	@rm -rf svg

svg:
	for file in $(PLOTTERS); do racket "$$file"; done
