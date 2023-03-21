PLOTTERS = plot-20230319.rkt

.PHONY: clean veryclean svg

clean:
	@rm -f *fasl
	@rm -f *~ TAGS

veryclean: clean
	@rm -f *.ldat
	@rm -rf svg

svg:
	racket $(PLOTTERS)
