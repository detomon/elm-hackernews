ELM_FLAGS = --optimize
UGLIFY = uglifyjs
UGLIFY_ELM_FLAGS = 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe'

.PHONY: main
main: \
	elm.min.js

elm.js: src/Main.elm
	elm make $(ELM_FLAGS) $< --output=$@

elm.min.js: elm.js
	$(UGLIFY) $< --compress $(UGLIFY_ELM_FLAGS) | $(UGLIFY) --mangle --output=$@

.PHONY: clean
clean:
	rm -f elm.min.js
