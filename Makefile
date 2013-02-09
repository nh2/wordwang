all: clean init build


staticfetcher.py:
	wget https://raw.github.com/nh2/staticfetcher/master/staticfetcher.py

.PHONY: statics_fetch statics_fetch_force statics_clean clean init dev test test_headless

statics_fetch: staticfetcher.py
	python statics.py fetch

statics_fetch_force: staticfetcher.py
	python statics.py fetch --force

statics_clean: staticfetcher.py
	python statics.py clean


clean:
	(test -f staticfetcher.py && make statics_clean) || true
	rm -f staticfetcher.py

init: statics_fetch_force

dev: statics_fetch
	./dev.py


test:
	@echo "See ../client/test.html for browser tests"
	mocha --compilers coffee:coffee-script spec/*  -R spec -w

test_headless:
	rm -rf test/headless/gen/screenshots/*
	cd test/headless && casperjs test ./


uglify:
