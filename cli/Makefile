deb: completions/bash/moshell
	cargo deb -p cli --deb-version $(shell git log --date=format:%Y%m%d --pretty=0.0~git%cd.%h | head -n 1)

completions/bash/moshell: | completions/bash
	cargo run -- --completions bash > $@

completions/bash:
	mkdir -p $@

.PHONY: deb
