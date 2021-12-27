MAKEFLAGS += --no-builtin-rules

BENCH_LONG_ROWS := 10000
BENCH_LONG_COLS := 25
BENCH_WIDE_ROWS := 1000
BENCH_WIDE_COLS := 100

BENCH_INPUTS := all_numeric-long/input.tsv all_numeric-wide/input.tsv all_character-long/input.tsv all_character-wide/input.tsv
TAXI_INPUTS := $(wildcard ~/data/small_trip_fare_*.csv)
FWF_INPUT := ~/data/small_PUMS5_06.TXT
BENCH_SRC := $(wildcard */*.R)
BENCH_MARKS := $(BENCH_SRC:.R=.tsv)
BENCH_OUT := $(patsubst %/,%.tsv, $(wildcard */))

all: $(BENCH_OUT) session_info.tsv

$(BENCH_OUT) : $(BENCH_MARKS)
	Rscript summarise-benchmarks.R

session_info.tsv: session_info.R
	./$<

%-long/input.tsv: %-long/input.R
	Rscript $< $(BENCH_LONG_ROWS) $(BENCH_LONG_COLS) $@

%-wide/input.tsv: %-wide/input.R
	Rscript $< $(BENCH_WIDE_ROWS) $(BENCH_WIDE_COLS) $@

taxi/%.tsv : taxi/%.R $(TAXI_INPUTS)
	run-bench.R $< $@ $(word 1, $(TAXI_INPUTS))

taxi_multiple/%.tsv : taxi_multiple/%.R $(TAXI_INPUTS)
	run-bench.R $< $@ $(TAXI_INPUTS)

taxi_writing/%.tsv : taxi_writing/%.R $(TAXI_INPUTS)
	run-bench.R $< $@ $(word 1, $(TAXI_INPUTS))

fwf/%.tsv : fwf/%.R $(FWF_INPUT)
	run-bench-fwf.R $< $@ $(FWF_INPUT)

%.tsv : %.R $(BENCH_INPUTS)
	run-bench.R $< $@ $(@D)/input.tsv

clean:
	rm -f $(BENCH_INPUTS) $(BENCH_MARKS) $(BENCH_OUT) session_info.tsv
