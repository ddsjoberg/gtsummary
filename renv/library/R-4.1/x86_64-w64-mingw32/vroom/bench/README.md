# Benchmarks for vroom

The benchmarks are run with a makefile, run `make` to run them.

## Personal notes on running benchmarks on AWS

### Starting up
- Create new volume from previous vroom snapshot - `io1` volume type - 2500 IOPS
- Attach volume to instance - `/dev/sda1`

### Attach to instance

`ssh vroom-bench`

When you first start there may be some unattended upgrades installing, use top
to monitor and wait until they are done.

### running

```
make -j 1 \
  TAXI_INPUTS='$(wildcard ~/data/trip_fare*csv)'  \
  FWF_INPUT=~/data/PUMS5_06.TXT \
  BENCH_LONG_ROWS=1000000 \
  BENCH_LOG_COLS=25 \
  BENCH_WIDE_ROWS=100000 \
  BENCH_WIDE_COLS=1000
```

### Tearing down
- Shut down instance
- Detach volume
- Create snapshot
- Delete volume
