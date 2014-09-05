#!/bin/sh

function run_test {
	stem=$1
	start="starts/${stem}.start"
	canonical="canonical_runs/${stem}.data"
	dump_file=$(mktemp)
	./life_callable.native --t 0 --write-to-file ${dump_file} --steps 500 ${start}
	diff ${dump_file} ${canonical}
	return
	}

function test_loop {
	for stem in $(cat full_length_tests) ; do
		if run_test ${stem} ; then
			echo -n . ; # No-op
		else
			echo "${stem} failed";
		fi;
	done
	}

test_loop
echo
