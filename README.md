# 1brc-cl

This repo is an implementation of [1brc](https://github.com/gunnarmorling/1brc) in common lisp. The implementation I am using is SBCL.

## 1brc implementations

- `1brc.ros` - single threaded and otherwise naive implmentation. Generally similar to the [original java implementation](https://github.com/gunnarmorling/1brc/blob/main/src/main/java/dev/morling/onebrc/CalculateAverage_baseline_original_rounding.java) from @gunnarmorling
    - time for 1 billion rows: `./1brc.ros measurements3.txt  923.52s user 8.90s system 99% cpu 15:34.57 total`
