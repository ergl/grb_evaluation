#!/usr/bin/awk -f

BEGIN {
    FS=","
    max_nr=0
    filecount=0
}

# FNR is the line number for the current file,
# so every time we reach 1, it means we're processing
# a new file
FNR == 1 {
  filecount++
}

# The first line is the header for the CSV, so only
# act on the data lines
FNR >= 2 {
  # Build an array of `n` (column 3) and
  # `mean` (column 5)
  means[FNR - 1, filecount, 0] = $3
  means[FNR - 1, filecount, 1] = $5

  if (filecount == 1) {
    # our final file will have as much lines as the first one
    # so keep this updated
    if ((NR - 1) > max_nr) {
      max_nr = NR - 1
    }

    # copy all the lines of the first file into the final
    # file (all columns)
    for (i = 1; i <= NF; i++) {
      a[NR - 1, i] = $i
    }

    # keep track of all the `n`s in the first file here
    # we'll add to this array as we process more files
    total_n[FNR - 1] = $3
  }

  if (filecount > 1) {

    # total n,
    # add the `n` in the current record number
    # to the accumulator
    total_n[FNR - 1] = $3 + total_n[FNR - 1]

    # n
    # do the same for the `a` array, that holds
    # all the lines for the first record
    a[FNR - 1, 3] = $3 + a[FNR - 1, 3]

    # rolling min (column 4)
    if ($4 < a[FNR - 1, 4]) {
      a[FNR - 1, 4] = $4
    }

    # rolling max (column 10)
    if ($10 > a[FNR - 1, 10]) {
      a[FNR - 1, 10] = $10
    }

    # errors, column 11
    a[FNR - 1, 11] = $11 + a[FNR - 1, 11]
  }
}

END {
  print "elapsed, window, n, min, mean, max, errors, median"
  for (i = 1; i <= max_nr; i++) {

    # means(i, k, 0) contains the `n` for line `i` of file `k`
    # means(i, k, 1) contains the `mean` for line `i` of file `k`
    # weighed mean then is taken by doing n*mean
    weighted_avg = 0
    for (k = 1; k <= filecount; k++) {
      weighted_avg = weighted_avg + (means[i, k, 0] * means[i, k, 1])
    }

    # sometimes we get a line without ops, in those cases,
    # ignore the latency (don't divide by 0)
    if (total_n[i] != 0 ) {
      weighted_avg = weighted_avg / total_n[i]
    } else {
      weighted_avg = 0
    }

    # print elapsed, window, n and min
    for (j = 1; j <= 4; j++) {
      printf("%s,", a[i, j])
    }

    printf("%s,", weighted_avg)

    # print max
    printf("%s,", a[i, 10])
    # print errors
    printf("%s,", a[i, 11])
    # print median (only from the first file)
    printf("%s", a[i, 6])
    print ""
  }
  print ""
}