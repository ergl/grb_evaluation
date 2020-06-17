#!/usr/bin/awk -f

BEGIN {
    FS=","
    max_nr=0
    filecount=0
}

FNR == 1 {
  filecount++
}

FNR >= 2 {
  means[FNR - 1, filecount, 0] = $3
  means[FNR - 1, filecount, 1] = $5

  if (filecount == 1) {
    if ((NR - 1) > max_nr) {
      max_nr = NR - 1
    }

    for (i = 1; i <= NF; i++) {
      a[NR - 1, i] = $i
    }

    total_n[FNR - 1] = $3
  }

  if (filecount > 1) {

    # total n
    total_n[FNR - 1] = $3 + total_n[FNR - 1]

    # n
    a[FNR - 1, 3] = $3 + a[FNR - 1, 3]

    # min
    if ($4 < a[FNR - 1, 4]) {
      a[FNR - 1, 4] = $4
    }

    # max
    if ($10 > a[FNR - 1, 10]) {
      a[FNR - 1, 10] = $10
    }

    # errors
    a[FNR - 1, 11] = $11 + a[FNR - 1, 11]
  }
}

END {
  print "elapsed, window, n, min, mean, max, errors"
  for (i = 1; i <= max_nr; i++) {

    weighted_avg = 0
    for (k = 1; k <= filecount; k++) {
      weighted_avg = weighted_avg + (means[i, k, 0] * means[i, k, 1])
    }

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
    printf("%s", a[i, 11])
    print ""
  }
  print ""
}