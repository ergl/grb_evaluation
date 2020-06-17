#!/usr/bin/awk -f

BEGIN {
    FS=","
    max_nr=0
    filecount=0
}

FNR == 1 {
  filecount++
}

NR >= 2 {
  if (filecount == 1) {
    if ((NR - 1) > max_nr) {
      max_nr = NR - 1
    }

    # If we're on the first file, just collect the data
    for (i = 1; i <= NF; i++) {
      a[NR - 1, i] = $i
    }
  }

  if (filecount > 1) {
    # If we're on any subsequent file, add to the previous data
    # (summaries are always cumulative)
    for (j = 3; j <= NF; j++) {
      a[FNR - 1, j] = $j + a[FNR - 1, j]
    }
  }
}

END {
  print "elapsed, window, total, successful, failed"
  for (i = 1; i <= max_nr; i++ ) {
    for (j = 1; j <= 5; j++) {
      if (j != 5) {
        printf("%s,", a[i,j])
      }
      if (j == 5) {
        printf("%s", a[i,j])
      }
    }
    print ""
  }
  print ""
}
