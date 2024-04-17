#!/bin/awk -f
BEGIN {
  aop = 0    # 0 for add, 1 for mult
  name = " " # name of function

  while((getline line) > 0) {
    # predicate determining whether to skip the current line
    skipP = match(line, "Compiling") + match(line, "Reporting") + match(line, "automatically")
    skipP = (line == "") || skipP

    if (!skipP) {
      split(line, fs, " ")
      runtime = int(fs[3])

      if (runtime == 0) {
        split(fs[1], tmp, ":")
        # set arithmetic operation
        name = tmp[2]
        if (match(fs[1], "add") != 0)
          aop = 0
        else
          aop = 1
        print
      }
      else {

        # addition
        if (aop == 0) {
          patsplit(fs[1], info, "[0-9]+")
          num_instances = info[1]
          m = info[2]
          base = info[3]
          bits = m * base
          bytes_accesses = 3 * num_instances * m * (base / 8)
          gigabytes = bytes_accesses / (runtime * 1000)
          printf "%s of %d-bit integers (base u%d) runs %d instances in:\t", name, bits, base, num_instances
          printf "%d microsecs, GB/sec: %d", runtime, gigabytes
          printf "\t %s %s %s %s %s\n", fs[4], fs[5], fs[6], fs[7], fs[8]
        }

        # multiplication
        if (aop == 1) {
          patsplit(fs[1], info, "[0-9]+")
          num_instances = info[1]
          m = info[2]
          base = info[3]
          bits = m * base
          num_u32_ops = 4 * num_instances * (bits / 32) * (bits / 32)
          if (match(name, "six") != 0) {
            num_u32_ops = num_u32_ops * 6
          }
          gigaopsu32 = num_u32_ops / (runtime * 1000)
          printf "%s of %d-bit integers (base u%d) runs %d instances in:\t", name, bits, base, num_instances
          printf "%d microsecs, Gu32ops/sec: %.2f", runtime, gigaopsu32
          printf "\t %s %s %s %s %s\n", fs[4], fs[5], fs[6], fs[7], fs[8]
        }
      }
    }
  }
}
