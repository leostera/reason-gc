/*
    iterate will use (i-1) * 2 minor words
    Space O(n)
 */
let rec iterate = (r, x_init, i) =>
  if (i == 1) {
    /* seems to use less memory than (i<1) */
    x_init;
  } else {
    /* two words in minor heap */
    let x = iterate(r, x_init, i - 1);
    r *. x *. (1.0 -. x);
  };

Random.self_init(); /* 1202 minor words */

for (x in 0 to 640) {
  /* 640*2 */
  let r = 4.0 *. float_of_int(x) /. 640.0; /* 2 for binding */
  for (_ in 0 to 39) {
    /* 40*2 */
    let x_init = Random.float(1.0); /* 3 allocations + 2 for binding */
    let x_final = iterate(r, x_init, 500); /* 499*2 + 2 */
    let y = int_of_float(x_final *. 480.); /* 2 for binding */
    y |> ignore;
  };
};

/* These two below are 80 minor words */
Gc.print_stat(stdout); /* 71 minor words */

/**
  1273

  640 * (2 + 2 + 40* (2 + 5 + (499*2 + 2) + 2))
  25 832 960
  25 832 960
  25 744 233
  */;
