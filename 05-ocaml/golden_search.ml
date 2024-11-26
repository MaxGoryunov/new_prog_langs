let golden_section_search f a b tol =
  let phi = (1.0 +. sqrt 5.0) /. 2.0 in
  let resphi = 1.0 /. phi in
  Printf.printf "resphi = %.6f \n" resphi;
  let neg_inf = neg_infinity in
  (* let resphi = 2.0 -. phi in *)

  let rec search a b x_1 x_2 =
    Printf.printf "a = %.6f " a;
    Printf.printf "b = %.6f " b;
    Printf.printf "x_1 = %.6f " x_1;
    Printf.printf "x_2 = %.6f\n" x_2;
    if abs_float (b -. a) < tol then
      (a +. b) /. 2.0 
    else
      let width = b -. a in
      let x1 = if x_1 = neg_inf then b -. resphi *. width else x_1 in
      Printf.printf "new x1 = %.6f \n" x1;
      let x2 = if x_2 = neg_inf then a +. resphi *. width else x_2 in
      Printf.printf "new x2 = %.6f \n" x2;
      
      if f x1 < f x2 then
        search a x2 neg_inf x1
      else
        search x1 b x2 neg_inf
  in
  search a b neg_inf neg_inf


let example_function x =
  let a = 2.0 in
  (x -. a) ** 2.0 +. 1.0


let () =
  let a = 0.0 in
  let b = 4.0 in
  let tolerance = 1e-1 in
  let minimum = golden_section_search example_function a b tolerance in
  Printf.printf "The minimum is at x = %f with value f(x) = %f\n" minimum (example_function minimum)
