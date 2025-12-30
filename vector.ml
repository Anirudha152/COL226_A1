type vector = float list

exception DimensionError of string

let create (n : int) (x : float) =
    if n < 1 then 
        raise (DimensionError "Dimension must be at least 1")
    else 
        let rec create_rec (v : vector) (i : int) = 
            if i = 0 then 
                v 
            else 
                create_rec (x :: v) (i - 1)
        in create_rec [] n

let dim (v : vector) =
    let rec dim_rec (i : int) = function
        | [] -> i
        | _ :: t -> dim_rec (i + 1) t
    in dim_rec 0 v

let is_zero (v : vector) =
    let rec is_zero_rec = function
        | [] -> true
        | h :: t -> if h != 0.0 then false else is_zero_rec t
    in is_zero_rec v

let unit (n : int) (j : int) =
    if j < 1 || j > n then 
        raise (DimensionError "Invalid index for unit vector")
    else 
        let rec unit_rec (v : vector) (i : int) = 
            if i = 0 then 
                v 
            else 
                unit_rec ((if i = j then 1.0 else 0.0) :: v) (i - 1) 
        in unit_rec [] n

let scale (c : float) (v : vector) =
    List.map (fun x -> c *. x) v

let addv (v1 : vector) (v2 : vector) =
    if dim v1 <> dim v2 then 
        raise (DimensionError "Vectors must have the same dimension")
    else 
        let rec addv_rec (v1 : vector) (v2 : vector) =
            match v1, v2 with
            | [], [] -> []
            | h1 :: t1, h2 :: t2 -> (h1 +. h2) :: addv_rec t1 t2
            | _ -> raise (DimensionError "Vectors must have the same dimension")
        in addv_rec v1 v2

let dot_prod (v1 : vector) (v2 : vector) =
    if dim v1 <> dim v2 then 
        raise (DimensionError "Vectors must have the same dimension")
    else 
        let rec dot_prod_rec (s : float) (v1 : vector) (v2 : vector) =
            match v1, v2 with
            | [], [] -> s
            | h1 :: t1, h2 :: t2 -> dot_prod_rec (s +. (h1 *. h2)) t1 t2
            | _ -> raise (DimensionError "Vectors must have the same dimension")
        in dot_prod_rec 0.0 v1 v2

let inv (v : vector) = scale (-1.0) v

let length (v : vector) = 
    let rec length_rec (s : float) = function
        | [] -> sqrt s
        | h :: t -> length_rec (s +. (h *. h)) t
    in length_rec 0.0 v

let angle (v1 : vector) (v2 : vector) =
    let len1 = length v1 in
    let len2 = length v2 in
    if len1 = 0.0 || len2 = 0.0 then raise (DimensionError "Cannot compute angle with zero-length vector")
    else (acos ((dot_prod v1 v2) /. (len1 *. len2)));;


(* Testcases *)
(* Create tests *)
Printf.printf "Create tests:\n";
(* Test 1: Normal case - Expected: [2.0; 2.0; 2.0] *)
Printf.printf "1: %s\n" (String.concat "; " (List.map string_of_float (create 3 2.0)));
(* Test 2: Single element - Expected: [1.0] *)
Printf.printf "2: %s\n" (String.concat "; " (List.map string_of_float (create 1 1.0)));
(* Test 3: Large vector - Expected: 100 elements of 0.0 *)
Printf.printf "3: %d elements\n" (dim (create 100 0.0));
try
  (* Test 4: Zero dimension - Expected: DimensionError *)
  ignore (create 0 1.0);
  Printf.printf "4: Failed\n"
with DimensionError _ -> Printf.printf "4: DimensionError\n";
try
  (* Test 5: Negative dimension - Expected: DimensionError *)
  ignore (create (-1) 1.0);
  Printf.printf "5: Failed\n"
with DimensionError _ -> Printf.printf "5: DimensionError\n";
(* Test 6: Float.infinity - Expected: [inf; inf] *)
Printf.printf "6: %s\n" (String.concat "; " (List.map string_of_float (create 2 Float.infinity)));
(* Test 7: Float.nan - Expected: [nan; nan] *)
Printf.printf "7: %s\n" (String.concat "; " (List.map string_of_float (create 2 Float.nan)));

(* Dim tests *)
Printf.printf "\nDim tests:\n";
(* Test 1: Empty vector - Expected: 0 *)
Printf.printf "1: %d\n" (dim []);
(* Test 2: Single element - Expected: 1 *)
Printf.printf "2: %d\n" (dim [1.0]);
(* Test 3: Normal case - Expected: 3 *)
Printf.printf "3: %d\n" (dim [1.0; 2.0; 3.0]);
(* Test 4: Large vector - Expected: 100 *)
Printf.printf "4: %d\n" (dim (create 100 0.0));
(* Test 5: Vector with inf - Expected: 2 *)
Printf.printf "5: %d\n" (dim [Float.infinity; Float.infinity]);
(* Test 6: Vector with nan - Expected: 2 *)
Printf.printf "6: %d\n" (dim [Float.nan; Float.nan]);
(* Test 7: Mixed values - Expected: 4 *)
Printf.printf "7: %d\n" (dim [1.0; Float.infinity; Float.nan; 0.0]);

(* Is_zero tests *)
Printf.printf "\nIs_zero tests:\n";
(* Test 1: Single zero - Expected: true *)
Printf.printf "2: %b\n" (is_zero [0.0]);
(* Test 2: Multiple zeros - Expected: true *)
Printf.printf "3: %b\n" (is_zero [0.0; 0.0; 0.0]);
(* Test 3: One non-zero - Expected: false *)
Printf.printf "4: %b\n" (is_zero [0.0; 1.0; 0.0]);
(* Test 4: All non-zero - Expected: false *)
Printf.printf "5: %b\n" (is_zero [1.0; 2.0; 3.0]);
(* Test 5: With infinity - Expected: false *)
Printf.printf "6: %b\n" (is_zero [0.0; Float.infinity]);
(* Test 6: With nan - Expected: false *)
Printf.printf "7: %b\n" (is_zero [0.0; Float.nan]);

(* Unit tests *)
Printf.printf "\nUnit tests:\n";
(* Test 1: Normal case - Expected: [0.0; 1.0; 0.0] *)
Printf.printf "1: %s\n" (String.concat "; " (List.map string_of_float (unit 3 2)));
try
  (* Test 2: Index too large - Expected: DimensionError *)
  ignore (unit 3 4);
  Printf.printf "2: Failed\n"
with DimensionError _ -> Printf.printf "2: DimensionError\n";
try
  (* Test 3: Index zero - Expected: DimensionError *)
  ignore (unit 3 0);
  Printf.printf "3: DimensionError\n"
with DimensionError _ -> Printf.printf "3: DimensionError\n";
try
  (* Test 4: Negative index - Expected: DimensionError *)
  ignore (unit 3 (-1));
  Printf.printf "4: DimensionError\n"
with DimensionError _ -> Printf.printf "4: DimensionError\n";
(* Test 5: Single dimension - Expected: [1.0] *)
Printf.printf "5: %s\n" (String.concat "; " (List.map string_of_float (unit 1 1)));
(* Test 6: Large dimension - Expected: 100000 elements, 1.0 at position 50 *)
let v6 = unit 100000 50 in
Printf.printf "6: dim=%d, value at 50=%f\n" (dim v6) (List.nth v6 49);
try
  (* Test 7: Zero dimension - Expected: DimensionError *)
  ignore (unit 0 1);
  Printf.printf "7: Failed\n"
with DimensionError _ -> Printf.printf "7: DimensionError\n";

(* Scale tests *)
Printf.printf "\nScale tests:\n";
(* Test 1: Normal case - Expected: [2.0; 4.0; 6.0] *)
Printf.printf "1: %s\n" (String.concat "; " (List.map string_of_float (scale 2.0 [1.0; 2.0; 3.0])));
(* Test 2: Scale by zero - Expected: [0.0; 0.0] *)
Printf.printf "2: %s\n" (String.concat "; " (List.map string_of_float (scale 0.0 [1.0; 2.0])));
(* Test 3: Unit vector - Expected: [2.0] *)
Printf.printf "3: %s\n" (String.concat "; " (List.map string_of_float (scale 2.0 [1.0])));
(* Test 4: Scale by infinity - Expected: [inf; inf] *)
Printf.printf "4: %s\n" (String.concat "; " (List.map string_of_float (scale Float.infinity [1.0; 2.0])));
(* Test 5: Scale by nan - Expected: [nan; nan] *)
Printf.printf "5: %s\n" (String.concat "; " (List.map string_of_float (scale Float.nan [1.0; 2.0])));
(* Test 6: Scale infinity - Expected: [inf; inf] *)
Printf.printf "6: %s\n" (String.concat "; " (List.map string_of_float (scale 2.0 [Float.infinity; Float.infinity])));
(* Test 7: Negative scale - Expected: [-1.0; -2.0; -3.0] *)
Printf.printf "7: %s\n" (String.concat "; " (List.map string_of_float (scale (-1.0) [1.0; 2.0; 3.0])));

(* Addv tests *)
Printf.printf "\nAddv tests:\n";
(* Test 1: Normal case - Expected: [2.0; 4.0; 6.0] *)
Printf.printf "1: %s\n" (String.concat "; " (List.map string_of_float (addv [1.0; 2.0; 3.0] [1.0; 2.0; 3.0])));
(* Test 2: Empty vectors - Expected: [] *)
Printf.printf "2: %s\n" (String.concat "; " (List.map string_of_float (addv [] [])));
try
  (* Test 3: Different dimensions - Expected: DimensionError *)
  ignore (addv [1.0; 2.0] [1.0; 2.0; 3.0]);
  Printf.printf "3: Failed\n"
with DimensionError _ -> Printf.printf "3: DimensionError\n";
(* Test 4: With infinity - Expected: [inf; inf] *)
Printf.printf "4: %s\n" (String.concat "; " (List.map string_of_float (addv [Float.infinity; 2.0] [1.0; Float.infinity])));
(* Test 5: With nan - Expected: [nan; nan] *)
Printf.printf "5: %s\n" (String.concat "; " (List.map string_of_float (addv [Float.nan; 2.0] [1.0; Float.nan])));
(* Test 6: Zero vectors - Expected: [0.0; 0.0] *)
Printf.printf "6: %s\n" (String.concat "; " (List.map string_of_float (addv [0.0; 0.0] [0.0; 0.0])));
(* Test 7: Negative numbers - Expected: [-2.0; -4.0] *)
Printf.printf "7: %s\n" (String.concat "; " (List.map string_of_float (addv [-1.0; -2.0] [-1.0; -2.0])));

(* Dot_prod tests *)
Printf.printf "\nDot_prod tests:\n";
(* Test 1: Normal case - Expected: 14.0 *)
Printf.printf "1: %f\n" (dot_prod [1.0; 2.0; 3.0] [1.0; 2.0; 3.0]);
(* Test 2: Empty vectors - Expected: 0.0 *)
Printf.printf "2: %f\n" (dot_prod [] []);
try
  (* Test 3: Different dimensions - Expected: DimensionError *)
  ignore (dot_prod [1.0; 2.0] [1.0; 2.0; 3.0]);
  Printf.printf "3: Failed\n"
with DimensionError _ -> Printf.printf "3: DimensionError\n";
(* Test 4: With infinity - Expected: inf *)
Printf.printf "4: %f\n" (dot_prod [Float.infinity; 2.0] [1.0; 2.0]);
(* Test 5: With nan - Expected: nan *)
Printf.printf "5: %f\n" (dot_prod [Float.nan; 2.0] [1.0; 2.0]);
(* Test 6: Zero vectors - Expected: 0.0 *)
Printf.printf "6: %f\n" (dot_prod [0.0; 0.0] [0.0; 0.0]);
(* Test 7: Negative numbers - Expected: 5.0 *)
Printf.printf "7: %f\n" (dot_prod [1.0; -2.0] [-1.0; -2.0]);

(* Length tests *)
Printf.printf "\nLength tests:\n";
(* Test 1: Normal case - Expected: 3.74 *)
Printf.printf "1: %.2f\n" (length [1.0; 2.0; 3.0]);
(* Test 2: Empty vector - Expected: 0.0 *)
Printf.printf "2: %f\n" (length []);
(* Test 3: Unit vector - Expected: 1.0 *)
Printf.printf "3: %f\n" (length [1.0]);
(* Test 4: Zero vector - Expected: 0.0 *)
Printf.printf "4: %f\n" (length [0.0; 0.0; 0.0]);
(* Test 5: With infinity - Expected: inf *)
Printf.printf "5: %f\n" (length [Float.infinity; 2.0]);
(* Test 6: With nan - Expected: nan *)
Printf.printf "6: %f\n" (length [Float.nan; 2.0]);
(* Test 7: Negative numbers - Expected: 5.0 *)
Printf.printf "7: %f\n" (length [3.0; -4.0]);

(* Angle tests *)
Printf.printf "\nAngle tests:\n";
(* Test 1: Same direction - Expected: 0.0 *)
Printf.printf "1: %f\n" (angle [1.0; 0.0] [2.0; 0.0]);
(* Test 2: Perpendicular - Expected: (pi/2) *)
Printf.printf "2: %f\n" (angle [1.0; 0.0] [0.0; 1.0]);
(* Test 3: Opposite direction - Expected: (pi) *)
Printf.printf "3: %f\n" (angle [1.0; 0.0] [-1.0; 0.0]);
try
  (* Test 4: Zero vector - Expected: DimensionError *)
  ignore (angle [0.0; 0.0] [1.0; 1.0]);
  Printf.printf "4: Failed\n"
with DimensionError _ -> Printf.printf "4: DimensionError\n";
try
  (* Test 5: Different dimensions - Expected: DimensionError *)
  ignore (angle [1.0; 0.0] [1.0; 0.0; 0.0]);
  Printf.printf "5: Failed\n"
with DimensionError _ -> Printf.printf "5: DimensionError\n";
(* Test 6: 45 degrees - Expected: (pi/4) *)
Printf.printf "6: %f\n" (angle [1.0; 1.0] [1.0; 0.0]);
(* Test 7: With infinity - Expected: nan *)
Printf.printf "7: %f\n" (angle [Float.infinity; 0.0] [1.0; 0.0])


(* Proofs Of Correctness *)
(* Commutativity: u + v = v + u
    Statement to be proved: addv u v = addv v u
    Proof using induction over vector length:
    They have to be of the same dimenstion, otherwise DimensionError will be thrown
    Base Case : u = [x], v = [y]
        addv u v ==> addv_rec [x] [y] ==> x :: [], y :: [] -> (x +. y) :: addv_rec [] [] ==> [x +. y]
        addv v u ==> addv_rec [y] [x] ==> y :: [], x :: [] -> (y +. x) :: addv_rec [] [] ==> [y +. x]
        Therefore, addv u v = addv v u
    Inductive Hypothesis: addv u v = addv v u
        Given u = [x1; x2; ...; xn], v = [y1; y2; ...; yn]
        By inductive hypothesis, addv u v = addv v u ( Suppose we have proven this for n, to prove for n+1 )
        For u' = x :: u, v' = y :: v
            addv u' v' 
            ==> addv_rec (x :: u) (y :: v) 
            ==> x :: addv_rec u v, y :: addv_rec v u -> x +. y :: addv_rec u v 
            ==> x +. y :: addv_rec u v
            addv v' u'
            ==> addv_rec (y :: v) (x :: u)
            ==> y :: addv_rec v u, x :: addv_rec u v -> y +. x :: addv_rec v u
            ==> y +. x :: addv_rec v u
            Since addv u v = addv v u, x +. y = y +. x
            addv u' v' = addv v' u'
        Hence induction holds
    Therefore, addv u v = addv v u
*)

(* Associavity: u + (v + w) = (u + v) + w
    Statement to be proved: addv u (addv v w) = addv (addv u v) w
    Proof using induction over vector length:
    They have to be of the same dimension, otherwise DimensionError will be thrown
    Base Case: u = [x], v = [y], w = [z]
        addv u (addv v w) 
        ==> addv [x] (addv [y] [z]) 
        ==> addv [x] (y :: [], z :: [] -> (y +. z) :: addv [] [])
        ==> addv [x] [y +. z] 
        ==> [x +. (y +. z)]
        addv (addv u v) w 
        ==> addv (addv [x] [y]) [z] 
        ==> addv (x :: [], y :: [] -> (x +. y) :: addv [] []) [z]
        ==> addv [x +. y] [z] 
        ==> [(x +. y) +. z]
        Since addition of floats is associative, x +. (y +. z) = (x +. y) +. z
        Therefore, addv u (addv v w) = addv (addv u v) w
    Inductive Hypothesis: addv u (addv v w) = addv (addv u v) w
        Given u = [x1; x2; ...; xn], v = [y1; y2; ...; yn], w = [z1; z2; ...; zn]
        By inductive hypothesis, addv u (addv v w) = addv (addv u v) w (Suppose we have proven this for n, to prove for n+1)
        For u' = x :: u, v' = y :: v, w' = z :: w
            addv u' (addv v' w')
            ==> addv (x :: u) (addv (y :: v) (z :: w))
            ==> addv (x :: u) ((y +. z) :: addv v w)
            ==> (x +. (y +. z)) :: addv u (addv v w)
            ==> (x +. (y +. z)) :: addv u (addv v w)  (* by inductive hypothesis *)

            addv (addv u' v') w'
            ==> addv (addv (x :: u) (y :: v)) (z :: w)
            ==> addv ((x +. y) :: addv u v) (z :: w)
            ==> ((x +. y) +. z) :: addv (addv u v) w
            ==> ((x +. y) +. z) :: addv (addv u v) w  (* by inductive hypothesis *)

            Since addition of floats is associative, (x +. (y +. z)) = ((x +. y) +. z)
            Since addv u (addv v w) = addv (addv u v) w:
            addv u' (addv v' w') = addv (addv u' v') w'
        Hence induction holds
    Therefore, addv u (addv v w) = addv (addv u v) w
*)

(* Identity of addition: v + 0 = v 
    Statement to be proved: addv v (create (dim v) 0.0) = v
    Proof using induction over vector length:
    Base Case: v = [x]
        addv [x] (create 1 0.0)
        ==> addv [x] [0.0]
        ==> (x +. 0.0) :: addv [] []
        ==> x :: []
        ==> [x]
        Therefore, addv [x] [0.0] = [x]
    Inductive Hypothesis: addv v (create (dim v) 0.0) = v
        Given v = [x1; x2; ...; xn]
        By inductive hypothesis, addv v (create (dim v) 0.0) = v (Suppose we have proven this for n, to prove for n+1)
        For v' = x :: v
            addv v' (create (dim v') 0.0)
            ==> addv (x :: v) (create (dim (x :: v)) 0.0)
            ==> addv (x :: v) (0.0 :: create (dim v) 0.0)
            ==> (x +. 0.0) :: addv v (create (dim v) 0.0)
            ==> x :: addv v (create (dim v) 0.0)  
            ==> x :: v (* by inductive hypothesis *)
            ==> v'
        Hence induction holds
    Therefore, addv v (create (dim v) 0.0) = v
*)

(* Identity of scalar multiplication: 1 * v = v 
    Statement to be proved: scale 1.0 v = v
    Proof using induction over vector length:
    Base Case: v = [x]
        scale 1.0 [x]
        ==> List.map (fun x -> 1.0 *. x) [x]
        ==> [1.0 *. x] :: List.map (fun x -> 1.0 *. x) []
        ==> x :: []
        ==> [x] (* by definition of List.map and by identity of float multiplication *)
        Therefore, scale 1.0 [x] = [x]
    Inductive Hypothesis: scale 1.0 v = v
        Given v = [x1; x2; ...; xn]
        By inductive hypothesis, scale 1.0 v = v (Suppose we have proven this for n, to prove for n+1)
        For v' = x :: v
            scale 1.0 v'
            ==> scale 1.0 (x :: v)
            ==> List.map (fun x -> 1.0 *. x) (x :: v)
            ==> (1.0 *. x) :: List.map (fun x -> 1.0 *. x) v
            ==> x :: scale 1.0 v
            ==> x :: v (* by inductive hypothesis *)
            ==> v'
        Hence induction holds
    Therefore, scale 1.0 v = v
*)

(* Annihilator scalar: 0 * v = 0
    Statement to be proved: scale 0.0 v = create (dim v) 0.0
    Proof using induction over vector length:
    Base Case: v = [x]
        scale 0.0 [x]
        ==> List.map (fun x -> 0.0 *. x) [x]
        ==> [0.0 *. x] :: List.map (fun x -> 0.0 *. x) []
        ==> 0.0 :: []
        ==> [0.0] (* by definition of List.map and by annihilator property of float multiplication *)
        create (dim [x]) 0.0
        ==> create 1 0.0
        ==> [0.0]
        Therefore, scale 0.0 [x] = create (dim [x]) 0.0
    Inductive Hypothesis: scale 0.0 v = create (dim v) 0.0
        Given v = [x1; x2; ...; xn]
        By inductive hypothesis, scale 0.0 v = create (dim v) 0.0 (Suppose we have proven this for n, to prove for n+1)
        For v' = x :: v
            scale 0.0 v'
            ==> scale 0.0 (x :: v)
            ==> List.map (fun x -> 0.0 *. x) (x :: v)
            ==> (0.0 *. x) :: List.map (fun x -> 0.0 *. x) v
            ==> 0.0 :: scale 0.0 v
            ==> 0.0 :: create (dim v) 0.0 (* by inductive hypothesis *)
            ==> create (dim (x :: v)) 0.0
            ==> create (dim v') 0.0
        Hence induction holds
    Therefore, scale 0.0 v = create (dim v) 0.0
*)

(* Additive Inverse: v + (- v) = 0
    Statement to be proved: addv v (inv v) = create (dim v) 0.0
    Proof using induction over vector length:
    Base Case: v = [x]
        addv [x] (inv [x])
        ==> addv [x] (scale (-1.0) [x])
        ==> addv [x] (List.map (fun x -> (-1.0) *. x) [x])
        ==> addv [x] [(-1.0) *. x]
        ==> (x +. ((-1.0) *. x)) :: addv [] []
        ==> 0.0 :: []
        ==> [0.0]
        create (dim [x]) 0.0
        ==> create 1 0.0
        ==> [0.0]
        Therefore, addv [x] (inv [x]) = create (dim [x]) 0.0
    Inductive Hypothesis: addv v (inv v) = create (dim v) 0.0
        Given v = [x1; x2; ...; xn]
        By inductive hypothesis, addv v (inv v) = create (dim v) 0.0 (Suppose we have proven this for n, to prove for n+1)
        For v' = x :: v
            addv v' (inv v')
            ==> addv (x :: v) (inv (x :: v))
            ==> addv (x :: v) (scale (-1.0) (x :: v))
            ==> addv (x :: v) ((-1.0 *. x) :: scale (-1.0) v)
            ==> (x +. (-1.0 *. x)) :: addv v (inv v)
            ==> 0.0 :: addv v (inv v)
            ==> 0.0 :: create (dim v) 0.0 (* by inductive hypothesis *)
            ==> (0.0 *. x) :: create (dim v) 0.0
            ==> create (dim (x :: v)) 0.0
            ==> create (dim v') 0.0
        Hence induction holds
    Therefore, addv v (inv v) = create (dim v) 0.0
*)

(* Scalar product combination: b.(c.v) = (b.c).v
    Statement to be proved: scale b (scale c v) = scale (b *. c) v
    Proof using induction over vector length:
    Base Case: v = [x]
        scale b (scale c [x])
        ==> scale b (List.map (fun x -> c *. x) [x])
        ==> scale b [c *. x]
        ==> List.map (fun x -> b *. x) [c *. x]
        ==> [b *. (c *. x)]
        ==> [(b *. c) *. x] (* by associativity of float multiplication *)
        ==> List.map (fun x -> (b *. c) *. x) [x]
        ==> scale (b *. c) [x]
        Therefore, scale b (scale c [x]) = scale (b *. c) [x]
    Inductive Hypothesis: scale b (scale c v) = scale (b *. c) v
        Given v = [x1; x2; ...; xn]
        By inductive hypothesis, scale b (scale c v) = scale (b *. c) v (Suppose we have proven this for n, to prove for n+1)
        For v' = x :: v
            scale b (scale c v')
            ==> scale b (scale c (x :: v))
            ==> scale b (List.map (fun x -> c *. x) (x :: v))
            ==> scale b ((c *. x) :: List.map (fun x -> c *. x) v)
            ==> List.map (fun x -> b *. x) ((c *. x) :: List.map (fun x -> c *. x) v)
            ==> (b *. (c *. x)) :: List.map (fun x -> b *. x) (List.map (fun x -> c *. x) v)
            ==> (b *. (c *. x)) :: scale b (scale c v)
            ==> (b *. (c *. x)) :: scale (b *. c) v (* by inductive hypothesis *)
            ==> ((b *. c) *. x) :: scale (b *. c) v (* by associativity of float multiplication *)
            ==> List.map (fun x -> (b *. c) *. x) (x :: v)
            ==> scale (b *. c) (x :: v)
            ==> scale (b *. c) v'
        Hence induction holds
    Therefore, scale b (scale c v) = scale (b *. c) v
*)

(* Scalar sum-product distribution: (b + c).v = b.v + c.v
    Statement to be proved: scale (b +. c) v = addv (scale b v) (scale c v)
    Proof using induction over vector length:
    Base Case: v = [x]
        scale (b +. c) [x]
        ==> List.map (fun x -> (b +. c) *. x) [x]
        ==> [(b +. c) *. x]
        ==> [b *. x +. c *. x] (* by distributive property of float multiplication *)
        ==> addv [b *. x] [c *. x]
        ==> addv (List.map (fun x -> b *. x) [x]) (List.map (fun x -> c *. x) [x])
        ==> addv (scale b [x]) (scale c [x])
        Therefore, scale (b +. c) [x] = addv (scale b [x]) (scale c [x])
    Inductive Hypothesis: scale (b +. c) v = addv (scale b v) (scale c v)
        Given v = [x1; x2; ...; xn]
        By inductive hypothesis, scale (b +. c) v = addv (scale b v) (scale c v) (Suppose we have proven this for n, to prove for n+1)
        For v' = x :: v
            scale (b +. c) v'
            ==> scale (b +. c) (x :: v)
            ==> List.map (fun x -> (b +. c) *. x) (x :: v)
            ==> ((b +. c) *. x) :: List.map (fun x -> (b +. c) *. x) v
            ==> (b *. x +. c *. x) :: List.map (fun x -> (b +. c) *. x) v (* by distributive property of float multiplication *)
            ==> (b *. x +. c *. x) :: scale (b +. c) v
            ==> (b *. x +. c *. x) :: addv (scale b v) (scale c v) (* by inductive hypothesis *)
            ==> addv (b *. x :: scale b v) (c *. x :: scale c v)
            ==> addv (List.map (fun x -> b *. x) (x :: v)) (List.map (fun x -> c *. x) (x :: v))
            ==> addv (scale b (x :: v)) (scale c (x :: v))
            ==> addv (scale b v') (scale c v')
        Hence induction holds
    Therefore, scale (b +. c) v = addv (scale b v) (scale c v)
*)

(* Scalar Distribution over vector sums: b.(u + v) = b.u + b.v
    Statement to be proved: scale b (addv u v) = addv (scale b u) (scale b v)
    Proof using induction over vector length:
    Base Case: u = [x], v = [y]
        scale b (addv [x] [y])
        ==> scale b (addv_rec [x] [y])
        ==> scale b [(x +. y)]
        ==> List.map (fun z -> b *. z) [(x +. y)]
        ==> [b *. (x +. y)]
        ==> [b *. x +. b *. y] (* by distributive property of float multiplication *)
        ==> addv [b *. x] [b *. y]
        ==> addv (List.map (fun z -> b *. z) [x]) (List.map (fun z -> b *. z) [y])
        ==> addv (scale b [x]) (scale b [y])
        Therefore, scale b (addv [x] [y]) = addv (scale b [x]) (scale b [y])
    Inductive Hypothesis: scale b (addv u v) = addv (scale b u) (scale b v)
        Given u = [x1; x2; ...; xn], v = [y1; y2; ...; yn]
        By inductive hypothesis, scale b (addv u v) = addv (scale b u) (scale b v) (Suppose we have proven this for n, to prove for n+1)
        For u' = x :: u, v' = y :: v
            scale b (addv u' v')
            ==> scale b (addv (x :: u) (y :: v))
            ==> scale b ((x +. y) :: addv u v)
            ==> List.map (fun z -> b *. z) ((x +. y) :: addv u v)
            ==> (b *. (x +. y)) :: List.map (fun z -> b *. z) (addv u v)
            ==> (b *. (x +. y)) :: scale b (addv u v)
            ==> (b *. (x +. y)) :: addv (scale b u) (scale b v) (* by inductive hypothesis *)
            ==> (b *. x +. b *. y) :: addv (scale b u) (scale b v) (* by distributive property of float multiplication *)
            ==> addv (b *. x :: scale b u) (b *. y :: scale b v)
            ==> addv (List.map (fun z -> b *. z) (x :: u)) (List.map (fun z -> b *. z) (y :: v))
            ==> addv (scale b (x :: u)) (scale b (y :: v))
            ==> addv (scale b u') (scale b v')
        Hence induction holds
    Therefore, scale b (addv u v) = addv (scale b u) (scale b v)
*)

(* Square of length equals dot product with self: |v|^2 = v.v
    Statement to be proved: length v *. length v = dot_prod v v
    Proof using induction over vector length:
    Base Case: v = [x]
        length [x] *. length [x]
        ==> length_rec 0.0 [x] *. length_rec 0.0 [x]
        ==> sqrt (0.0 +. (x *. x)) *. sqrt (0.0 +. (x *. x))
        ==> sqrt (x *. x) *. sqrt (x *. x)
        ==> x *. x
        dot_prod [x] [x]
        ==> dot_prod_rec 0.0 [x] [x]
        ==> dot_prod_rec (0.0 +. (x *. x)) [] []
        ==> x *. x
        Therefore, length [x] *. length [x] = dot_prod [x] [x]
    Inductive Hypothesis: length v *. length v = dot_prod v v
        Given v = [x1; x2; ...; xn]
        By inductive hypothesis, length v *. length v = dot_prod v v (Suppose we have proven this for n, to prove for n+1)
        For v' = x :: v
            length v' *. length v'
            ==> length_rec 0.0 (x :: v) *. length_rec 0.0 (x :: v)
            ==> sqrt ((x *. x) +. s) *. sqrt ((x *. x) +. s) where s is sum of squares of v, because of inductive hypothesis
            ==> (x *. x) +. s (* by definition of sqrt *)
            
            dot_prod v' v'
            ==> dot_prod_rec 0.0 (x :: v) (x :: v)
            ==> dot_prod_rec ((x *. x) +. 0.0) v v
            ==> (x *. x) +. dot_prod v v
            ==> (x *. x) +. s (* by inductive hypothesis *)
        Hence induction holds
    Therefore, length v *. length v = dot_prod v v
*)

(* Commutativity of dot product: u.v = v.u
    Statement to be proved: dot_prod u v = dot_prod v u
    Proof using induction over vector length:
    They have to be of the same dimension, otherwise DimensionError will be thrown
    Base Case: u = [x], v = [y]
        dot_prod u v
        ==> dot_prod_rec 0.0 [x] [y]
        ==> dot_prod_rec (0.0 +. (x *. y)) [] []
        ==> x *. y
        dot_prod v u
        ==> dot_prod_rec 0.0 [y] [x]
        ==> dot_prod_rec (0.0 +. (y *. x)) [] []
        ==> y *. x
        Since float multiplication is commutative, x *. y = y *. x
        Therefore, dot_prod u v = dot_prod v u
    Inductive Hypothesis: dot_prod u v = dot_prod v u
        Given u = [x1; x2; ...; xn], v = [y1; y2; ...; yn]
        By inductive hypothesis, dot_prod u v = dot_prod v u (Suppose we have proven this for n, to prove for n+1)
        For u' = x :: u, v' = y :: v
            dot_prod u' v'
            ==> dot_prod_rec 0.0 (x :: u) (y :: v)
            ==> dot_prod_rec ((x *. y) +. 0.0) u v
            ==> (x *. y) +. dot_prod u v
            dot_prod v' u'
            ==> dot_prod_rec 0.0 (y :: v) (x :: u)
            ==> dot_prod_rec ((y *. x) +. 0.0) v u
            ==> (y *. x) +. dot_prod v u
            Since float multiplication is commutative, x *. y = y *. x
            Since dot_prod u v = dot_prod v u by inductive hypothesis
            dot_prod u' v' = dot_prod v' u'
        Hence induction holds
    Therefore, dot_prod u v = dot_prod v u
*)

(* If u.v = 0, angle between u and v is pi/2 (orthogonality)
    Statement to be proved: If dot_prod u v = 0.0, then angle u v = pi /. 2.0
    Proof:
    From the angle function:
    angle u v = acos ((dot_prod u v) /. (length u *. length v))

    When dot_prod u v = 0.0:
    angle u v = acos (0.0 /. (length u *. length v))
    ==> angle u v = acos 0.0
    ==> angle u v = pi /. 2.0 

    This proof relies on:
    1. length u and length v cannot be 0 (checked in angle function)
    2. acos 0.0 = pi/2 (mathematical fact)
    3. The angle function correctly computes angles using acos of dot product over product of lengths
       (which is the standard mathematical definition of angle between vectors)

    Therefore, if dot_prod u v = 0.0, then angle u v = pi /. 2.0
*)
