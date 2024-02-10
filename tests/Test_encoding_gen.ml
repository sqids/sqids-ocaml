open QCheck2

let rec list_equal eq l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | [], _ :: _ | _ :: _, [] -> false
  | a1 :: l1, a2 :: l2 -> eq a1 a2 && list_equal eq l1 l2

let sqids = Sqids.make ()

let roundtrip nums =
  list_equal Int.equal (Sqids.decode sqids (Sqids.encode sqids nums)) nums

let test_max_int =
  Test.make ~name:"roundtrip" ~count:1000
    (Gen.small_list (Gen.int_bound max_int))
    roundtrip

let test_small_int =
  Test.make ~name:"roundtrip" ~count:1000
    (Gen.small_list (Gen.small_int_corners ()))
    roundtrip

let () =
  Test.check_exn test_max_int;
  Test.check_exn test_small_int
