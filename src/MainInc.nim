#proc myMain() =
#  type
#    Asdf = object
#      a*: int
#      v*: Vec3_int
#  var
#    a: Asdf
#    b = Asdf(a: 3)
#  var c: Asdf = Asdf(a: b.a)
#
#  proc doAsdf(
#    asdf: Asdf,
#    b: int,
#  ): Asdf =
#    var temp: Asdf = Asdf(
#      a: asdf.a + b,
#      v: Vec3_int(x:0, y:1, z:2)
#    )
#    result = temp
#
#  var d = doAsdf(asdf=c, b=8)
#  a.a = 9
#  var arr: array[2, array[3, Vec3_int]]
#  for j in 0 ..< arr.len:
#    for i in 0 ..< arr[j].len:
#      arr[j][i].x = arr[j][i].x + 1
