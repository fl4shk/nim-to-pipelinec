import std/macros, NimToPipelineC

#dumpTree:
#block:
#  proc myMain[T](
#    i: T,
#  ): T =
#    #dumpTree:
#    #block:
#    type Vec3[T] = object
#      x*: T
#      y*: T
#      z*: T
#
#    type TriVert[T] = object
#      #v: array[3, Vec3[T]]
#      v0, v1: Vec3[T]
#      v2: Vec3[T]
#
#    var
#      a = Vec3[int](x:8, y:3, z:9)
#      b: int
#      d: Vec3[int]
#    #var (c, d) = (3, 9)
#    var c: int = 8
#    a.x = 9 + 3
#    a = Vec3[int](x:1, y:9, z:4)
#
#    #var
#    #  asdf: int = 3
#    var
#      arr: array[8, Vec3[int]]
#    for i in 0 ..< arr.len:
#      arr[i].echo
#      #let asdf = echo(arr[i])
#    #var a: T
#    #a = i + 3
#    #return a
#    return b
#  proc main(): int =
#    var temp = 3
#    result = temp.myMain
#  echo toPipelineC(main)

macro doTypedefVec3(
  T: untyped
): untyped =
  let name = ident("Vec3_" & $T)
  let plusName = ident(name.strVal & "_plus")
  #let plusOpName = ident("`+`")
  let minusName = ident(name.strVal & "_minus")
  #let minusOpName = ident("system.-")
  let starName = ident(name.strVal & "_star")
  #let starOpName = ident("system.*")
  let slashName = ident(name.strVal & "_slash")
  #let slashOpName = ident("system./")
  #result = newStmtList()
  result = quote do:
    type
      `name`* = object
        x*: `T`
        y*: `T`
        z*: `T`
    proc `plusName`*(
      left: `name`,
      right: `name`,
    ): `name` =
      `name`(
        x: left.x + right.x,
        y: left.y + right.y,
        z: left.z + right.z,
      )
    template plus*(
      left: `name`,
      right: `name`,
    ): `name` =
      left.`plusName` right
    proc `minusName`*(
      left: `name`,
      right: `name`,
    ): `name` =
      `name`(
        x: left.x - right.x,
        y: left.y - right.y,
        z: left.z - right.z,
      )
    template minus*(
      left: `name`,
      right: `name`,
    ): `name` =
      left.`minusName` right
    proc `starName`*(
      left: `name`,
      right: `T`,
    ): `name` =
      `name`(
        x: left.x * right,
        y: left.y * right,
        z: left.z * right,
      )
    template star*(
      left: `name`,
      right: `T`,
    ): `name` =
      left.`starName` right
    proc `slashName`*(
      left: `name`,
      right: `T`,
    ): `name` =
      `name`(
        x: left.x div right,
        y: left.y div right,
        z: left.z div right,
      )
    template slash*(
      left: `name`,
      right: `T`,
    ): `name` =
      left.`slashName` right

doTypedefVec3(int)

#macro mkMyMain(
#  T: untyped
#): untyped =
#  let name = ident("myMain_" & $T)
#  result = newStmtList()
#  result.add quote do:
#    proc `name`(
#      i: `T`
#    ): `T` =
#      result = i
#macro doMkVec3ExtraFuncs(
#  T: untyped
#): untyped = 
#  let name = ident("Vec3_" & $T)
#  let plusName = ident(name.strVal & "_plus")
#  let minusName = ident(name.strVal & "_minus")
#  let starName = ident(name.strVal & "_star")
#  let slashName = ident(name.strVal & "_slash")
#  template `+`*(
#    left: name,
#    right: name,
#  ): name =
#    left.plusName` right
#  template `-`*(
#    left: `name`,
#    right: `name`,
#  ): `name` =
#    left.`minusName` right
#
#  template `*`*(
#    left: Vec3_int,
#    right: int,
#  ): Vec3_int =
#    left.`starName` right
#  template `/`*(
#    left: Vec3_int,
#    right: int,
#  ): Vec3_int =
#    left.`slashName` right
#
#doMkVec3ExtraFuncs(int)

#proc myMain(): Vec3_int =
#dumpTree:
#  var a = Vec3_int(x:8, y:9, z: 10)
#  var b = Vec3_int(x:8, y:9, z: 10)
#  var c: Vec3_int
#  #c = Vec3_int_plus(a, b)#a + b #Vec3_int_plus(a, b)
#  #c = a.Vec3_int_plus(b)
#  c = a.plus b
#  #c = c * 3
#  plus a, b
#  echo c
#  c
#echo toPipelineC(myMain)

#dumpAstGen:
#dumpTree:
proc testPipelineC() =
  type
    Asdf = object
      a*: int
      v*: Vec3_int
  var
    a: Asdf
    b = Asdf(a: 3)
  var c: Asdf = Asdf(a: b.a)

  proc doAsdf(
    asdf: Asdf,
    b: int,
  ): Asdf =
    return Asdf(
      a: asdf.a + b,
      v: Vec3_int(x:0, y:1, z:2)
    )

  var d = doAsdf(asdf=c, b=8)
  a.a = 9
  var arr: array[2, array[3, Vec3_int]]
  for j in 0 ..< arr.len:
    for i in 0 ..< arr[j].len:
      arr[j][i].x = arr[j][i].x + 1

echo toPipelineC(testPipelineC)


#var a = Vec3_int(x:9, y: 8, z: 7)
#echo a
#mkMyMain(Vec3_int)
#echo myMain_int(3)

#dumpTree(main())
#main()
#echo toPipelineC(myMain_Vec3_int)
#echo toPipelineC(myMain)
