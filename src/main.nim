include extraMacros
import nimToPipelineC

macro `MyVec`*(
  I: untyped,
  T: untyped,
): untyped =
  result = quote do:
    array[`I`, `T`]

type
  Vec3[T] = object
    #x*, y*, z*: T
    v: MyVec(3, T)

template `[]`*(
  self: Vec3,
  idx: untyped
): untyped =
  self.v[idx]

template `[]=`*(
  self: Vec3,
  idx: untyped,
  val: untyped
): untyped =
  self.v[idx] = val

template `x`*(
  self: Vec3
): untyped =
  self.v[0]
template `y`*(
  self: Vec3
): untyped =
  self.v[1]
template `z`*(
  self: Vec3
): untyped =
  self.v[2]

template `x=`*(
  self: Vec3,
  val: untyped
): untyped =
  self.v[0] = val
template `y=`*(
  self: Vec3,
  val: untyped
): untyped =
  self.v[1] = val
template `z=`*(
  self: Vec3,
  val: untyped
): untyped =
  self.v[2] = val

proc mkVec3[T](
  x, y, z: T,
): Vec3[T] =
  var temp: Vec3[T] = Vec3[T](v: [x, y, z])
  ##result = Vec3[T](v: [x, y, z])
  #temp.x = x
  #temp.y = y
  #temp.z = z
  result = temp
#template mkVec3(
#  T: untyped,
#  x, y, z: untyped,
#): Vec3 =
#  Vec3[T](v: [x, y, z])

proc `plus`*[T](
  left: Vec3[T],
  right: Vec3[T],
): Vec3[T] =
  # example 1:
  var i: int = 0
  #for i in 0 ..< result.v.len():
  while i < result.v.len():
    result[i] = left[i] + right[i]
    i = i + 1

  ## example 2:
  #result.x = left.x + right.x
  #result.y = left.y + right.y
  #result.z = left.z + right.z

template `+`*(
  left: Vec3,
  right: Vec3,
): Vec3 =
  left.plus right

proc `minus`*[T](
  left: Vec3[T],
  right: Vec3[T],
): Vec3[T] =
  # example 1:
  #var i: int = 0
  for i in 0 ..< result.v.len():
  #while i < result.v.len():
    if (
      (
        i > 0
      ) and (
        i < 3
      )
    ):
      result[i] = left[i] - right[i]
  #  i = i + 1

  ## example 2:
  #result.x = left.x + right.x
  #result.y = left.y + right.y
  #result.z = left.z + right.z

template `-`*(
  left: Vec3,
  right: Vec3,
): Vec3 =
  left.minus right

#proc doVec3IAdd(
#  a: Vec3[int],
#  b: Vec3[int],
#): Vec3[int] {.importc: "doVec3IAdd".}


type
  AluOpKind* = enum
    aokAdd,
    aokSub,
    #aokFakeAnd,

type
  AluInp*[T] = object
    a*: T
    b*: T
    op*: AluOpKind

type
  AluOutp*[T] = object
    ret*: T

#dumpTree:
#  proc `alu`*[T](
#    inp: AluInp[T]
#  ): AluOutp[T] {.cstatic,cnomangle.} =
#    if inp.op == aokAdd:
#      result.ret = inp.a + inp.b
#    else:
#      result.ret = inp.a - inp.b
proc `alu`*[T](
  inp {.cconst.}: AluInp[T]
): AluOutp[T] =
  #if inp.op == aokAdd:
  #  result.ret = inp.a + inp.b
  #else:
  #  result.ret = inp.a - inp.b
  case inp.op:
  of aokAdd:#, aokSub:#, aokFakeAnd:
    result.ret = inp.a + inp.b
    #result.ret = result.ret + inp.a
  else:
    result.ret = inp.a - inp.b

#proc `myVec3IntAlu`*(
#  inp: AluInp[Vec3[int]]
#): AluOutp[Vec3[int]] {.cextern,cnomangle.} =
#  discard

#proc doVec3IAdd(
#  a: Vec3[int],
#  b: Vec3[int],
#): Vec3[int] =
#  result = a + b

#macro myPragmaStr(): untyped =
#  result = quote do:
#    "#pragma MAIN_MHZ myMain 100.0"
#macro myMainPragmas(
#  ResultT: untyped
#): untyped =
#  let tempStr = "300.0"
#  result = quote do:

# The FPGA of the Arty A7 100T
macro part(): untyped =
  result = quote do:
    "#pragma PART \"xc7a100tcsg324-1\"\n" 

proc myMain(
  #a: Vec3[int],
  #b: Vec3[int],
  #op: AluOpKind,
  inp: AluInp[Vec3[int]]
): AluOutp[Vec3[int]] {.cnomangle,craw: part,cmainmhz: "300.0".} =
  #{.craw: myPragmaStr.}
  #let a: Vec3[int] = mkVec3[int](x=1, y=2, z=3)
  #let b: Vec3[int] = mkVec3[int](x=7, y=9, z=2)
  #result = doVec3IAdd(a=a, b=b)

  #type
  #  MyArray[T] = object
  #    arr: array[8, T]
  #var tempA: MyArray[Vec3[int]]

  result = alu(inp)
  #result = myVec3IntAlu(inp=inp)

#proc myMain(
#  a: Vec3[int],
#  b: Vec3[int],
#): Vec3[int] {.craw: part,cmainmhz: "300.0",cstatic.} =
#  #{.craw: myPragmaStr.}
#  #let a: Vec3[int] = mkVec3[int](x=1, y=2, z=3)
#  #let b: Vec3[int] = mkVec3[int](x=7, y=9, z=2)
#  result = doVec3IAdd(a=a, b=b)

#proc myTreeReprInner(
#  obj: NimNode
#): string =
#  result = obj.getTypeImpl().treeRepr()
#
#macro myTreeRepr(
#  obj: typed
#): untyped =
#  #result = quote do:
#  #  obj.treeRepr()
#  newLit(myTreeReprInner(obj))

proc myOuterMain(
  a: Vec3[int],
  b: Vec3[int],
  op: AluOpKind,
): AluOutp[Vec3[int]] =
  #result = myMain(a=a, b=b, op=op)
  #echo myTreeRepr(a)
  let tempA = mkVec3[int](a.x, a.y, a.z)
  var aluInp = AluInp[Vec3[int]](
    #a: a[],
    a: tempA,
    b: b,
    op: op,
  )
  result = myMain(aluInp)

#let a: Vec3[int] = mkVec3(x=1, y=2, z=3)
#let b: Vec3[int] = mkVec3(x=7, y=9, z=2)
#let op: AluOpKind = aokAdd
proc myOuterOuterMain(): AluOutp[Vec3[int]] =
  var a = mkVec3(x=1, y=2, z=3)
  let b = mkVec3[int](x=7, y=9, z=2)
  let op = aokAdd
  result = myOuterMain(
    a=(a),
    b=b,
    op=op
  )
proc myOuterOuterOuterMain(): AluOutp[Vec3[int]] =
  result = myOuterOuterMain(
    #a=a,
    #b=b,
    #op=op
  )
#echo toPipelineC(myOuterMain)
#echo toPipelineC(myOuterOuterMain)

echo toPipelineC(
  myOuterOuterOuterMain,
  regularC=false,
  cppConstRefInp=true,
)
#echo myOuterOuterOuterMain()

#let a = 3
#case a:
#else:
#  echo a
#
#type
#  Vec2I = object
#    x*: int
#    y*: int
#
#type
#  Asdf[T] = object
#    a*: int
#    v*: Vec3[T]
#    b*: array[2, int]
#    vi: Vec2I
#
#proc doAsdf[T, U](
#  asdf: Asdf[T],
#  b: int,
#): Asdf[T] =
#  var temp {.cstatic.}: Asdf[T]
#  temp = Asdf[T](
#    a: asdf.a + b,
#    v: Vec3[T](x:0, y:1, z:2),
#    #b: [1, 2]
#  )
#  #var tempVec3: Vec3[T]
#  #var tempVec3b: Vec3[T] = tempVec3.plus temp.v
#  #var tempVec3c: Vec3[Asdf[T]]
#  #var g: Vec3(int)
#  #const
#  #  eSize2dX = 8
#  #  eSize2dY = 3
#  var e: array[3, array[8, Vec3[Asdf[T]]]]
#  ##var e: array[3, array[8, Vec3[int]]]
#  #if e[0][0].x.v.x == 0:
#  #  e[1][1].x.a = 9
#  #  if e[1][1].y.a == 7:
#  #    e[0][0].x.v.z = 200
#  #elif e[0][0].x.v.y == 0:
#  #  e[0][0].x.v.z = 8
#  #else:
#  #  e[2][2].x.a = 7
#  for j in 0 ..< e.len:
#    for i in 0 ..< e[j].len:
#      e[j][i].x.v.x = e[j][i].x.v.x + 1
#  #var f: array[3, array[8, Vec3[int]]]
#  #var f: array[3, array[8, int]]
#  result = e[temp.a][(temp.a) + 1].x
#  #result = temp
#
##doTypedefVec3(int)
#type
#  MyArray[T] = object
#    #e: array[2, Vec3[Asdf[T]]]
#    e: array[2, T]
#
#proc myMain[T](
#  #e: array[2, Vec3[Asdf[T]]]
#  e: MyArray[T]
#): Asdf[T] =
#  var a {.cstatic.}: Asdf[T]
#  var c: Asdf[T]
#  var b: Asdf[T] = doAsdf[T, float](
#    asdf=c,
#    b=8
#  )
#  #if b.v.x == b.v.y:
#  b = doAsdf[T, float](a, 9)
#  #var d = doAsdf(asdf=c, b=8)
#  #a.a = 9
#  var arr: array[2, array[3, Vec3[T]]]
#  for j in 0 ..< arr.len:
#    for i in 0 ..< arr[j].len:
#      arr[j][i].x = arr[j][i].x + 1
#  #type
#  #  TriVert = object
#  #    v0: Vec3(T)
#  #    v1: Vec3(T)
#  #    v2: Vec3(T)
#  #var a: TriVert
#  result = b
##myMain()
##proc myMain2[T](
##  a, c: Vec3[Asdf[T]],
##  b: Asdf[int16],
##): int =
##  result = 3
#proc myOuterMain(): int =
#  #var e: Asdf[int]
#  #var e: array[2, Vec3[Asdf[int]]]
#  var e: MyArray[int] = MyArray[int](
#    e: [1, 2]
#  )
#  var t: array[8, Asdf[float]]
#  result = myMain[int](
#    e=e
#  ).a
#  #var a: Vec3[Asdf[int8]]
#  #var b: Asdf[int16]
#  #result = myMain2(
#  #  a=a,
#  #  c=a,
#  #  b=b,
#  #)
#proc myOuterOuterMain(): int =
#  result = myOuterMain()
##let temp = 
##echo toPipelineC(bindSym("myOuterOuterMain"))
##echo toPipelineC(myOuterOuterMain)
##var temp = 
#echo toPipelineC(myOuterOuterMain)
##echo temp
#
#
##var a = Vec3(int)(x:9, y: 8, z: 7)
##echo a
##mkMyMain(Vec3(int))
##echo myMain_int(3)
#
##dumpTree(main())
##main()
##echo toPipelineC(myMain_Vec3_int)
##echo toPipelineC(myMain)
