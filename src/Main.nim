#import std/macros
include ExtraMacros
import NimToPipelineC
#include Vec3Macros

#dumpTree:
#proc testPipelineC()

#dumpTree:
#proc testPipelineC() = 
type
  Vec3[T] = object
    x*: T
    y*: T
    z*: T
proc plus[T](
  left: Vec3[T],
  right: Vec3[T]
): Vec3[T] =
  result = Vec3[T](
    x: left.x + right.x,
    y: left.y + right.y,
    z: left.z + right.z,
  )
proc `+`(
  left: Vec3,
  right: Vec3,
): Vec3 =
  result = (left.plus right)

type
  Asdf[T] = object
    a*: int
    v*: Vec3[T]
#var
#  a: Asdf
#  b = Asdf(a: 3)
#var c: Asdf = Asdf(a: b.a)
#var e: array[8, int]

proc doAsdf[T, U](
  asdf: Asdf[T],
  b: int,
): Asdf[T] =
  var temp: Asdf[T]
  temp = Asdf[T](
    a: asdf.a + b,
    v: Vec3[T](x:0, y:1, z:2)
  )
  var tempVec3: Vec3[int]
  #var g: Vec3(int)
  #const
  #  eSize2dX = 8
  #  eSize2dY = 3
  #var e: array[3, array[8, Asdf[T]]]
  ##var e: array[3, array[8, Vec3[int]]]
  #if e[0][0].v.x == 0:
  #  e[1][1].a = 9
  #elif e[0][0].v.y == 0:
  #  e[0][0].v.z = 8
  #else:
  #  e[2][2].a = 7
  #var f: array[3, array[8, Vec3[int]]]
  #result = e[temp.a][(temp.a) + 1]
  result = temp

#doTypedefVec3(int)

proc myMain[T](): Asdf[T] =
  var a: Asdf[T]
  var c: Asdf[T]
  var b: Asdf[T] = doAsdf[T, float](
    asdf=c,
    b=8
  )
  #if b.v.x == b.v.y:
  b = doAsdf[T, float](a, 9)
  #var d = doAsdf(asdf=c, b=8)
  #a.a = 9
  #var arr: array[2, array[3, Vec3(T)]]
  #for j in 0 ..< arr.len:
  #  for i in 0 ..< arr[j].len:
  #    arr[j][i].x = arr[j][i].x + 1
  #type
  #  TriVert = object
  #    v0: Vec3(T)
  #    v1: Vec3(T)
  #    v2: Vec3(T)
  #var a: TriVert
  result = b
#myMain()
proc myOuterMain(): int =
  result = myMain[int]().a
proc myOuterOuterMain(): int =
  result = myOuterMain()
#let temp = 
echo toPipelineC(bindSym("myOuterOuterMain"))


#var a = Vec3(int)(x:9, y: 8, z: 7)
#echo a
#mkMyMain(Vec3(int))
#echo myMain_int(3)

#dumpTree(main())
#main()
#echo toPipelineC(myMain_Vec3_int)
#echo toPipelineC(myMain)
