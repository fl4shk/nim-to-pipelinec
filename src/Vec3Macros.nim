import std/macros
import ExtraMacros

macro `doTypedefVec3`*(
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
      `name` = object
        x*: `T`
        y*: `T`
        z*: `T`
    proc `plusName`(
      left: `name`,
      right: `name`,
    ): `name` =
      `name`(
        x: left.x + right.x,
        y: left.y + right.y,
        z: left.z + right.z,
      )
    #template plus(
    #  left: `name`,
    #  right: `name`,
    #): `name` =
    #  left.`plusName` right
    mkBinop(
      LeftT=`name`,
      RightT=`name`,
      opStr="+",
      funcStr=`plusName`,
    )
    proc `minusName`(
      left: `name`,
      right: `name`,
    ): `name` =
      `name`(
        x: left.x - right.x,
        y: left.y - right.y,
        z: left.z - right.z,
      )
    #template minus(
    #  left: `name`,
    #  right: `name`,
    #): `name` =
    #  left.`minusName` right
    mkBinop(
      LeftT=`name`,
      RightT=`name`,
      opStr="-",
      funcStr=`minusName`,
    )
    proc `starName`(
      left: `name`,
      right: `T`,
    ): `name` =
      `name`(
        x: left.x * right,
        y: left.y * right,
        z: left.z * right,
      )
    #template star(
    #  left: `name`,
    #  right: `T`,
    #): `name` =
    #  left.`starName` right
    mkBinop(
      LeftT=`name`,
      RightT=`name`,
      opStr="*",
      funcStr=`starName`,
    )
    proc `slashName`(
      left: `name`,
      right: `T`,
    ): `name` =
      `name`(
        x: left.x div right,
        y: left.y div right,
        z: left.z div right,
      )
    #template slash(
    #  left: `name`,
    #  right: `T`,
    #): `name` =
    #  left.`slashName` right
    mkBinop(
      LeftT=`name`,
      RightT=`name`,
      opStr="/",
      funcStr=`slashName`,
    )
#doTypedefVec3(int)

macro `Vec3`*(
  T: untyped
): untyped =
  let name = ident("Vec3_" & $T)
  result = quote do:
    `name`
