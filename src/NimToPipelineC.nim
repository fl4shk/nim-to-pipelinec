import std/macros
import strutils
import std/tables#, src/fusion/matching
import Borrowed

{.experimental: "caseStmtMacros".}

##macro cdecl*[T](
##  name: untyped,
##): untyped = 
##  result = newStmtList()
#
#  #result = newStmtList()
#  #for i in 0..<10:
#  #  let name = ident("myProc" & $i)
#  #  let content = newLit("I am procedure number #" & $i)
#
#  #  result.add quote do:
#  #    proc `name`() =
#  #      echo `content`
#
#template cif*(
#  expr: untyped,
#  stmtList: untyped
#): untyped =
#  let ret {.nodecl.} = expr
#  {.emit:"if (".}
#  {.emit:"ret".}
#  {.emit:") {\n".}
#  stmtList
#  {.emit:"}\n".}
#
##macro cif*(
##  expr: untyped,
##  stmtList: untyped,
##): untyped =
##  stmtList.expectKind nnkStmtList
##  let retName = ident("ret")
##  result = quote do:
##    {.emit:"if (".}
##    #cexpr(expr=`expr`)
##    #let temp {.nodecl.}: typeof(`expr`) = `expr`
##    #discard temp
##    #let `ret` = `expr`
##    let `retName` {.importc,nodecl.} = `expr`
##    #cexpr(expr=`expr`)
##    {.emit:") {\n".}
##    cstmtlist(stmtList=`stmtList`)
##    {.emit:"}\n".}
#
#macro celif*(
#  expr: untyped,
#  stmtList: untyped,
#): untyped =
#  result = quote do:
#    {.emit:"else if (".}
#    cexpr(expr=`expr`)
#    {.emit:") {\n".}
#    cstmtlist(stmtList=`stmtList`)
#    {.emit:"}\n".}
#
#macro celse*(
#  stmtList: untyped
#): untyped = 
#  result = quote do:
#    {.emit:"else {".}
#    cstmtlist(stmtList=`stmtList`)
#    {.emit:"}\n".}
#
#macro cstmtlist*(
#  stmtList: untyped
#): untyped =
#  stmtList.expectKind nnkStmtList
#
##macro cexpr*(
##  expr: untyped,
##): untyped =
##  result = quote do:
##    {.emit:"" & $`expr`.}
#template cexpr*(
#  expr: untyped
#): bool =
#  let temp {.nodecl.}: typeof(expr) = expr
#  temp
#
#
##macro `eq`*(
##  left: untyped,
##  right: untyped
##): untyped =
##  discard
#
##cif(3):
##  let a = 3
##  let b = 8

#type
#  Rename = object
#    typeName: string
#    typeImpl: NimNode
#macro asType(
#  T: typedesc
#): untyped =
#  T

type
  Convert = object
    #procRenameTbl: seq[Table[string, string]]
    #objRenameTbl: seq[Table[string, string]]
    #renameLevel: int = 0
    funcTbl: Table[string, string]
    funcSeq: seq[string]
    typedefTbl: Table[string, seq[string]]
    typedefSeq: seq[string]
    res: string
    useResult: bool
    hadArray: bool

macro fail(): untyped =
  result = quote do:
    assert(
      false,
      (
        $n.kind & " " & repr(n) & " disallowed (maybe just for now?): "
      ) & (
        n.treeRepr
      )
    )
#macro fail(
#  n: untyped
#): untyped =
#  result = quote do:
#    fail()

proc have(
  n: NimNode,
  kinds: seq[NimNodeKind],
  start: int=0
): bool =
  result = true
  for i in 0 ..< kinds.len:
    if n[i + start].kind != kinds[i]:
      result = false

proc toCodeExprInner(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int,
  isLhs: static bool,
  isTypeInst: bool=false,
  isSingle: bool=true,
  typeImpl: NimNode=nil,
  inHaveArray: bool=false,
  #stickyInHaveArray: bool=false
  #varName: ref string=nil,
  arrayPass: int=(-1)
  #hadArray: ref bool=nil,
): string

#proc nextProcRenameTbl(
#  self: var Convert,
#  n: NimNode,
#) =
#  #self.procRenameTbl.clear()
#  echo "nextProcRenameTbl():"
#  echo n.treeRepr
#  if n.kind == nnkProcTy:
#    if n[0].kind == nnkFormalParams:
#      for paramDef in n[0]:
#        echo paramDef.treeRepr
#        if paramDef.kind == nnkBracketExpr:
#          #echo paramDef.
#          #echo paramDef.treeRepr
#          discard
#        #elif paramDef.kind == nnkIdentDefs:
#        #  discard
#        #elif paramDef.kind == 
#        else:
#          #let n = paramDef
#          #fail()
#    else:
#      #n[0].
#      fail()
#  #else:
#  #  fail()

  

proc toCodeExpr(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int,
  isLhs: static bool,
  isTypeInst: bool=false,
)
proc toCodeStmts(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int
)

proc toCodeIfStmt(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0
) =
  var first: bool = true
  for n in nodes:
    case n.kind:
    of nnkEmpty:
      discard
    of nnkElifBranch:
      addIndent(self.res, level)
      if not first:
        self.res.add "else "
      else:
        first = false
      self.res.add "if ("
      self.toCodeExpr(n[0], level, false)
      self.res.add ") {\n"

      addIndent(self.res, level)
      self.toCodeStmts(n[1], level)
      addIndent(self.res, level)
      self.res.add "}\n"
    of nnkElse:
      addIndent(self.res, level)
      self.res.add "else {\n"
      self.toCodeStmts(n[0], level)
      addIndent(self.res, level)
      self.res.add "}\n"
    else:
      fail()

proc typeRenameInner(
  s: string
): (bool, string) =
  case s:
  of "int":
    result = (true, "int32_t")
  of "uint":
    result = (true, "uint32_t")
  of "int8":
    result = (true, "int8_t")
  of "uint8":
    result = (true, "uint8_t")
  of "int16":
    result = (true, "int16_t")
  of "uint16":
    result = (true, "uint16_t")
  of "int32":
    result = (true, "int32_t")
  of "uint32":
    result = (true, "uint32_t")
  of "int64":
    result = (true, "int64_t")
  of "uint64":
    result = (true, "uint64_t")
  of "float32", "float":
    result = (true, "float")
  else:
    result = (false, s)
proc typeRename(
  s: string
): string =
  typeRenameInner(s)[1]


proc funcRenameIter(
  self: var Convert,
  paramType: NimNode,
  procName: var string,
  first: var bool,
): string =
  #if paramType.kind == nnkVarTy:
  #  #echo "test"
  #  fail()
  ##elif paramType.kind == nnkBracketExpr:
  ##  #fail()
  #else:
  #paramsStr.add ""
  #paramsStr.add paramType.strVal
  result = self.toCodeExprInner(
    nodes=paramType,
    level=0,
    isLhs=false,
    isTypeInst=true,
    isSingle=true,
    #typeImpl=paramType,
  )
  #if first:
  #  first = false
  #  procName.add "_f"
  #  #procName.add "_"
  #else:
  #  #discard
  procName.add "_c"
    #procName.add "_"
  procName.add result
  #echo "funcRenameIter: " & procName
  #paramsStr.add tempParamsStr
  #result = tempParamsStr


proc toCodeExprInner(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int,
  isLhs: static bool,
  isTypeInst: bool=false,
  isSingle: bool=true,
  typeImpl: NimNode=nil,
  inHaveArray: bool=false,
  #stickyInHaveArray: bool=false
  #varName: ref string=nil,
  arrayPass: int=(-1),
  #hadArray: ref bool=nil,
): string =
  var n = nodes
  #echo n.kind
  self.hadArray = false
  if isTypeInst:
    proc innerHandleIdentSym(
      someN: NimNode,
      someTypeImpl: NimNode=nil,
    ): string =
      var tempStr: (bool, string) = typeRenameInner(someN.strVal)
      if someN.kind == nnkIdent:
        if someTypeImpl != nil:
          #echo "have nnkIdent with typeImpl:"
          #echo tempStr[1]
          #echo someTypeImpl.treeRepr
          #tempStr[1] = someType
          discard
        #echo someN.getType()
        #echo someN.astGenRepr()
        #echo someN.getImpl()
        #echo someN.getTypeInst()
      #result.add someN.strVal
      if not tempStr[0]:
        result.add tempStr[1].replace("_", "_a")
        #result.add tempStr[1]#.replace("_", "_a")
      else:
        result.add tempStr[1]
      #if not isSingle:
      result.add "_c"
        #result.add "_"

    #echo "isTypeInst: " & repr(n)
    case n.kind:
    of nnkIdent, nnkSym:
      #echo "test:"
      #echo n.treeRepr
      result.add innerHandleIdentSym(
        someN=n,
        someTypeImpl=typeImpl,
      )
    #of nnkSym:
    #  result.add n.strVal
    of nnkBracketExpr:
      echo "typeinst nnkBracketExpr: "
      echo n.treeRepr
      let innerTypeImpl = n.getTypeImpl()
      echo innerTypeImpl.treeRepr
      let innerTypeInst = n.getTypeInst()
      echo innerTypeInst.treeRepr
      proc getHaveArray(
        someN: NimNode
      ): bool =
        echo "getHaveArray: begin"
        var haveArray: bool = (
          have(someN, @[nnkSym, nnkInfix])
        )
        if haveArray:
          echo "getHaveArray: first"
          haveArray = (
            someN[0].strVal == "array"
          )
        if haveArray:
          echo "getHaveArray: second"
          haveArray = (
            someN[1].len == 3
          )
        if haveArray:
          echo "getHaveArray: third"
          haveArray = (
            have(someN[1], @[nnkIdent, nnkIntLit, nnkIntLit])
          )
        if haveArray:
          echo "getHaveArray: fourth"
          haveArray = (
            someN[1][0].strVal == ".."
          )
        result = haveArray
      let tempHadArray = getHaveArray(someN=n)
      echo "tempHadArray 0: ", tempHadArray
      self.hadArray = tempHadArray
      #if hadArray != nil:
      #  #echo "hadArray:" & $hadArray[]
      #  hadArray[] = tempHadArray
      #  echo "tempHadArray: ", tempHadArray
      if tempHadArray:
        if inHaveArray:
          fail()
        echo "getHaveArray(someN=n) == true:"
        proc handleArray(
          self: var Convert,
          nodes: NimNode,
          ret: var seq[string],
          level: int,
        ) =
          let n = nodes
          proc doLastIter(
            self: var Convert,
            nodes: NimNode,
            ret: var seq[string],
            level: int,
          ) =
            #if arrayPass == 0:
            #  echo "doLastIter():"
            #  echo nodes.treeRepr
            #  echo "--------"
            ret.add self.toCodeExprInner(
              nodes=nodes,
              level=level,
              isLhs=isLhs,
              isTypeInst=isTypeInst,
              isSingle=isSingle,
              typeImpl=nil, # we already have the implementation I think?
              inHaveArray=true,
            )

          case n.kind:
          of nnkBracketExpr:
            if getHaveArray(someN=n):
              #if arrayPass == 0:
              #  echo "nnkBracketExpr getHaveArray:"
              ret.add $(n[1][2].intVal - n[1][1].intVal + 1)
              self.handleArray(
                nodes=n[2],
                ret=ret,
                level=level,
              )
            else:
              #if arrayPass == 0:
              #  echo "nnkBracketExpr not getHaveArray:"
              #  echo n.treeRepr
              #  echo "--------"
              self.doLastIter(
                nodes=n,
                ret=ret,
                level=level,
              )
          else:
            #if arrayPass == 0:
            #  echo "not nnkBracketExpr getHaveArray:"
            #  echo n.treeRepr
            #  echo "--------"
            self.doLastIter(
              nodes=n,
              ret=ret,
              level=level,
            )

        var mySeq: seq[string]
        self.handleArray(
          nodes=n,
          ret=mySeq,
          level=level
        )
        if mySeq.len > 1:
          if arrayPass == 0:
            echo "arrayPass == 0:"
            echo mySeq
            result.add mySeq[^1]
            #result.add " "
            #result.add self.toCodeExprInner(
            #  nodes=n[0],
            #  level=level,
            #  isLhs=false,
            #  isTypeInst=false,
            #  inHaveArray=true,
            #)
            self.hadArray = true
          elif arrayPass == 1:
            for i in 0 ..< mySeq.len - 1:
              result.add "["
              result.add mySeq[i]
              result.add "]"
          else:
            fail()
        else:
          fail()
      else: # if not haveArray
        #echo "not getHaveArray(someN=n):"
        case n[0].kind:
        of nnkIdent, nnkSym:
          if typeImpl != nil:
            echo "n[0].kind == nnkIdent, nnkSym"
            echo typeImpl[0].treeRepr
            result.add innerHandleIdentSym(
              someN=n[0],
              someTypeImpl=typeImpl[0],
            )
          else:
            result.add innerHandleIdentSym(
              someN=n[0],
              someTypeImpl=nil
            )
        else:
          result.add self.toCodeExprInner(
            n[0], level, isLhs, true, false, typeImpl
          )
        result.add "_b"
        case n[1].kind:
        of nnkIdent, nnkSym:
          if typeImpl != nil:
            echo "n[1].kind == nnkIdent, nnkSym"
            echo typeImpl[1].treeRepr
            result.add innerHandleIdentSym(
              someN=n[1],
              someTypeImpl=typeImpl[1],
            )
          else:
            result.add innerHandleIdentSym(
              someN=n[1],
              someTypeImpl=nil
            )
        else:
          result.add self.toCodeExprInner(
            n[1], level, isLhs, true, false, typeImpl
          )
        #result.add "_d"
    of nnkUIntLit, nnkUInt8Lit, nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit:
      #result.add("((int)" & $n.intVal & ")")
      # (old concept for the transpiler, couldn't get it to work!):
      # only allow unsigned integer literals
      result.add($n.intVal)
    of nnkIntLit, nnkInt8Lit, nnkInt16Lit, nnkInt32Lit, nnkInt64Lit:
      let tempIntVal = n.intVal
      if tempIntVal < 0:
        fail()
      else:
        result.add($tempIntVal)
    of nnkCall:
      #echo "#--------"
      #echo "typeinst nnkCall: "
      #echo n.treeRepr
      let innerTypeImpl = n.getTypeImpl()
      let innerTypeInst = n.getTypeInst()
      #echo ""
      #echo innerTypeImpl.treeRepr
      #echo ""
      #echo innerTypeInst.treeRepr
      #echo "--------"
      if n.len >= 3:
        if n[0].kind == nnkOpenSymChoice:
          #echo "n[0].kind == nnkOpenSymChoice: first:"
          result.add self.toCodeExprInner(
            innerTypeInst, level, isLhs, isTypeInst, isSingle,
            innerTypeImpl, arrayPass=arrayPass
          )
          #result.add "_b"
          #var idx = 2
          #if idx < innerTypeImpl[2 .. ^1].len():
          #  for param in innerTypeImpl[2 .. ^1]:
          #    echo param.treeRepr
          #    result.add self.toCodeExprInner(
          #      param, level, isLhs, isTypeInst, isSingle,
          #      innerTypeImpl[idx]
          #    )
          #    result.add "_d"
          #    idx += 1
        else:
          #echo n.repr
          #echo n.treeRepr
          fail()
      else:
        #echo "typeinst nnkCall other len: "
        #echo n.repr
        #echo n.treeRepr
        fail()
    of nnkObjectTy:
      echo "typeinst have nnkObjectTy:"
      echo n.treeRepr
      #discard
      fail()
    of nnkEmpty:
      discard
    else:
      echo "typeinst nnkOther:"
      echo n.treeRepr
      fail()

    return result

  case n.kind:
  of nnkIdent, nnkSym:
    echo "is this \"t\"?"
    echo n.strVal
    result.add n.strVal
  of nnkInfix:
    if n[0].repr in ["+=", "-=", "*=", "/="]:
      #self.toCodeExprInner(n[1], level + 1, isLhs)
      #result.add " "
      #self
      # TODO: maybe support this?
      fail()
    else:
      result.add "("
      result.add self.toCodeExprInner(n[1], level, isLhs)
      result.add " "
      if n[0].repr in ["mod"] and n[1].getType().repr != "int":
        result.add("fmod")
      else:
        result.add(n[0].strVal)
      result.add " "
      result.add self.toCodeExprInner(n[2], level, isLhs)
      result.add ")"
  of nnkDotExpr:
    result.add self.toCodeExprInner(n[0], level, isLhs)
    result.add "."
    result.add self.toCodeExprInner(n[1], level, isLhs)
  of nnkBracketExpr:
    #if n[0].kind == nnkBracketExpr:
    #  discard
    #else:
    #echo "test: " & $n.len
    #when not isTypeInst:
    result.add self.toCodeExprInner(n[0], level, isLhs)
    result.add "["
    result.add self.toCodeExprInner(n[1], level, false)
    result.add "]"
  #of nnkResult:
  #  discard
  of nnkChckRange:
    # skip check range and treat it as a hidden cast instead
    var typeStr = typeRename(n.getType.repr)
    #echo("typeStr 0: ", typeStr)
    result.add "(("
    result.add typeStr
    result.add ")"
    result.add self.toCodeExprInner(n[0], level, false)
    result.add ")"
  of nnkHiddenStdConv:
    var typeStr = typeRename(n.getType.repr)
    #echo("typeStr 1: ", typeStr)
    #if typeStr == "float" and n[1].kind == nnkIntLit:
    #  discard
    #elif typeStr == "float" and n[1].kind == nnkFloatLit:
    #  discard
    if typeStr.startsWith("range["):
      result.add self.toCodeExprInner(n[1], level, false)
    else:
      for j in 1 .. n.len-1:
        result.add "(("
        result.add typeStr
        result.add ")"
        result.add self.toCodeExprInner(n[1], level, false)
        result.add ")"

  else:
    when not isLhs:
      case n.kind:
      of nnkIntLit:
        #result.add("((int)" & $n.intVal & ")")
        result.add($n.intVal)
      of nnkUIntLit:
        result.add("((unsigned int)" & $n.intVal & ")")
      of nnkInt8Lit:
        result.add("((int8_t)" & $n.intVal & ")")
      of nnkUInt8Lit:
        result.add("((uint8_t)" & $n.intVal & ")")
      of nnkInt16Lit:
        result.add("((int16_t)" & $n.intVal & ")")
      of nnkUInt16Lit:
        result.add("((uint16_t)" & $n.intVal & ")")
      of nnkInt32Lit:
        result.add("((int32_t)" & $n.intVal & ")")
      of nnkUInt32Lit:
        result.add("((uint32_t)" & $n.intVal & ")")
      of nnkInt64Lit:
        result.add("((int64_t)" & $n.intVal & ")")
      of nnkUInt64Lit:
        result.add("((uint64_t)" & $n.intVal & ")")
      of nnkFloatLit, nnkFloat32Lit:
        result.add("((float)" & $n.floatVal & ")")
      #of nnkFloat32Lit:
      #  discard
      #of nnkFloat64Lit:
      #  discard
      of nnkObjConstr:
        #echo repr(n)
        ##echo n.strVal
        #echo n[0].kind
        var typeName: string
        #case n[0].kind:
        #of nnkBracketExpr:
        typeName = self.toCodeExprInner(
          n[0], level, isLhs, true, true
        )
        #else:
        #  typeName = n[0].strVal

        result.add "(("
        result.add typeName
        result.add "){"
        for i in 1 ..< n.len:
          #if n[i].len == 2:
          result.add "."
          result.add n[i][0].strVal
          result.add "="
          result.add self.toCodeExprInner(n[i][1], level, isLhs)
          #else:
          #  self.toCodeExprInner(n[i][0], level, isLhs)
          if i + 1 < n.len:
            result.add ", "
        result.add "})"
      of nnkCall:
        var procName: string = n[0].strVal #& "_f"
        #echo "expr nnkCall: " # & procName
        #echo n.treeRepr
        #echo n[0].getTypeInst().treeRepr
        var first: bool = true
        #if 
        #echo "pre for loop"
        for i in 1 ..< n.len:
          #--------
          #--------
          #echo n[i].treeRepr
          var paramType: NimNode
          #echo "tempificate:"
          #echo "n[i].kind: " & $n[i].kind & " " & $i
          case n[i].kind:
          of nnkExprEqExpr:
            #echo n[i][1].getTypeInst()
            #result.add self.toCodeExprInner(n[i][1], level, isLhs)
            paramType = n[i][1].getTypeInst()
          else:
            #echo n[i].getTypeInst()
            #result.add self.toCodeExprInner(n[i], level, isLhs)
            paramType = n[i].getTypeInst()

          if paramType.kind == nnkVarTy:
            let n = paramType
            fail()
          else:
            #echo "doing rename: "
            discard self.funcRenameIter(
              paramType=paramType,
              procName=procName,
              first=first,
            )
            #echo paramType.treeRepr
        #echo "post for loop"

        #echo "#--------"
        #echo "expr nnkCall: " & procName
        #echo n.treeRepr
        #echo ""
        #echo n.getTypeImpl().treeRepr
        #echo ""
        #echo n.getTypeInst().treeRepr
        #echo "--------"
        var returnType: string
        #returnType = self.toCodeExprInner(
        #  nodes=n[0].getTypeInst(),
        #)
        returnType = self.toCodeExprInner(
          nodes=n[0].getTypeInst()[0][0],
          level=level,
          isLhs=false,
          isTypeInst=true,
        )
        procName = procName & "_g" & returnType
        #procName = procName & "_" & returnType
        result.add procName
        result.add "("
        for i in 1 ..< n.len:
          case n[i].kind:
          of nnkExprEqExpr:
            result.add self.toCodeExprInner(n[i][1], level, isLhs)
          else:
            result.add self.toCodeExprInner(n[i], level, isLhs)
          if i + 1 < n.len:
            result.add ", "
        result.add ")"
      #of nnkCommand:
      #  
      else:
        #echo repr(n)
        #echo n.kind
        fail()
    else:
      #echo repr(n)
      #echo n.kind
      fail()

proc toCodeExpr(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int,
  isLhs: static bool,
  isTypeInst: bool=false
) =
  self.res.add self.toCodeExprInner(nodes, level, isLhs, isTypeInst)
  discard

proc toCodeAsgn(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int,
) =
  addIndent(self.res, level)
  let n = nodes
  case nodes.len:
    of 2:
      self.toCodeExpr(
        nodes=n[0],
        #res=res,
        level=level,
        isLhs=true,
      )
      self.res.add " = "
      self.toCodeExpr(
        nodes=n[1],
        #res=res,
        level=level,
        isLhs=false,
      )
    else:
      fail()
  self.res.addSmart(';')
      
  #for n in nodes:
  #  case n.kind:
  #  else:
  #    assert false
#proc toCodeCall(
#  self: var Convert,
#  nodes: NimNode,
#  #res: var string,
#  level = 0
#) =
#  addIndent(self.res, level)

proc typeDef(
  self: var Convert,
  topLevelNode: NimNode,
  level: int,
): string =
  #var typeName = ""

  #assert topLevelNode.kind in {nnkTypeDef}
  let n = topLevelNode
  result.add "typedef struct "
  result.add self.toCodeExprInner(n[0], level, true)
  result.add " {\n"
  if have(n, @[nnkEmpty, nnkObjectTy], 1):
    discard
  else:
    fail()
  result.add "} "
  result.add self.toCodeExprInner(n[0], level, true)
  result.add ";\n"

  #result = typeName

proc toCodeTypeSection(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0,
) =
  #echo nodes.kind
  addIndent(self.res, level)
  let n = nodes
  #for n in nodes:
  case n.kind:
  of nnkTypeDef:
    # `n[0]` should be the `nnkIdent` or `nnkSym`
    echo "in toCodeTypeSection:"
    echo n.len
  else:
    fail()

#proc toCodeTypeInst(
#  self: var Convert,
#  nodes: NimNode,
#  level = 0,
#): string = 
#  self.toCodeExprInner(nodes, level, false, true)

proc toCodeVarSection(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0,
) =
  addIndent(self.res, level)
  #echo repr(nodes) & " " & repr(nodes.kind)
  let n = nodes
  #echo "#----"
  #echo "toCodeVarSection(): start:"
  #echo n.repr
  #echo n.treeRepr
  #echo "----"
  case n.kind:
  of nnkIdentDefs:
    case n.len:
    of 3:
      #echo n[0].kind
      #echo n[1].kind
      if (
        (
          have(n, @[nnkSym]) 
        ) and (
          not have(n, @[nnkEmpty], 1)
        )
      ):
        #if n[1].strVal != "array":
        #if (
        #  (
        #    n[1].kind != nnkBracketExpr
        #  ) and (
        #    not (
        #      (
        #        n[1].kind == nnkCall
        #      ) and (
        #        n[1][0].repr == "[]"
        #      )
        #    )
        #  )
        #):
        #echo "#----"
        #echo "not an array?"
        #echo n.repr
        echo "in nnkVarSection:"
        echo n.treeRepr
        echo n[0].treeRepr
        #echo "----"

        #var hadArray: ref bool = new bool
        #hadArray[] = false
        self.hadArray = false
        var tempToAdd: string
        tempToAdd = self.toCodeExprInner(
          n[1].getTypeInst(),
          level, isLhs=false, isTypeInst=true, arrayPass=0,
          #hadArray=hadArray
        )
        echo "tempToAdd: " & tempToAdd
        self.res.add tempToAdd
        #echo "not eek: ", self.hadArray
        #echo "not eek: "
        let temp = self.hadArray
        self.res.add " "
        tempToAdd = self.toCodeExprInner(
          n[0], level, isLhs=true
        )
        #echo tempToAdd
        self.res.add tempToAdd
        #echo "post n[0]"
        if temp:
          #echo "past self.hadArray:"
          self.res.add self.toCodeExprInner(
            n[1].getTypeInst(),
            level, isLhs=false, isTypeInst=true, arrayPass=1,
          )
        if not have(n, @[nnkEmpty], 2):
          self.res.add " = "
          self.toCodeExpr(n[2], level, isLhs=false)
        self.res.add ";\n"
      #elif have(n, @[nnkSym, nnkEmpty]):
      #  if have(n, @[nnkSym], 2):
      #  else:
      #    fail()
      else:
        #echo "my-fail-notes:"
        #echo n.treeRepr
        fail()
      #of (nnkIdent, nnkIdent, nnkEmpty):
      #  discard
      #else:
      #  discard
    else:
      #echo "disallowed for now"
      #echo n
      fail()
    #for n in n:
    #  case n.kind:
    #  of nnkIdent:
    #    discard
    #  of nnkEmpty:
    #    discard
    #  of nnkObjConstr:
    #    discard
    #  else:
    #    echo n
    #    fail()
  else:
    #echo n
    fail()

proc toCodeStmts(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int
) = 
  addIndent(self.res, level)
  #echo repr(nodes)
  #echo repr(nodes.kind)
  #echo nodes.treeRepr
  proc innerFunc(
    self: var Convert,
    n: NimNode,
    level: int
  ) =
    #echo repr(n.kind)
    #echo n.len
    case n.kind:
    of nnkEmpty:
      discard
    of nnkSym:
      discard
    of nnkIfStmt:
      self.toCodeIfStmt(n, level + 1)
      #discard
    #of nnkForStmt:
    #  self.toCodeForStmt(n, level + 1)
    of nnkTypeSection:
      self.toCodeTypeSection(n[0], level + 1)
    of nnkVarSection:
      self.toCodeVarSection(n[0], level + 1)
    #of nnkV
    of nnkAsgn:
      self.toCodeAsgn(n, level + 1)
    of nnkCall:
      #self.toCodeCall(n, level + 1)
      self.toCodeExpr(n, level + 1, false)
    #of nnkElifBranch:
    #  discard
    #of nnkElse:
    #  discard
    of nnkBracket:
      #echo repr(n)
      #n.dumpAstGen
      #dumpLisp(n)
      #echo n.treeRepr
      discard
    of nnkCommentStmt:
      discard
    else:
      #echo n
      #echo repr(n)
      #echo n.kind
      fail()

  #echo repr(nodes)
  #echo nodes.kind
  #echo repr(nodes.kind)
  case nodes.kind:
    of nnkStmtList:
      for n in nodes:
        self.innerFunc(n, level)
    of nnkCommentStmt:
      discard
    else:
      self.res.addIndent level
      self.innerFunc(nodes, level)
      self.res.addSmart ';'
      self.res.add "\n"
      

#proc toCodeTopLevel(
#  self: var Convert,
#  topLevelNode: NimNode,
#  #res: var string,
#  level = 0
#) = 
#  assert topLevelNode.kind == nnkProcDef
#  for n in topLevelNode:
#    case n.kind:
#    of nnkEmpty:
#      discard
#    of nnkSym:
#      discard
#    of nnkTypeSection:
#      self.toCodeTypeSection(n, level + 1)
#    #of nnkFormalParams:
#    #  for param in n:
#    else:
#      self.toCodeStmts(n, level + 1)
#      discard


const ignoreFuncs = [
  "echo", "print", "debugEcho", "len", "$"
]

proc hasResult(
  self: var Convert,
  node: NimNode,
): bool =
  if node.kind == nnkSym and node.strVal == "result":
    return true
  for c in node.children:
    if self.hasResult(c):
      return true
  return false

proc procDef(
  self: var Convert,
  #parentNode: NimNode,
  procNode: NimNode,
  typeImpl: NimNode,
): (string, string) =
  var procName = ""
  var paramsStr = ""
  var returnType = "void"
  #result[0] = false
  result[0] = ""
  #echo topLevelNode.treeRepr
  #echo "procDef(): very start:"
  #echo procNode.treeRepr

  assert procNode.kind in {
    nnkProcDef, nnkFuncDef
  }
  assert typeImpl.kind in {
    nnkProcTy
  }
  #echo "#----"
  #echo "procDef(): start:"
  #echo procNode.kind
  #echo procNode.treeRepr
  #echo ""
  #echo typeImpl.kind
  #echo typeImpl.treeRepr
  #echo "----"
  for n in procNode:
    #echo n.kind
    #echo n.treeRepr
    #echo "----"
    #echo "for loop begin:"
    #echo procName
    #echo "--------"
    case n.kind:
    of nnkEmpty:
      discard
    of nnkPragma:
      # TODO: come back to this later
      discard
    of nnkSym:
      procName = $n
    of nnkAccQuoted:
      if n.len == 1:
        procName = $n[0]
      else:
        fail()
    of nnkBracket:
      if (
        (
          n[0].kind == nnkEmpty
        ) and (
          n[1].kind == nnkGenericParams
        )
      ):
        #echo n.treeRepr
        let n = n[1]
        if n[0].kind == nnkIdentDefs:
          #for i in 0 ..< n[0].len:
          ##for paramDef in n[0]:
          #  let paramDef = n[0][i]
          #  echo $i
          #  echo paramDef.treeRepr
          #  if (
          #    (
          #      paramDef.kind != nnkEmpty
          #    ) and (
          #      i != 0
          #    )
          #  ):
          #    echo "temp: asdf"
          #    fail()
          #  else:
          #    discard
          discard
        else:
          #echo "temp 1: asdf"
          fail()
      else:
        fail()
    of nnkFormalParams:
      if n[0].kind != nnkEmpty:
        #returnType = n[0].strVal
        #echo "returnType:"
        #echo n.treeRepr
        #echo ""
        #echo n[0].treeRepr
        #echo n[0].getTypeImpl().treeRepr
        #echo n[0].getTypeInst().treeRepr
        #echo typeImpl
        returnType = self.toCodeExprInner(
          nodes=typeImpl[0][0],
          level=0,
          isLhs=false,
          isTypeInst=true,
        )
        #echo returnType
        #echo "--------"
      var idx = 1
      for paramDef in n[1 .. ^1]:
        # the paramDef is like `x, y, z: float`
        #echo "paramDef:"
        #echo paramDef.treeRepr
        var first: bool = true
        if paramDef.kind != nnkEmpty:
          for param in paramDef[0 ..< ^2]:
          #for otherIdx in 0 ..< paramDef[0 ..< ^2].len:
            #let param = paramDef[otherIdx]
            #echo "param stuff:"
            #echo param.treeRepr
            #echo "the length: ", paramDef[0 ..< ^2].len()
            # Process each `x`, `y`, `z` in a loop
            paramsStr.add "  "
            let paramName = param.repr()
            #echo "here's the paramName: ", paramName
            #let paramType = param.getTypeInst()
            #let paramTypeImpl = param.getTypeImpl()
            #echo "#--------"
            #echo "testificate:"
            #echo typeImpl.treeRepr
            #echo ""
            #echo "----#----"
            #echo param.treeRepr
            #echo ""
            #echo paramType.treeRepr
            #echo ""
            #echo paramTypeImpl.treeRepr
            #echo "--------"
            #echo paramName & ": " & $paramType
            #dumpAstNode(n)
            #dumpTree(n)
            #if paramType.kind == nnkVarTy:
            #  #echo "test"
            #  #fail()
            #  #let n = paramType
            #  fail()
            ##elif paramType.kind == nnkBracketExpr:
            ##  #fail()
            #else:
            #paramsStr.add ""
            #paramsStr.add paramType.strVal
            #let tempParamsStr = self.toCodeExprInner(
            #  nodes=paramType,
            #  level=0,
            #  isLhs=false,
            #  isTypeInst=true,
            #)
            #if first:
            #  first = false
            #  procName.add "_f"
            #else:
            #  procName.add "_c"
            #procName.add tempParamsStr
            #paramsStr.add tempParamsStr
            #echo "procName: " & procName
            #echo "paramsStr.add: "
            #echo typeImpl[0][idx].treeRepr
            #echo ""
            #echo typeImpl[0][idx].treeRepr
            #echo "note:"
            #echo typeImpl[idx].treeRepr
            #echo "idx, otherIdx: " & $idx & " " & $otherIdx
            #echo typeImpl[0][idx].treeRepr
            #echo "kind: ", typeImpl[0][idx].kind
            case typeImpl[0][idx].kind:
            of nnkIdentDefs:
              #echo "temp: "
              #echo typeImpl[0][0][idx][1].treeRepr
              #echo typeImpl[0][idx].treeRepr
              #echo typeImpl[0][idx][1].treeRepr
              #echo "temp end:"
              paramsStr.add(
                self.funcRenameIter(
                  paramType=(
                    #paramType
                    typeImpl[0][idx][1]
                  ),
                  procName=procName,
                  first=first,
                )
              )
            else:
              discard
            #echo "idx: ", $idx
            paramsStr.add " "
            paramsStr.add paramName
            paramsStr.add ",\n"
            idx += 1
      if paramsStr.len > 0:
        # remove the final ",\n" since C doesn't support that
        paramsStr.setLen(paramsStr.len - 2)
        paramsStr.add "\n"
    else:
      #echo "ending? " & procName
      self.res.setLen(0)
      self.res.add "\n"
      procName = procName & "_g" & returnType
      #procName = procName & "_" & returnType
      result[0] = procName
      #echo "setting result[0]"
      #echo procName
      #echo "--------"

      self.res.add returnType & " " & procName  & "("
      #if result[0].len > 0:
      #  procName.add "_f" & result[0]
      #self.res.add "("
      if paramsStr.len == 0:
        self.res.add "void"
      else:
        self.res.add "\n"
        self.res.add paramsStr
      self.res.add ") {\n"
      self.useResult = self.hasResult(n)
      if self.useResult:
        self.res.addIndent(1)
        self.res.add returnType
        self.res.add " result;"
      self.toCodeStmts(n, 0)
      if self.useResult:
        if "return result" notin self.res[^20..^1]:
          self.res.addIndent(1)
          self.res.add "return result;\n"
        else:
          self.res.add "\n"
      else:
        self.res.add "\n"
      self.res.add "}"
    #echo "--------"
    #echo "for loop end:"
    #echo procName
    #echo "--------"

  #echo paramsStr
  #echo self.res
  #echo "test: "
  #result[0] = procName
  result[1] = self.res
  #echo result


proc findTopLevel(
  self: var Convert,
  topLevelNode: NimNode,
  #typeImpl: NimNode,
) =
  for n in topLevelNode:
    if n.kind != nnkEmpty:
      #echo "#----"
      #echo "findTopLevel(): n.kind outer:"
      #echo n.repr
      #echo n.treeRepr
      #echo "----"
      discard
    if n.kind == nnkCall:
      #echo n
      #dumpTree n
      if n[0].repr() == "[]":
        continue
      var procName = n[0].repr()
      if (
        (
          procName in ignoreFuncs
        ) or (
          procName in self.funcTbl
        )
      ):
        continue

      case n[0].kind:
      of nnkIdent:
        echo "findTopLevel(): nnkIdent"
        echo repr(n)
        #echo n[0]
        echo n.treeRepr
        echo "test in findTopLevel()"
        fail() # can't have 
      of nnkSym:
        #echo "#--------"
        #echo "findTopLevel(): nnkSym"
        #echo n[0].kind
        #echo n[0].symKind()
        let impl = n[0].getImpl()
        let innerTypeImpl = n[0].getTypeImpl()
        #let innerTypeInst = n[0].getTypeInst()
        #let innerType = n[0].getType()
        ###echo repr(impl)
        #echo "impl.treeRepr:"
        #echo impl.treeRepr
        #echo ""
        #echo "innerTypeImpl.treeRepr:"
        #echo innerTypeImpl.treeRepr
        #echo ""
        #echo n[0].repr()
        #echo "innerTypeInst.treeRepr:"
        #echo innerTypeInst.treeRepr
        #echo ""
        #echo "innerType.treeRepr:"
        #echo innerType.treeRepr
        #echo "--------"
        #self.nextProcRenameTbl(typeImpl)
        self.findTopLevel(
          impl#, innerTypeImpl
        )
        let myProcDef = self.procDef(impl, innerTypeImpl)
        let innerProcName = myProcDef[0]
        if (
          (
            innerProcName in ignoreFuncs
          ) or (
            innerProcName in self.funcTbl
          )
        ):
          #echo "continuing:"
          #echo procName
          #echo myProcDef[0]
          #echo myProcDef[1]
          #echo "..."
          continue
        self.funcSeq.add myProcDef[1]
        self.funcTbl[myProcDef[0]] = self.funcSeq[^1]
        #if self.procRenameTbl.len > 0:
        #  discard self.procRenameTbl.pop()
      else:
        #echo "test in findTopLevel() 1"
        #echo repr(n[0])
        discard

    self.findTopLevel(
      n#, typeImpl
    )


proc toPipelineCInner*(
  s: NimNode,
): string =
  var code: string
  #code.add "asdf"
  #echo s.treeRepr
  #echo $s.kind
  var n = getImpl(s)
  #echo "#--------"
  #echo "toPipelineCInner():"
  #echo n.treeRepr
  #echo "--------"
  ##var n = s
  #var n = s.getImplTransformed()
  #echo s.treeRepr
  #var n = s.bindSym()
  #var n = s
  #echo n.treeRepr
  #echo n.treeRepr

  ##var funcTbl: Table[string, string]
  ##var typedefTbl: Table[string, string]
  ##var convert: Convert = Convert(
  ##  #funcTbl=funcTbl,
  ##  #typedefTbl=typedefTbl,
  ##  #res=code,
  ##)
  var convert: Convert
  convert.findTopLevel(n)
  #echo convert.funcTbl
  #echo convert.typedefTbl
  #var globals: Table[string, string]

  ##for k, v in globals:
  ##  code.add(v)
  ##  code.add "\n"

  for v in convert.typedefSeq:
    code.add(v)
    code.add "\n"
  code.add "\n"

  for v in convert.funcSeq:
    code.add v
    code.add "\n"
  code.add "\n"

  ##toCodeTopLevel(topLevelNode=n, res=code, level=0)
  #code = convert.res

  #result = quote do:
  #  `code`
  result = code

macro toPipelineC*(
  s: typed,
): untyped =
  #echo s.treeRepr
  #result = quote do:
  newLit(toPipelineCInner(s))
  #result = quote do:
  #  ""
  #echo s.getImpl
  #toPipelineCInner(getImpl(s))
