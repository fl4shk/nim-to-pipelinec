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

type
  Convert = object
    procRenameTbl: seq[Table[string, string]]
    objRenameTbl: seq[Table[string, string]]
    #renameLevel: int = 0
    funcTbl: Table[string, string]
    funcSeq: seq[string]
    typedefTbl: Table[string, seq[string]]
    typedefSeq: seq[string]
    res: string
    useResult: bool

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
): string

proc nextProcRenameTbl(
  self: var Convert,
  n: NimNode,
) =
  #self.procRenameTbl.clear()
  echo "nextProcRenameTbl():"
  echo n.treeRepr
  if n.kind == nnkProcTy:
    if n[0].kind == nnkFormalParams:
      for paramDef in n[0]:
        if paramDef.kind == nnkBracketExpr:
          #echo paramDef.
          discard
        else:
          discard
    else:
      #n[0].
      fail()
  else:
    fail()

  

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
      self.toCodeStmts(n[1], level + 1)
      addIndent(self.res, level)
      self.res.add "}\n"
    of nnkElse:
      addIndent(self.res, level)
      self.res.add "else {\n"
      self.toCodeStmts(n[0], level + 1)
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
  of "float32":
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
  )
  if first:
    first = false
    procName.add "_f"
  else:
    procName.add "_c"
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
): string =
  var n = nodes
  #echo n.kind
  if isTypeInst:
    #echo "isTypeInst: " & repr(n)
    case n.kind:
    of nnkIdent, nnkSym:
      var tempStr: (bool, string) = typeRenameInner(n.strVal)
      #result.add n.strVal
      if not tempStr[0]:
        result.add tempStr[1]
      else:
        result.add tempStr[1].replace("_", "_a")
      if not isSingle:
        result.add "_c"
    #of nnkSym:
    #  result.add n.strVal
    of nnkBracketExpr:
      result.add self.toCodeExprInner(n[0], level, isLhs, true, false)
      result.add "_b"
      result.add self.toCodeExprInner(n[1], level, isLhs, true, false)
      result.add "_d"
    #of nnkUIntLit, nnkUInt8Lit, nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit:
    #  #result.add("((int)" & $n.intVal & ")")
    #  # (old concept for the transpiler, couldn't get it to work!):
    #  # only allow unsigned integer literals
    #  result.add($n.intVal)
    of nnkCall:
      #echo "typeinst nnkCall: "
      #echo n.treeRepr
      if n.len >= 3:
        if n[0].kind == nnkOpenSymChoice:
          result.add self.toCodeExprInner(
            n[1], level, isLhs, isTypeInst, false
          )
          result.add "_b"
          for param in n[2 .. ^1]:
            result.add self.toCodeExprInner(
              param, level, isLhs, isTypeInst, false
            )
            result.add "_d"
        else:
          #echo n.repr
          #echo n.treeRepr
          fail()
      else:
        #echo "typeinst nnkCall other len: "
        #echo n.repr
        #echo n.treeRepr
        fail()
    else:
      #echo "typeinst nnkOther:"
      #echo n.treeRepr
      fail()

    return result

  case n.kind:
  of nnkIdent, nnkSym:
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
          n[0], level, isLhs, true
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
        #echo "expr nnkCall: " & procName
        #echo n.treeRepr
        var first: bool = true
        for i in 1 ..< n.len:
          #--------
          #--------
          #echo n[i].treeRepr
          var paramType: NimNode
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
            discard self.funcRenameIter(
              paramType=paramType,
              procName=procName,
              first=first,
            )

        echo "expr nnkCall: " & procName
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
        if (
          n[1].kind != nnkBracketExpr
        ):
          #echo repr(n)
          #echo n.treeRepr
          #echo "----"
          self.toCodeExpr(n[1], level, isLhs=false, isTypeInst=true)
          self.res.add " "
          self.toCodeExpr(n[0], level, isLhs=true)
          if not have(n, @[nnkEmpty], 2):
            self.res.add " = "
            self.toCodeExpr(n[2], level, isLhs=false)
        elif (
          (
            have(n[1], @[nnkSym])
          )
          #and (
          #)
        ):
          #echo repr(n)
          if n[1][0].strVal == "array":
            proc handleArray(
              self: var Convert,
              nodes: NimNode,
              ret: var seq[string],
              level = 0
            ) =
              let n = nodes
              if (n.kind == nnkBracketExpr):
                #if have(n, @[nnkSym]):
                #echo "n.kind == nnkBracketExpr: " & repr(n) & ": " & $n.len
                #echo n.kind
                ret.add self.toCodeExprInner(
                  nodes=n[1],
                  level=level,
                  isLhs=false,
                )
                if (
                  (
                    have(n, @[nnkSym])
                  ) and (
                    n[0].strVal == "array"
                  )
                ):
                  #echo "have array: " & n[0].strVal
                  self.handleArray(
                    nodes=n[2],
                    ret=ret,
                    level=level,
                  )
                else:
                  ret.add self.toCodeExprInner(
                    nodes=n,
                    level=level,
                    isLhs=false,
                    isTypeInst=true,
                  )
                  #if n.len > 0:
                  #  echo "don't have array: " & n[0].strVal & ": " & $n.len
              elif n.kind == nnkSym:
                #echo "handleArray(): " & repr(n)
                ret.add self.toCodeExprInner(
                  nodes=n,
                  level=level,
                  isLhs=false,
                  isTypeInst=true,
                )
              else:
                fail()
            var mySeq: seq[string]
            #echo repr(n)
            #echo repr(n[0])
            #echo repr(n[1])
            self.handleArray(
              nodes=n[1],
              ret=mySeq,
              level=level
            )
            self.res.add mySeq[^1]
            self.res.add " "
            self.toCodeExpr(n[0], level, isLhs=false, isTypeInst=true)
            if mySeq.len > 1:
              for i in 0 ..< mySeq.len - 1:
                self.res.add "["
                self.res.add mySeq[i]
                self.res.add "]"
          else:
            #echo "test: " & repr(n)
            self.toCodeExpr(n[1], level, isLhs=true, isTypeInst=true)
            self.res.add " "
            self.toCodeExpr(n[0], level, isLhs=true)
            if not have(n, @[nnkEmpty], 2):
              self.res.add " = "
              self.toCodeExpr(n[2], level, isLhs=false)
        else:
          fail()
        self.res.add ";\n"
      #elif have(n, @[nnkSym, nnkEmpty]):
      #  if have(n, @[nnkSym], 2):
      #  else:
      #    fail()
      else:
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
  topLevelNode: NimNode,
): (string, string) =
  var procName = ""
  var paramsStr = ""
  var returnType = "void"
  #result[0] = false
  result[0] = ""
  #echo topLevelNode.treeRepr

  assert topLevelNode.kind in {
    nnkProcTy, nnkProcDef, nnkFuncDef
  }
  for n in topLevelNode:
    #echo n.kind
    #echo n.treeRepr
    #echo "----"
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
        echo n.treeRepr
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
          echo "temp 1: asdf"
          fail()
      else:
        fail()
    of nnkFormalParams:
      if n[0].kind != nnkEmpty:
        #returnType = n[0].strVal
        returnType = self.toCodeExprInner(
          nodes=n[0],
          level=0,
          isLhs=false,
          isTypeInst=true,
        )
      for paramDef in n[1 .. ^1]:
        # the paramDef is like `x, y, z: float`
        var first: bool = true
        if paramDef.kind != nnkEmpty:
          for param in paramDef[0 ..< ^2]:
          #for idx in 0 .. paramDef[0 ..< ^2].len:
            #let param = paramDef[idx]
            # Process each `x`, `y`, `z` in a loop
            paramsStr.add "  "
            let paramName = param.repr()
            let paramType = param.getTypeInst()
            #echo paramName & ": " & $paramType
            #dumpAstNode(n)
            #dumpTree(n)
            if paramType.kind == nnkVarTy:
              #echo "test"
              #fail()
              let n = paramType
              fail()
            #elif paramType.kind == nnkBracketExpr:
            #  #fail()
            else:
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
              paramsStr.add (
                self.funcRenameIter(
                  paramType=paramType,
                  procName=procName,
                  first=first,
                )
              )
            paramsStr.add " "
            paramsStr.add paramName
            paramsStr.add ",\n"
      if paramsStr.len > 0:
        # remove the final ",\n" since C doesn't support that
        paramsStr.setLen(paramsStr.len - 2)
        paramsStr.add "\n"
    else:
      self.res.setLen(0)
      self.res.add "\n"
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

  #echo paramsStr
  #echo self.res
  result[0] = procName
  result[1] = self.res


proc findTopLevel(
  self: var Convert,
  topLevelNode: NimNode,
  firstIter: bool=false,
) =
  echo "findTopLevel:"
  echo topLevelNode.treeRepr
  #for n in topLevelNode:
  proc innerFunc(
    self: var Convert,
    n: NimNode,
  )
  proc innerInnerFunc(
    self: var Convert,
    n: NimNode
  ) = 
    #var procName: string
    #echo n.repr
    #if n.repr == "[]":
    #  echo "have n.repr == \"[]\""
    #  echo n.treeRepr
    #  procName = self.toCodeExprInner(
    #    nodes=n,
    #    level=0,
    #    isLhs=false,
    #    isTypeInst=true,
    #  )
    #else:
    #  procName = n.repr

    #if repr(n) == "[]":
    #  echo "test"
    #  echo repr(n)
    #  echo n.treeRepr
    #  #continue
    #  #proc findTopLevelInnerFunc(
    #  #  n: NimNode,
    #  #): string =
    #  #  case n.kind:
    #  #  of nnkBracketExpr:
    #  #    discard
    #  #  else:
    #  #    discard
    #  procName = 
    #  #procName = n[1].repr.replace("_", "_a")
    #  #for i in 2 ..< n.len:
    #  #  procName.add findTopLevelInnerFunc(n[i])
    #  #  #procName.add "_b"
    #  #  #procName.add n[2].repr
    #else:
    #  procName = repr n
    #echo repr(n)
    #echo repr(n)
    #echo n.kind
    #echo n.kind
    #echo n.len
    #echo n.getSym 
    #echo n.strVal

    #let ast = n.getAst
    #echo repr(ast)
    #echo getType(n)
    #try: 
    #echo "procName: " & procName

    if n.repr == "[]":
      echo "testificate"
      echo n.treeRepr
      #echo procName
      discard
    else:
      #let tempImpl = n.getTypeInst()
      #echo "tempImpl:"
      #echo tempImpl.treeRepr

      case n.kind:
      of nnkIdent:
        #echo repr(n)
        #echo n
        #echo n.treeRepr
        #echo "test in findTopLevel()"
        fail() # can't have 
      of nnkSym:
        #echo "#----"
        #echo n.kind
        #echo n.symKind()
        #echo "#----"
        let impl = n.getImpl()
        echo "doing inner findTopLevel"
        #self.findTopLevel(impl)
        echo impl.treeRepr
        self.innerFunc(impl)
        #let tempProcDef = self.procDef(impl)

        ##procName.add "_f"
        ##procName.add tempProcDef[0]
        #let procName = tempProcDef[0]
        ##echo "test in of nnkSym: " & procName
        #if (
        #  (
        #    procName in ignoreFuncs
        #  ) or (
        #    procName in self.funcTbl
        #  )
        #):
        #  #echo "ignoring"
        #  return

        #self.funcSeq.add tempProcDef[1]
        #self.funcTbl[procName] = self.funcSeq[^1]
      of nnkCall:
        echo "innerInnerFunc: nnkCall:"
        echo n.treeRepr
        self.innerFunc(n)
      else:
        echo "test in findTopLevel() 1"
        echo repr(n)
        discard
  proc innerFunc(
    self: var Convert,
    n: NimNode,
  ) = 
    if n.kind != nnkEmpty:
      echo "outer:"
      echo n.kind
      #echo n.repr
      #echo n.treeRepr
      echo ""
    if n.kind == nnkSym:
      echo "innerFunc(): nnkSym"
      self.innerInnerFunc(n)
    elif n.kind == nnkProcDef:
      echo "innerFunc(): nnkProcDef"
      let typeImpl = n[0].getTypeInst()
      echo "#--------"
      echo "impl:"
      echo n.treeRepr #n.getImpl().treeRepr
      echo "#--------"
      #echo "impl 1:"
      #echo n.kind
      echo "typeImpl:"
      echo typeImpl.treeRepr
      echo "#--------"
      echo "typeInst:"
      echo n[0].getTypeImpl().treeRepr
      echo "#--------"
      echo "#--------"
      #echo "#--------"
      #echo "#--------"
      #self.renameLevel += 1
      self.nextProcRenameTbl(
        #n.getTypeInst()
        typeImpl
      )
      echo "debug:"
      ##echo topLevelNode.getTypeInst().treeRepr
      ##echo topLevelNode.getImpl().treeRepr
      #echo "#----"
      #echo typeImpl.treeRepr
      #echo "#----"
      #echo n.getTypeImpl().treeRepr
      #echo "#----"
      #echo n.getTypeInst().treeRepr

      #echo impl.treeRepr
      #echo repr(impl)
      #echo "#----"
      #self.innerInnerFunc(n[0])
      for i in 1 ..< n.len:
        echo $i
        echo n[i].treeRepr
        if n[i].kind != nnkEmpty:
          self.findTopLevel(n[i])
      let tempProcDef = self.procDef(n)

      #procName.add "_f"
      #procName.add tempProcDef[0]
      let procName = tempProcDef[0]
      #echo "test in of nnkSym: " & procName
      if (
        (
          procName in ignoreFuncs
        ) or (
          procName in self.funcTbl
        )
      ):
        echo "ignoring: " & procName
        echo procName in ignoreFuncs
        echo procname in self.funcTbl
        return

      echo "funcSeq:" & $self.funcSeq
      self.funcSeq.add tempProcDef[1]
      self.funcTbl[procName] = self.funcSeq[^1]
    elif n.kind == nnkCall:
      echo "innerFunc(): nnkCall"
      echo n.treeRepr
      self.innerInnerFunc(n[0])
      #echo "primary:"
      #echo n.repr
      #echo n.treeRepr
      #echo ""
      #echo n
      #dumpTree n
      #var procName: string


      #except:
      #  try:
      #    discard
      #  except:
      #    discard

        
        

      #try:
      #  let impl = n[0].getAst()
      #except:
      #  discard

    #if (
    #  n.kind == nnkVarSection
    #):
    #  #dumpAstGen(n)
    #  #echo "testificate"
    #  if repr(n[0]) == "[]":
    #    continue
    #  for n in n:
    #    case n.kind:
    #    of nnkIdentDefs:
    #      case n.len:
    #      of 2, 3:
    #        if have(n, @[nnkIdent, nnkIdent]):
    #          discard
    #        elif have(n, @[nnkIdent, nnkEmpty]):
    #          # TODO: support this
    #        #  discard
    #        #else:
    #          fail()
    #      else:
    #        fail()
    #    else:
    #      fail()

    #if (
    #  n.kind == nnkTypeSection
    #):
    #  #dumpTree n
    #  #echo n
    #  if repr(n[0]) == "[]":
    #    #echo "continuing..."
    #    continue
    #  #echo repr n[0]
    #  let typedefName = repr n[0]
    #  if typedefName in self.typedefTbl:
    #    continue
    #  echo typedefName
    #  #let myType = n[0].getTypeInst()
    #  #echo myType
    #  #let impl = n[0].getImpl()
    #  #findFuncsTypedefs(impl, funcTbl, typedefTbl)
    #  #typedefTbl[typedefName] = typeDef(impl)

    # TODO: possibly add this back? Well... maybe not!
  for n in topLevelNode:
    #self.innerFunc(n)
    echo "outer loop"
    echo n.repr
    echo n.treeRepr
    self.findTopLevel(n)
  if firstIter:
    echo "firstIter"
    self.innerInnerFunc(topLevelNode)
  else:
    self.innerFunc(topLevelNode)
      #self.findTopLevel(n)


proc toPipelineCInner*(
  s: NimNode,
): string =
  var code: string
  #code.add "asdf"
  echo s.treeRepr
  echo $s.kind
  #var n = getImpl(s)
  ##var n = s
  #var n = s.getImplTransformed()
  #echo s.treeRepr
  #var n = s.bindSym()
  var n = s
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
  convert.findTopLevel(n, true)
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
