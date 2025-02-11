StmtList
  TypeSection
    TypeDef
      Ident "Vec3"
      GenericParams
        IdentDefs
          Ident "T"
          Empty
          Empty
      ObjectTy
        Empty
        Empty
        RecList
          IdentDefs
            Postfix
              Ident "*"
              Ident "x"
            Ident "T"
            Empty
          IdentDefs
            Postfix
              Ident "*"
              Ident "y"
            Ident "T"
            Empty
          IdentDefs
            Postfix
              Ident "*"
              Ident "z"
            Ident "T"
            Empty
  ProcDef
    Ident "plus"
    Empty
    GenericParams
      IdentDefs
        Ident "T"
        Empty
        Empty
    FormalParams
      BracketExpr
        Ident "Vec3"
        Ident "T"
      IdentDefs
        Ident "left"
        BracketExpr
          Ident "Vec3"
          Ident "T"
        Empty
      IdentDefs
        Ident "right"
        BracketExpr
          Ident "Vec3"
          Ident "T"
        Empty
    Empty
    Empty
    StmtList
      Asgn
        Ident "result"
        ObjConstr
          BracketExpr
            Ident "Vec3"
            Ident "T"
          ExprColonExpr
            Ident "x"
            Infix
              Ident "+"
              DotExpr
                Ident "left"
                Ident "x"
              DotExpr
                Ident "right"
                Ident "x"
          ExprColonExpr
            Ident "y"
            Infix
              Ident "+"
              DotExpr
                Ident "left"
                Ident "y"
              DotExpr
                Ident "right"
                Ident "y"
          ExprColonExpr
            Ident "z"
            Infix
              Ident "+"
              DotExpr
                Ident "left"
                Ident "z"
              DotExpr
                Ident "right"
                Ident "z"
  ProcDef
    AccQuoted
      Ident "+"
    Empty
    Empty
    FormalParams
      Ident "Vec3"
      IdentDefs
        Ident "left"
        Ident "Vec3"
        Empty
      IdentDefs
        Ident "right"
        Ident "Vec3"
        Empty
    Empty
    Empty
    StmtList
      Asgn
        Ident "result"
        Par
          Command
            DotExpr
              Ident "left"
              Ident "plus"
            Ident "right"
  TypeSection
    TypeDef
      Ident "Asdf"
      GenericParams
        IdentDefs
          Ident "T"
          Empty
          Empty
      ObjectTy
        Empty
        Empty
        RecList
          IdentDefs
            Postfix
              Ident "*"
              Ident "a"
            Ident "int"
            Empty
          IdentDefs
            Postfix
              Ident "*"
              Ident "v"
            BracketExpr
              Ident "Vec3"
              Ident "T"
            Empty
  ProcDef
    Ident "doAsdf"
    Empty
    GenericParams
      IdentDefs
        Ident "T"
        Empty
        Empty
    FormalParams
      BracketExpr
        Ident "Asdf"
        Ident "T"
      IdentDefs
        Ident "asdf"
        BracketExpr
          Ident "Asdf"
          Ident "T"
        Empty
      IdentDefs
        Ident "b"
        Ident "int"
        Empty
    Empty
    Empty
    StmtList
      VarSection
        IdentDefs
          Ident "temp"
          BracketExpr
            Ident "Asdf"
            Ident "T"
          ObjConstr
            BracketExpr
              Ident "Asdf"
              Ident "T"
            ExprColonExpr
              Ident "a"
              Infix
                Ident "+"
                DotExpr
                  Ident "asdf"
                  Ident "a"
                Ident "b"
            ExprColonExpr
              Ident "v"
              ObjConstr
                BracketExpr
                  Ident "Vec3"
                  Ident "int"
                ExprColonExpr
                  Ident "x"
                  IntLit 0
                ExprColonExpr
                  Ident "y"
                  IntLit 1
                ExprColonExpr
                  Ident "z"
                  IntLit 2
      VarSection
        IdentDefs
          Ident "tempVec3"
          BracketExpr
            Ident "Vec3"
            Ident "int"
          Empty
      VarSection
        IdentDefs
          Ident "e"
          BracketExpr
            Ident "array"
            IntLit 3
            BracketExpr
              Ident "array"
              IntLit 8
              BracketExpr
                Ident "Asdf"
                Ident "T"
          Empty
      IfStmt
        ElifBranch
          Infix
            Ident "=="
            DotExpr
              DotExpr
                BracketExpr
                  BracketExpr
                    Ident "e"
                    IntLit 0
                  IntLit 0
                Ident "v"
              Ident "x"
            IntLit 0
          StmtList
            Asgn
              DotExpr
                BracketExpr
                  BracketExpr
                    Ident "e"
                    IntLit 1
                  IntLit 1
                Ident "a"
              IntLit 9
        ElifBranch
          Infix
            Ident "=="
            DotExpr
              DotExpr
                BracketExpr
                  BracketExpr
                    Ident "e"
                    IntLit 0
                  IntLit 0
                Ident "v"
              Ident "y"
            IntLit 0
          StmtList
            Asgn
              DotExpr
                DotExpr
                  BracketExpr
                    BracketExpr
                      Ident "e"
                      IntLit 0
                    IntLit 0
                  Ident "v"
                Ident "z"
              IntLit 8
        Else
          StmtList
            Asgn
              DotExpr
                BracketExpr
                  BracketExpr
                    Ident "e"
                    IntLit 2
                  IntLit 2
                Ident "a"
              IntLit 7
      VarSection
        IdentDefs
          Ident "f"
          BracketExpr
            Ident "array"
            IntLit 3
            BracketExpr
              Ident "array"
              IntLit 8
              BracketExpr
                Ident "Vec3"
                Ident "int"
          Empty
      Asgn
        Ident "result"
        BracketExpr
          BracketExpr
            Ident "e"
            DotExpr
              Ident "temp"
              Ident "a"
          Infix
            Ident "+"
            Par
              DotExpr
                Ident "temp"
                Ident "a"
            IntLit 1
  ProcDef
    Ident "myMain"
    Empty
    Empty
    FormalParams
      Empty
    Empty
    Empty
    StmtList
      VarSection
        IdentDefs
          Ident "a"
          BracketExpr
            Ident "Asdf"
            Ident "int"
          Empty
      VarSection
        IdentDefs
          Ident "c"
          BracketExpr
            Ident "Asdf"
            Ident "int"
          Empty
      VarSection
        IdentDefs
          Ident "b"
          BracketExpr
            Ident "Asdf"
            Ident "int"
          Call
            DotExpr
              Ident "c"
              Ident "doAsdf"
            ExprEqExpr
              Ident "b"
              IntLit 8
      Asgn
        Ident "b"
        Call
          DotExpr
            Ident "a"
            Ident "doAsdf"
          IntLit 9
  Call
    Ident "myMain"
