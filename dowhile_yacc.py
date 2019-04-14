from dowhile_lex import symbol_table
def flatten(l):
    output = []
    def removeNestings(l):
        for i in l:
            if type(i) == list:
                removeNestings(i)
            else:
                output.append(i)
    if type(l) == list:
        removeNestings(l)
    else:
        output.append(l)
    return output

def retrieve(t):
    if t in symbol_table:
        if symbol_table[t]['valid']:
            t = symbol_table[t]['value']
        else:
            print("error line:",symbol_table[t]["token"],"   rhs = ", t)
    return t

from anytree import Node
from anytree import RenderTree
# from anytree.dotexport import RenderTreeGraph
from anytree.exporter import DotExporter

tokens = [
"BOOLEAN", "BREAK", "BYTE",
"CHAR", "CLASS", "CONTINUE","COMMENT",
"DO", "DOUBLE",
"FLOAT", "IMPORT", "INT",
"LONG",
"NEW",
"PACKAGE", "PRIVATE", "PROTECTED", "PUBLIC",
"RETURN",
"SHORT", "STATIC",
"THIS",
"VOID",
"WHILE","FOR",
"OP_INC", "OP_DEC", "OP_DIM",
"OP_GE", "OP_LE", "OP_EQ", "OP_NE",
"OP_LAND", "OP_LOR",
"ASS_MUL", "ASS_DIV", "ASS_MOD", "ASS_ADD", "ASS_SUB", "ASS_OR", "ASS_AND",
"IDENTIFIER", "LITERAL", "BOOLLIT", "NUM_LITERAL"
]



# Build the lexer


# Precedence rules for the arithmetic operators
# precedence = (
#     ('left','PLUS','MINUS'),
#     ('left','TIMES','DIVIDE'),
#     ('right','UMINUS'),
#     )


start = 'CompilationUnit'
root = Node("root")
# def p_OP_DIM(p):
#     'OP_DIM :'
#     pass

def p_error(p):
    print("error")
    # get formatted representation of stack
    # stack_state_str = ' '.join([symbol.type for symbol in parser.symstack][1:])

    # print('Syntax error in input! Parser State:{} {} . {}'
    #       .format(parser.state,
    #               stack_state_str,
    #               p))

def p_TypeSpecifier(p):
    '''TypeSpecifier : TypeName
    | TypeName Dims
    '''
    
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = Node("TypeSpecifier",children=[p[1],p[2]])
    # print("xdfgbhnjkl",p[:])

def p_TypeName(p):
    '''TypeName : PrimitiveType
    | QualifiedName
    '''
    p[0] = p[1]

def p_PrimitiveType(p):
    '''PrimitiveType : BOOLEAN
    | CHAR
    | BYTE
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    | VOID
    '''
    p[0] = Node(p[1])

def p_SemiColons(p):
    '''SemiColons : ';'
    | SemiColons ';'
    '''
    if(len(list(p)) == 3):
        p[0] = Node("SemiColons",children=[p[1],Node(p[2])])
    else:
        p[0] = Node("SemiColons",children=[Node(p[1])])

def p_CompilationUnit(p):
    '''CompilationUnit : ProgramFile
    '''
    root.children = [p[1]]
    for pre, _, node in RenderTree(root):
        print("%s%s" % (pre, node.name))
    # DotExporter(root).to_dotfile("tree.dot")
    p[0] = root

def p_ProgramFile(p):
    '''ProgramFile : PackageStatement ImportStatements TypeDeclarations
    | PackageStatement ImportStatements
    | PackageStatement                  TypeDeclarations
    |                  ImportStatements TypeDeclarations
    | PackageStatement
    |                  ImportStatements
    |                                   TypeDeclarations
    '''
    p[0] = Node("ProgramFile",children = p[1:])
    # p[0] = p[1:]

def p_PackageStatement(p):
    '''PackageStatement : PACKAGE QualifiedName SemiColons
    '''
    p[0] = Node("PackageStatement",children=[Node(p[1]),p[2],p[3]])

def p_TypeDeclarations(p):
    '''TypeDeclarations : TypeDeclarationOptSemi
    | TypeDeclarations TypeDeclarationOptSemi
    '''
    p[0] = Node("TypeDeclarations",children = p[1:])
    # p[0] = p[1:]

def p_TypeDeclarationOptSemi(p):
    '''TypeDeclarationOptSemi : TypeDeclaration
        | TypeDeclaration SemiColons
    '''
    p[0] = Node("TypeDeclarationOptSemi",children = p[1:])
    # p[0] = p[1:]

def p_ImportStatements(p):
    '''ImportStatements : ImportStatement
    | ImportStatements ImportStatement
    '''
    p[0] = Node("ImportStatements",children=p[1:])

def p_ImportStatement(p):
    '''ImportStatement : IMPORT QualifiedName SemiColons
    | IMPORT QualifiedName '.' '*' SemiColons
    '''
    if(len(list(p)) == 4):
        p[0] = Node("ImportStatement",children = [Node(p[1]),p[2],p[3]])
    else:
        p[0] = Node("ImportStatement",children = [Node(p[1]),p[2],Node(p[3]),Node(p[4]),p[5]])

def p_QualifiedName(p):
    '''QualifiedName : IDENTIFIER
    | QualifiedName '.' IDENTIFIER
    '''
    if(len(list(p)) == 4):
        p[0] = Node("QualifiedName",children = [p[1],Node(p[3])])
    else:
        p[0] = Node(p[1])

def p_TypeDeclaration(p):
    '''TypeDeclaration : ClassHeader '{' FieldDeclarations '}'
    | ClassHeader '{' '}'
    '''
    if(len(list(p)) == 5):
        p[0] = Node("Type Declaration",children = [p[1],Node(p[2]),p[3],Node(p[4])])
    else:
        p[0] = Node("Type Declaration",children = [p[1],Node(p[2]),Node(p[3])])

    # p[0] = p[1:]

def p_ClassHeader(p):
    '''ClassHeader : Modifiers ClassWord IDENTIFIER
    |           ClassWord IDENTIFIER
    '''
    if len(list(p))==4:
        symbol_table[p[3]]['modifiers'] = flatten(p[1])
        var = Node(p[3])
        p[0] = Node("ClassHeader",children = [p[1],p[2],var])
    else:
        symbol_table[p[2]]['modifiers'] = None
        var = Node(p[2])
        p[0] = Node("ClassHeader",children = [p[1], var])
    symbol_table[flatten(list(p))[-1]]["valid"] = True
    symbol_table[flatten(list(p))[-1]]["dtype"] = "class"


    # p[0] = p[1:]

def p_Modifiers(p):
    '''Modifiers : Modifier
    | Modifiers Modifier
    '''
    p[0] = Node("Modifiers",children = p[1:])
    # p[0] = p[1:]

def p_Modifier(p):
    '''Modifier : PUBLIC
    | PROTECTED
    | PRIVATE
    | STATIC
    '''
    var = Node(p[1])
    p[0] = Node("Modifier",children = [var])
    # p[0] = p[1:]

def p_ClassWord(p):
    '''ClassWord : CLASS'''
    var = Node(p[1])
    p[0] = Node("Class",children = [var])
    # p[0] = p[1:]

def p_FieldDeclarations(p):
    '''FieldDeclarations : FieldDeclarationOptSemi
        | FieldDeclarations FieldDeclarationOptSemi
    '''
    p[0] = Node("Field Declaration",children = p[1:])

def p_FieldDeclarationOptSemi(p):
    '''FieldDeclarationOptSemi : FieldDeclaration
        | FieldDeclaration SemiColons
    '''
    # p[0] = p[1:]
    p[0] = Node("Field Declaration Opt Semi",children=p[1:])

def p_FieldDeclaration(p):
    '''FieldDeclaration : FieldVariableDeclaration ';'
    | MethodDeclaration
    | ConstructorDeclaration
    | StaticInitializer
        | NonStaticInitializer
        | TypeDeclaration
    '''
    if(len(list(p)) == 2):
        p[0] = Node("FieldDeclaration",children=[p[1]])
    else:
        p[0] = Node("FieldDeclaration",children=[p[1],Node(p[2])])

def p_FieldVariableDeclaration(p):
    '''FieldVariableDeclaration : Modifiers TypeSpecifier VariableDeclarators
    |           TypeSpecifier VariableDeclarators
    '''
    if len(list(p))==4:
        for variable in flatten(list(p[3])):
            if variable in symbol_table:
                symbol_table[variable]['modifiers'] = flatten(p[1])
                symbol_table[variable]['value'] = 'None'
                symbol_table[variable]["valid"] = True
                symbol_table[variable]['dtype'] = flatten(list(p[2]))[0]
                # symbol_table[(variable,True)]['global'] = True
    else:
        for variable in flatten(list(p[2])):
            if variable in symbol_table:
                symbol_table[variable]['modifiers'] = None
                symbol_table[variable]['value'] = 'None'
                symbol_table[variable]["valid"] = True
                symbol_table[variable]['dtype'] = flatten(list(p[1]))[0]
                # symbol_table[(variable,True)]['global'] = True
    p[0] = Node("FieldVariableDeclaration",children = p[1:])

def p_VariableDeclarators(p):
    '''VariableDeclarators : VariableDeclarator
    | VariableDeclarators ',' VariableDeclarator
    '''
    if(len(list(p)) == 4):
        p[0] = Node("VariableDeclarators",children=[p[1],p[3]])
    else:
        p[0] = Node("VariableDeclarators",children=[p[1]])

def p_VariableDeclarator(p):
    '''VariableDeclarator : DeclaratorName
    | DeclaratorName '=' VariableInitializer
    '''
    if len(list(p))==4:
        variable = p[1].name
        # symbol_table[variable]['value'] = flatten(list(p[3]))[0]
        p[0] = Node("=",children=[p[1],p[3]])
    else:
        p[0] = p[1]

def p_VariableInitializer(p):
    '''VariableInitializer : Expression
    | '{' '}'
        | '{' ArrayInitializers '}'
    '''

    if(len(list(p)) == 2):
        p[0] = p[1]
    elif(len(list(p)) == 3):
        p[0] = Node("VariableInitializer",children=[Node(p[1]),Node(p[2])])
    else:
        p[0] = p[2]
        # p[0] = Node("VariableInitializer",children=[Node(p[1]),p[2],Node(p[3])])
    # print(p[0])

def p_ArrayInitializers(p):
    '''ArrayInitializers : VariableInitializer
    | ArrayInitializers ',' VariableInitializer
    | ArrayInitializers ','
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    elif(len(list(p)) == 4):
        p[0] = Node("ArrayInitializers",children=[p[1],p[3]])
    else:
        p[0] = Node("ArrayInitializers",children=[p[1]])

def p_MethodDeclaration(p):
    '''MethodDeclaration : Modifiers TypeSpecifier MethodDeclarator        MethodBody
    |           TypeSpecifier MethodDeclarator        MethodBody
    '''
    if len(list(p)) == 5:
        method = (p[3].children)[0]
        # method = flatten(list(p[3]))[0]
        # symbol_table[method.name]['modifiers'] = p[1].children
        # symbol_table[method.name]["valid"] = True
        # symbol_table[method.name]['dtype'] = (p[2].children)[0]
    else:
        method = (p[2].children)[0]
        # method =flatten(list(p[2]))[0]
        # symbol_table[method]['modifiers'] = None
        # symbol_table[method]["valid"] = True
        # symbol_table[method]['dtype'] = (p[1].children)[0]
    p[0] = Node("MethodDeclaration",children=p[1:])

def p_MethodDeclarator(p):
    '''MethodDeclarator : DeclaratorName '(' ParameterList ')'
    | DeclaratorName '(' ')'
    | MethodDeclarator OP_DIM
    '''
    if len(list(p)) == 5:
        method = flatten(list(p[1]))[0]
        symbol_table[method]['params'] = []
        for param in flatten(list(p[3])):
            if param in symbol_table and param not in symbol_table[method]['params']:
                if symbol_table[param]['type'] =='identifier':
                    symbol_table[method]['params'].append(param)

    p[0] = Node("MethodDeclarator",children=[p[1],Node(p[2]),Node(p[3])])

def p_ParameterList(p):
    '''ParameterList : Parameter
    | ParameterList ',' Parameter
    '''
    p[0] = p[1:]

def p_Parameter(p):
    '''Parameter : TypeSpecifier DeclaratorName
    '''
    variable = flatten(list(p[2]))[0]
    symbol_table[variable]['dtype'] = flatten(list(p[1]))[0]
    symbol_table[variable]['valid'] = True
    p[0] = Node("Params",children=[p[1],p[2]])

def p_DeclaratorName(p):
    '''DeclaratorName : IDENTIFIER
    | DeclaratorName OP_DIM
    | DeclaratorName '[' NUM_LITERAL ']'
    '''

    if(len(list(p)) == 2):
        p[0] = Node(p[1])
    elif(len(list(p)) == 3):
        p[0] = Node("[]",children =[p[1]])
    else:
        p[0] = Node("[]",children =[p[1],Node(p[3])])

def p_MethodBody(p):
    '''MethodBody : Block
    | ';'
    '''
    if(isinstance(p[1],Node)):
        p[0] = p[1]
    else:
        p[0] = Node(";")
def p_ConstructorDeclaration(p):
    '''ConstructorDeclaration : Modifiers ConstructorDeclarator        Block
    |           ConstructorDeclarator        Block
    '''
    if len(list(p)) == 4:
        method = flatten(list(p[2]))[0]
        symbol_table[method]['modifiers'] = flatten(p[1])
        symbol_table[method]["valid"] = True
    else:
        method =flatten(list(p[1]))[0]
        symbol_table[method]['modifiers'] = None
        symbol_table[method]["valid"] = True
    p[0] = p[1:]

def p_ConstructorDeclarator(p):
    '''ConstructorDeclarator : IDENTIFIER '(' ParameterList ')'
    | IDENTIFIER '(' ')'
    '''
    if len(list(p)) == 5:
        method = flatten(list(p[1]))[0]
        symbol_table[method]['params'] = []
        for param in flatten(list(p[3])):
            if param in symbol_table and param not in symbol_table[method]['params']:
                if symbol_table[param]['type'] =='IDENTIFIER':
                    symbol_table[method]['params'].append(param)
    p[0] = p[1:]

def p_StaticInitializer(p):
    '''StaticInitializer : STATIC Block
    '''
    p[0] = p[1:]

def p_NonStaticInitializer(p):
    '''NonStaticInitializer : Block
    '''
    p[0] = p[1:]

def p_Block(p):
    '''Block : '{' LocalVariableDeclarationsAndStatements '}'
    | '{' '}'
    '''
    if(len(list(p)) == 4):
        p[0] = p[2]
    else:
        p[0] = Node("Block",children=[Node(p[1]),Node(p[2])])

def p_LocalVariableDeclarationsAndStatements(p):
    '''LocalVariableDeclarationsAndStatements : LocalVariableDeclarationOrStatement
    | LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = Node("LocalVariableDeclarationsAndStatements",children=[p[1],p[2]])

def p_LocalVariableDeclarationOrStatement(p):
    '''LocalVariableDeclarationOrStatement : LocalVariableDeclarationStatement
    | Statement
    '''
    p[0] = p[1]

def p_LocalVariableDeclarationStatement(p):
    '''LocalVariableDeclarationStatement : TypeSpecifier VariableDeclarators ';'
    '''
    for variable in p[2].children:
        if variable in symbol_table:
            symbol_table[variable]['modifiers'] = None
            # symbol_table[variable]['value'] = 6
            symbol_table[variable]['valid'] = True
            symbol_table[variable]['dtype'] = ((p[1].children)[0]).name
    p[0] = Node("LocalVariableDeclarationStatement",children=[p[1],p[2]])

def p_Statement(p):
    '''Statement : EmptyStatement
    | ExpressionStatement ';'
    | IterationStatement
    | JumpStatement
    | Block
    '''
    p[0] = p[1]

def p_EmptyStatement(p):
    '''EmptyStatement : ';'
    '''
    p[0] = p[1]

def p_ExpressionStatement(p):
    '''ExpressionStatement : Expression
    '''
    p[0] = p[1]

def p_IterationStatement(p):
    '''IterationStatement : DO Statement WHILE '(' Expression ')' ';'
    | FOR '(' ForInit ForExpr ForIncr ')' Statement
    | FOR '(' ForInit ForExpr         ')' Statement
    '''
    if(p[1] == "do"):
        p[0] = Node("DO-WHILE",children=[p[5],p[2]])
    elif(p[1] == "for"):
        p[0] = Node("FOR",children=[p[3],p[4],p[5],p[7]])
        

def p_ForInit(p):
    '''ForInit : ExpressionStatements ';'
    | LocalVariableDeclarationStatement
    | ';'
    '''
    if(len(list(p)) == 3):
        p[0] = p[1]
    else:
        if(isinstance(p[1],Node)):
            p[0] = p[1]
        else:
            p[0] = Node(p[1])
def p_ForExpr(p):
    '''ForExpr : Expression ';'
    | ';'
    '''
    if(len(list(p)) == 3):
        p[0] = p[1]
    else:
        p[0] = Node(p[1])


def p_ForIncr(p):
    '''ForIncr : ExpressionStatements '''
    p[0] = p[1]

def p_ExpressionStatements(p):
    '''ExpressionStatements : ExpressionStatement
    | ExpressionStatements ',' ExpressionStatement
    '''
    if(len(list(p))==2):
        p[0] = p[1]
    else:
        p[0] = Node("ExpressionStatements",children=[p[1],p[3]])

def p_JumpStatement(p):
    '''JumpStatement : BREAK IDENTIFIER ';'
    | BREAK            ';'
    | CONTINUE IDENTIFIER ';'
    | CONTINUE            ';'
    | RETURN Expression ';'
    | RETURN            ';'
    '''
    if(len(list(p)) == 3):
        p[0] = Node(p[1],children=[Node([2])])
    else:
        p[0] = Node(p[1])

def p_PrimaryExpression(p):
    '''PrimaryExpression : QualifiedName
    | NotJustName
    '''
    p[0] = p[1]

def p_NotJustName(p):
    '''NotJustName : SpecialName
    | NewAllocationExpression
    | ComplexPrimary
    '''
    p[0] = p[1]

def p_ComplexPrimary(p):
    '''ComplexPrimary : '(' Expression ')'
    | ComplexPrimaryNoParenthesis
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_ComplexPrimaryNoParenthesis(p):
    '''ComplexPrimaryNoParenthesis : LITERAL
    | NUM_LITERAL
    | BOOLLIT
    | ArrayAccess
    | FieldAccess
    | MethodCall
    '''
    term = p[1]
    if term == "true":
        p[0] = Node("True")
    elif term == "false":
        p[0] = Node("False")
    elif type(term) == str or type(term) == int or type(term) == float:
        p[0] = Node(p[1])
    else:
        p[0] = p[1]



def p_ArrayAccess(p):
    '''ArrayAccess : QualifiedName '[' Expression ']'
    | ComplexPrimary '[' Expression ']'
    '''
    p[0] = Node("[]",children=[p[1],p[3]])

def p_FieldAccess(p):
    '''FieldAccess : NotJustName '.' IDENTIFIER
    | RealPostfixExpression '.' IDENTIFIER
        | QualifiedName '.' THIS
        | QualifiedName '.' CLASS
        | PrimitiveType '.' CLASS
    '''
    p[0] = p[1:]

def p_MethodCall(p):
    '''MethodCall : MethodAccess '(' ArgumentList ')'
    | MethodAccess '(' ')'
    '''
    if(len(list(p)) == 5):
        p[0] = Node("MethodCall",children=[p[1],p[3]])
        # p[0] = Node(p[1:])

def p_MethodAccess(p):
    '''MethodAccess : ComplexPrimaryNoParenthesis
    | SpecialName
    | QualifiedName
    '''
    p[0] = p[1]

def p_SpecialName(p):
    '''SpecialName : THIS
    '''
    p[0] = Node(p[1])

def p_ArgumentList(p):
    '''ArgumentList : Expression
    | ArgumentList ',' Expression
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = Node("ArgList",children = [p[1],p[3]])

def p_NewAllocationExpression(p):
    '''NewAllocationExpression : PlainNewAllocationExpression
        | QualifiedName '.' PlainNewAllocationExpression
    '''
    p[0] = p[1:]

def p_PlainNewAllocationExpression(p):
    '''PlainNewAllocationExpression : ArrayAllocationExpression
        | ClassAllocationExpression
        | ArrayAllocationExpression '{' '}'
        | ClassAllocationExpression '{' '}'
        | ArrayAllocationExpression '{' ArrayInitializers '}'
        | ClassAllocationExpression '{' FieldDeclarations '}'
    '''
    p[0] = p[1:]

def p_ClassAllocationExpression(p):
    '''ClassAllocationExpression : NEW TypeName '(' ArgumentList ')'
    | NEW TypeName '('              ')'
    '''
    p[0] = p[1:]

def p_ArrayAllocationExpression(p):
    '''ArrayAllocationExpression : NEW TypeName DimExprs Dims
    | NEW TypeName DimExprs
    | NEW TypeName Dims
    '''
    p[0] = p[1:]

def p_DimExprs(p):
    '''DimExprs : DimExpr
    | DimExprs DimExpr
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = Node("DimExprs",children=[p[1],p[2]])

def p_DimExpr(p):
    '''DimExpr : '[' Expression ']'
    '''
    p[0] = p[2]

def p_Dims(p):
    '''Dims : OP_DIM
    | Dims OP_DIM
    '''
    if(len(list(p)) == 2):
        p[0] = Node("Dims")
    else:
        p[0] = Node("Dims",children = p[2])

def p_PostfixExpression(p):
    '''PostfixExpression : PrimaryExpression
    | RealPostfixExpression
    '''
    p[0] = p[1]

def p_RealPostfixExpression(p):
    '''RealPostfixExpression : PostfixExpression OP_INC
    | PostfixExpression OP_DEC
    '''
    p[0] = Node("+=",children=[p[1],Node("1")])
    # variable = flatten(p[1])[0]
    # p[0] = retrieve(variable)
    # if p[2]=='++':
    #     p[0] += 1
    # elif p[2]=='--':
    #     p[0] -=1
    # symbol_table[variable]['value'] = p[0]

def p_UnaryExpression(p):
    '''UnaryExpression : OP_INC UnaryExpression
    | OP_DEC UnaryExpression
    | ArithmeticUnaryOperator CastExpression
    | LogicalUnaryExpression
    '''
    if len(list(p))==3:
        variable = flatten(p[2])[0]
        p[0] = retrieve(variable)
        if variable in symbol_table:
            if p[1]=='++':
                p[0] += 1
            elif p[1]=='--':
                p[0] -= 1
            symbol_table[variable]['value'] = p[0]
        elif p[1] == '-':
            p[0] = -flatten(p[2])[0]
    else:
        p[0] = p[1]



def p_LogicalUnaryExpression(p):
    '''LogicalUnaryExpression : PostfixExpression
    | LogicalUnaryOperator UnaryExpression
    '''
    if len(list(p)) == 3:
        operator = p[1]
        if operator == "!":
            p[0] = not(p[2].name)
        elif operator == "~":
            p[0] = ~(p[2].name)
    else:
        p[0] = p[1]

def p_LogicalUnaryOperator(p):
    '''LogicalUnaryOperator : '~'
    | '!'
    '''
    p[0] = p[1]

def p_ArithmeticUnaryOperator(p):
    '''ArithmeticUnaryOperator : '+'
    | '-'
    '''
    p[0] = p[1]

def p_CastExpression(p):
    '''CastExpression : UnaryExpression
    | '(' PrimitiveTypeExpression ')' CastExpression
    | '(' ClassTypeExpression ')' CastExpression
    | '(' Expression ')' LogicalUnaryExpression
    '''
    p[0] = p[1]

def p_PrimitiveTypeExpression(p):
    '''PrimitiveTypeExpression : PrimitiveType
    | PrimitiveType Dims
    '''
    p[0] = p[1:]

def p_ClassTypeExpression(p):
    '''ClassTypeExpression : QualifiedName Dims
    '''
    p[0] = p[1:]

def p_MultiplicativeExpression(p):
    '''MultiplicativeExpression : CastExpression
    | MultiplicativeExpression '*' CastExpression
    | MultiplicativeExpression '/' CastExpression
    | MultiplicativeExpression '%' CastExpression
    '''
    if len(list(p))==4:
        t1 = retrieve(p[1].name)
        t2 = retrieve(p[3].name)
        if p[2]=='*':
            p[0] = Node("*",children=[p[1],p[3]])
        elif p[2]=='/':
            p[0] = Node("/",children=[p[1],p[3]])
        elif p[2]=='%':
            p[0] = Node("%",children=[p[1],p[3]])
    else:
        p[0] = p[1]

# Need to add string case.
def p_AdditiveExpression(p):
    '''AdditiveExpression : MultiplicativeExpression
    | AdditiveExpression '+' MultiplicativeExpression
    | AdditiveExpression '-' MultiplicativeExpression
    '''
    if len(list(p))==4:
        t1 = retrieve(p[1].name)
        t2 = retrieve(p[3].name)
        if p[2]=='+':
            p[0] = Node("+",children=[p[1],p[3]])
        elif p[2]=='-':
            p[0] = Node("-",children=[p[1],p[3]])
    else:
        p[0] = p[1]

def p_RelationalExpression(p):
    '''RelationalExpression : AdditiveExpression
    | RelationalExpression '<' AdditiveExpression
    | RelationalExpression '>' AdditiveExpression
    | RelationalExpression OP_LE AdditiveExpression
    | RelationalExpression OP_GE AdditiveExpression
    '''
    if len(list(p))==4:
        # t1 = retrieve(flatten(p[1])[0])
        # t2 = retrieve(flatten(p[3])[0])

        if p[2]=='<':
            p[0] = Node("<",children=[p[1],p[3]])
        elif p[2]=='>':
            p[0] = Node(">",children=[p[1],p[3]])
        elif p[2]=='<=':
            p[0] = Node("<=",children=[p[1],p[3]])
        elif p[2]=='>=':
            p[0] = Node(">=",children=[p[1],p[3]])
    else:
        p[0] = p[1]

def p_EqualityExpression(p):
    '''EqualityExpression : RelationalExpression
        | EqualityExpression OP_EQ RelationalExpression
        | EqualityExpression OP_NE RelationalExpression
    '''
    if len(list(p))==4:
        # t1 = retrieve(flatten(p[1])[0])
        # t2 = retrieve(flatten(p[3])[0])

        if p[2]=='==':
            p[0] = Node("==",children=[p[1],p[3]])
        elif p[2]=='!=':
            p[0] = Node("!=",children=[p[1],p[3]])
    else:
        p[0] = p[1]

def p_AndExpression(p):
    '''AndExpression : EqualityExpression
        | AndExpression '&' EqualityExpression
    '''
    if len(list(p))==4:
        # t1 = retrieve(flatten(p[1])[0])
        # t2 = retrieve(flatten(p[3])[0])
        p[0] = Node("&",children=[p[1],p[3]])

    else:
        p[0] = p[1]

def p_ExclusiveOrExpression(p):
    '''ExclusiveOrExpression : AndExpression
    | ExclusiveOrExpression '^' AndExpression
    '''
    if len(list(p))==4:
        # t1 = retrieve(flatten(p[1])[0])
        # t2 = retrieve(flatten(p[3])[0])
        p[0] = Node("^",children=[p[1],p[3]])

    else:
        p[0] = p[1]

def p_InclusiveOrExpression(p):
    '''InclusiveOrExpression : ExclusiveOrExpression
    | InclusiveOrExpression '|' ExclusiveOrExpression
    '''
    if len(list(p))==4:
        # t1 = retrieve(flatten(p[1])[0])
        # t2 = retrieve(flatten(p[3])[0])
        p[0] = Node("|",children=[p[1],p[3]])

    else:
        p[0] = p[1]

def p_ConditionalAndExpression(p):
    '''ConditionalAndExpression : InclusiveOrExpression
    | ConditionalAndExpression OP_LAND InclusiveOrExpression
    '''
    if len(list(p))==4:
        # t1 = retrieve(flatten(p[1])[0])
        # t2 = retrieve(flatten(p[3])[0])

        p[0] = Node("&&",children=[p[1],p[3]])
    else:
        p[0] = p[1]

def p_ConditionalOrExpression(p):
    '''ConditionalOrExpression : ConditionalAndExpression
    | ConditionalOrExpression OP_LOR ConditionalAndExpression
    '''
    if len(list(p))==4:
        # t1 = retrieve(flatten(p[1])[0])
        # t2 = retrieve(flatten(p[3])[0])

        p[0] = Node("||",children=[p[1],p[3]])
    else:
        p[0] = p[1]

def p_ConditionalExpression(p):
    '''ConditionalExpression : ConditionalOrExpression
    | ConditionalOrExpression '?' Expression ':' ConditionalExpression
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_AssignmentExpression(p):
    '''AssignmentExpression : ConditionalExpression
    | UnaryExpression AssignmentOperator AssignmentExpression
    '''
    if len(list(p)) == 4:
        variable = p[1].name
        p[0] = Node(p[2],children=[p[1],p[3]])
        # p[0] = retrieve(variable)
        # # rhs = retrieve((p[3].children)[0])
        # if p[2] == '=':
        #     p[0] = rhs
        # elif p[2][0]=='+=':
        #     p[0] += rhs
        # elif p[2][0]=='-=':
        #     p[0] -= rhs
        # elif p[2][0]=='*=':
        #     p[0] *= rhs
        # elif p[2][0]=='/=':
        #     p[0] /= rhs
        # elif p[2][0]=='%=':
        #     p[0] %= rhs
        # elif p[2][0]=='&=':
        #     p[0] &= rhs
        # elif p[2][0]=='|=':
        #     p[0] |= rhs
        # symbol_table[variable]['value'] = p[0]
    else:
        p[0] = p[1]

def p_AssignmentOperator(p):
    '''AssignmentOperator : '='
    | ASS_MUL
    | ASS_DIV
    | ASS_MOD
    | ASS_ADD
    | ASS_SUB
    | ASS_AND
    | ASS_OR
    '''
    p[0] = p[1]



def p_Expression(p):
    '''Expression : AssignmentExpression
    '''
    p[0] = p[1]


