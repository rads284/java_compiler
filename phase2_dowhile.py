# -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables.
# -----------------------------------------------------------------------------

def flatten(l):
    output = []
    def removeNestings(l):
        for i in l:
            if type(i) == list:
                removeNestings(i)
            else:
                output.append(i)
    removeNestings(l)
    return output

symbol_table = {}

tokens = [
"BOOLEAN", "BREAK", "BYTE",
"CHAR", "CLASS", "CONTINUE",
"DO", "DOUBLE",
"FLOAT", "IMPORT", "INT",
"LONG",
"NEW",
"PACKAGE", "PRIVATE", "PROTECTED", "PUBLIC",
"RETURN",
"SHORT", "STATIC",
"THIS",
"VOID",
"WHILE",
"OP_INC", "OP_DEC", "OP_DIM",
"OP_GE", "OP_LE", "OP_EQ", "OP_NE",
"OP_LAND", "OP_LOR",
"ASS_MUL", "ASS_DIV", "ASS_MOD", "ASS_ADD", "ASS_SUB", "ASS_OR", "ASS_AND",
"IDENTIFIER", "LITERAL", "BOOLLIT", "NUM_LITERAL"
]

keywords = ["boolean", "break", "byte",
"char", "class", "continue",
"do", "double",
"else",
"final", "finally", "float", "for",
"if", "import", "int",
"long",
"new",
"package", "private", "protected", "public",
"return",
"short", "static",
"this",
"var", "void",
"while"]
for kw in keywords:
    symbol_table[kw] = {}
    symbol_table[kw]['type'] = "keyword"
# Tokens
# t_OP_INC = r"\+\+"
def t_OP_INC(t) :
 r"\+\+"
 t.type = "OP_INC"
 return t

# t_OP_DEC = r"--"
def t_OP_DEC(t) :
 r"--"
 t.type = "OP_DEC"
 return t

# t_OP_GE = r">="
def t_OP_GE(t) :
 r">="
 t.type = "OP_GE"
 return t

# t_OP_LE = r"<="
def t_OP_LE(t) :
 r"<="
 t.type = "OP_LE"
 return t

# t_OP_EQ = r"=="
def t_OP_EQ(t) :
 r"=="
 t.type = "OP_EQ"
 return t

# t_OP_NE = r"!="
def t_OP_NE(t) :
 r"!="
 t.type = "OP_NE"
 return t

# t_OP_LAND = r"&&"
def t_OP_LAND(t) :
 r"&&"
 t.type = "OP_LAND"
 return t

# t_OP_LOR = r"\|\|"
def t_OP_LOR(t) :
 r"\|\|"
 t.type = "OP_LOR"
 return t

# t_ASS_MUL = r"\*="
def t_ASS_MUL(t) :
 r"\*="
 t.type = "ASS_MUL"
 return t

# t_ASS_DIV = r"/="
def t_ASS_DIV(t) :
 r"/="
 t.type = "ASS_DIV"
 return t

# t_ASS_MOD = r"%="
def t_ASS_MOD(t) :
 r"%="
 t.type = "ASS_MOD"
 return t

# t_ASS_ADD = r"\+="
def t_ASS_ADD(t) :
 r"\+="
 t.type = "ASS_ADD"
 return t

# t_ASS_SUB = r"-="
def t_ASS_SUB(t) :
 r"-="
 t.type = "ASS_SUB"
 return t

t_LITERAL  = r'\"(\\.|[^\\"])*\"'
# t_COMMENT = r'\/\*\*(\\.|[^\\"])*\*\/'
def t_COMMENT(t) :
    r'\/\*\*(\\.|[^\\"])*\*\/'

# t_BOOLLIT  = r'"true"|"false"'
def t_BOOLLIT(t) :
 r'"true"|"false"'
 t.type = "BOOLLIT"
 return t

literals = [';', '.', ',', '+', '-', '*', '/',
            '%','<', '>', '!', '&', '|',
            '(', ')', '{', '}', '=']
def t_IDENTIFIER(t):
    r"[A-Za-z$_][A-Za-z$_0-9]*"
    if t.value in symbol_table:
        if("type" in symbol_table[t.value]):
            if(symbol_table[t.value]["type"] == "keyword"):
                t.type = t.value.upper()
                symbol_table[t.value]["token"] = t
    else:
        symbol_table[t.value] = {}
        symbol_table[t.value]["type"] = "identifier"
        symbol_table[t.value]["token"] = t
        symbol_table[t.value]["valid"] = False
    return t

def t_semicolon(t):
    r';'
    t.type = ';'
    return t

def t_fullstop(t):
    r'\.'
    t.type = '.'      # Set token type to the expected literal
    return t

def t_comma(t):
    r','
    t.type = ','      # Set token type to the expected literal
    return t
def t_op_plus(t):
    r'\+'
    t.type = '+'      # Set token type to the expected literal
    return t
def t_op_minus(t):
    r'-'
    t.type = '-'      # Set token type to the expected literal
    return t
def t_op_multiply(t):
    r'\*'
    t.type = '*'      # Set token type to the expected literal
    return t
def t_divide(t):
    r'/'
    t.type = '/'      # Set token type to the expected literal
    return t
def t_op_modulus(t):
    r'%'
    t.type = '%'      # Set token type to the expected literal
    return t
def t_op_lt(t):
    r'<'
    t.type = '<'      # Set token type to the expected literal
    return t
def t_op_gt(t):
    r'>'
    t.type = '>'      # Set token type to the expected literal
    return t
def t_op_not(t):
    r'!'
    t.type = '!'      # Set token type to the expected literal
    return t
def t_op_and(t):
    r'&'
    t.type = '&'      # Set token type to the expected literal
    return t
def t_op_or(t):
    r'\|'
    t.type = '|'      # Set token type to the expected literal
    return t
def t_lparen(t):
    r'\('
    t.type = '('      # Set token type to the expected literal
    return t
def t_rparen(t):
    r'\)'
    t.type = ')'      # Set token type to the expected literal
    return t
def t_lbrace(t):
    r'{'
    t.type = '{'      # Set token type to the expected literal
    return t
def t_rbrace(t):
    r'}'
    t.type = '}'      # Set token type to the expected literal
    return t
def t_ass(t):
    r'='
    t.type = '='      # Set token type to the expected literal
    return t

def t_NUM_LITERAL(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Ignored characters
t_OP_DIM = r"\[{ |\t}*\]"
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Build the lexer
import sys
import ply.lex as lex
import ply.yacc as yacc
lex.lex(debug=0)

# Precedence rules for the arithmetic operators
# precedence = (
#     ('left','PLUS','MINUS'),
#     ('left','TIMES','DIVIDE'),
#     ('right','UMINUS'),
#     )

# dictionary of names (for storing variables)
names = { }

start = 'CompilationUnit'

# def p_OP_DIM(p):
#     'OP_DIM :'
#     pass

def p_error(p):

    # get formatted representation of stack
    stack_state_str = ' '.join([symbol.type for symbol in parser.symstack][1:])

    print('Syntax error in input! Parser State:{} {} . {}'
          .format(parser.state,
                  stack_state_str,
                  p))

def p_TypeSpecifier(p):
    '''TypeSpecifier : TypeName
    | TypeName Dims
    '''
    p[0] = p[1:]

def p_TypeName(p):
    '''TypeName : PrimitiveType
    | QualifiedName
    '''
    p[0] = p[1:]

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
    p[0] = p[1:]

def p_SemiColons(p):
    '''SemiColons : ';'
    | SemiColons ';'
    '''
    p[0] = p[1:]

def p_CompilationUnit(p):
    '''CompilationUnit : ProgramFile
    '''
    p[0] = p[1:]

def p_ProgramFile(p):
    '''ProgramFile : PackageStatement ImportStatements TypeDeclarations
    | PackageStatement ImportStatements
    | PackageStatement                  TypeDeclarations
    |                  ImportStatements TypeDeclarations
    | PackageStatement
    |                  ImportStatements
    |                                   TypeDeclarations
    '''
    p[0] = p[1:]

def p_PackageStatement(p):
    '''PackageStatement : PACKAGE QualifiedName SemiColons
    '''
    p[0] = p[1:]

def p_TypeDeclarations(p):
    '''TypeDeclarations : TypeDeclarationOptSemi
    | TypeDeclarations TypeDeclarationOptSemi
    '''
    p[0] = p[1:]

def p_TypeDeclarationOptSemi(p):
    '''TypeDeclarationOptSemi : TypeDeclaration
        | TypeDeclaration SemiColons
    '''
    p[0] = p[1:]

def p_ImportStatements(p):
    '''ImportStatements : ImportStatement
    | ImportStatements ImportStatement
    '''
    p[0] = p[1:]

def p_ImportStatement(p):
    '''ImportStatement : IMPORT QualifiedName SemiColons
    | IMPORT QualifiedName '.' '*' SemiColons
    '''
    p[0] = p[1:]

def p_QualifiedName(p):
    '''QualifiedName : IDENTIFIER
    | QualifiedName '.' IDENTIFIER
    '''
    p[0] = p[1:]

def p_TypeDeclaration(p):
    '''TypeDeclaration : ClassHeader '{' FieldDeclarations '}'
    | ClassHeader '{' '}'
    '''
    p[0] = p[1:]

def p_ClassHeader(p):
    '''ClassHeader : Modifiers ClassWord IDENTIFIER
    |           ClassWord IDENTIFIER
    '''
    if len(list(p))==4:
        symbol_table[p[3]]['modifiers'] = flatten(p[1])
    else:
        symbol_table[p[3]]['modifiers'] = None
    symbol_table[flatten(list(p))[-1]]["valid"] = True
    p[0] = p[1:]

def p_Modifiers(p):
    '''Modifiers : Modifier
    | Modifiers Modifier
    '''
    p[0] = p[1:]

def p_Modifier(p):
    '''Modifier : PUBLIC
    | PROTECTED
    | PRIVATE
    | STATIC
    '''
    p[0] = p[1:]

def p_ClassWord(p):
    '''ClassWord : CLASS'''
    p[0] = p[1:]

def p_FieldDeclarations(p):
    '''FieldDeclarations : FieldDeclarationOptSemi
        | FieldDeclarations FieldDeclarationOptSemi
    '''
    p[0] = p[1:]

def p_FieldDeclarationOptSemi(p):
    '''FieldDeclarationOptSemi : FieldDeclaration
        | FieldDeclaration SemiColons
    '''
    p[0] = p[1:]

def p_FieldDeclaration(p):
    '''FieldDeclaration : FieldVariableDeclaration ';'
    | MethodDeclaration
    | ConstructorDeclaration
    | StaticInitializer
        | NonStaticInitializer
        | TypeDeclaration
    '''
    p[0] = p[1:]

def p_FieldVariableDeclaration(p):
    '''FieldVariableDeclaration : Modifiers TypeSpecifier VariableDeclarators
    |           TypeSpecifier VariableDeclarators
    '''
    if len(list(p))==4:
        for variable in flatten(list(p[3])):
            print(flatten(list(p[3])))
            if variable in symbol_table:
                symbol_table[variable]['modifiers'] = flatten(p[1])
                symbol_table[variable]['value'] = 'None'
                symbol_table[variable]["valid"] = True
                symbol_table[variable]['dtype'] = flatten(list(p[2]))[0]
    else:
        for variable in flatten(list(p[2])):
            print(flatten(list(p[2])))
            if variable in symbol_table:
                symbol_table[variable]['modifiers'] = None
                symbol_table[variable]['value'] = 'None'
                symbol_table[variable]["valid"] = True
                symbol_table[variable]['dtype'] = flatten(list(p[1]))[0]

    p[0] = p[1:]

def p_VariableDeclarators(p):
    '''VariableDeclarators : VariableDeclarator
    | VariableDeclarators ',' VariableDeclarator
    '''
    # x = p[0][0][0]
    p[0] = p[1:]

def p_VariableDeclarator(p):
    '''VariableDeclarator : DeclaratorName
    | DeclaratorName '=' VariableInitializer
    '''
    p[0] = p[1:]


    print(p[1:])

def p_VariableInitializer(p):
    '''VariableInitializer : Expression
    | '{' '}'
        | '{' ArrayInitializers '}'
    '''
    p[0] = p[1:]

def p_ArrayInitializers(p):
    '''ArrayInitializers : VariableInitializer
    | ArrayInitializers ',' VariableInitializer
    | ArrayInitializers ','
    '''
    p[0] = p[1:]

def p_MethodDeclaration(p):
    '''MethodDeclaration : Modifiers TypeSpecifier MethodDeclarator        MethodBody
    |           TypeSpecifier MethodDeclarator        MethodBody
    '''
    if len(list(p)) == 5:
        symbol_table[flatten(list(p[3]))[0]]['modifiers'] = flatten(p[1])
        symbol_table[flatten(list(p[3]))[0]]["valid"] = True
    else:
        symbol_table[flatten(list(p[2]))[0]]['modifiers'] = None
        symbol_table[flatten(list(p[2]))[0]]["valid"] = True
    p[0] = p[1:]

def p_MethodDeclarator(p):
    '''MethodDeclarator : DeclaratorName '(' ParameterList ')'
    | DeclaratorName '(' ')'
    | MethodDeclarator OP_DIM
    '''
    symbol_table[flatten(list(p[1]))[0]]['dtype'] = flatten(list(p[-1]))[0]
    p[0] = p[1:]

def p_ParameterList(p):
    '''ParameterList : Parameter
    | ParameterList ',' Parameter
    '''
    p[0] = p[1:]

def p_Parameter(p):
    '''Parameter : TypeSpecifier DeclaratorName
    '''
    p[0] = p[1:]

def p_DeclaratorName(p):
    '''DeclaratorName : IDENTIFIER
    | DeclaratorName OP_DIM
    '''

    p[0] = p[1:]


def p_MethodBody(p):
    '''MethodBody : Block
    | ';'
    '''
    p[0] = p[1:]

def p_ConstructorDeclaration(p):
    '''ConstructorDeclaration : Modifiers ConstructorDeclarator        Block
    |           ConstructorDeclarator        Block
    '''
    p[0] = p[1:]

def p_ConstructorDeclarator(p):
    '''ConstructorDeclarator : IDENTIFIER '(' ParameterList ')'
    | IDENTIFIER '(' ')'
    '''
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
    p[0] = p[1:]

def p_LocalVariableDeclarationsAndStatements(p):
    '''LocalVariableDeclarationsAndStatements : LocalVariableDeclarationOrStatement
    | LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement
    '''
    p[0] = p[1:]

def p_LocalVariableDeclarationOrStatement(p):
    '''LocalVariableDeclarationOrStatement : LocalVariableDeclarationStatement
    | Statement
    '''
    p[0] = p[1:]

def p_LocalVariableDeclarationStatement(p):
    '''LocalVariableDeclarationStatement : TypeSpecifier VariableDeclarators ';'
    '''
    for variable in flatten(list(p[2])):
        if variable in symbol_table:
            symbol_table[variable]['modifiers'] = None
            symbol_table[variable]['value'] = None
            symbol_table[variable]['valid'] = True
            symbol_table[variable]['dtype'] = flatten(list(p[1]))[0]
    x = p[1][0][0][0]
    p[0] = p[1:]

def p_Statement(p):
    '''Statement : EmptyStatement
    | ExpressionStatement ';'
        | IterationStatement
    | JumpStatement
    | Block
    '''
    p[0] = p[1:]

def p_EmptyStatement(p):
    '''EmptyStatement : ';'
    '''
    p[0] = p[1:]

def p_ExpressionStatement(p):
    '''ExpressionStatement : Expression
    '''
    p[0] = p[1:]

def p_IterationStatement(p):
    '''IterationStatement : DO Statement WHILE '(' Expression ')' ';'
    '''
    p[0] = p[1:]

def p_JumpStatement(p):
    '''JumpStatement : BREAK IDENTIFIER ';'
    | BREAK            ';'
        | CONTINUE IDENTIFIER ';'
    | CONTINUE            ';'
    | RETURN Expression ';'
    | RETURN            ';'
    '''
    p[0] = p[1:]

def p_PrimaryExpression(p):
    '''PrimaryExpression : QualifiedName
    | NotJustName
    '''
    p[0] = p[1:]

def p_NotJustName(p):
    '''NotJustName : SpecialName
    | NewAllocationExpression
    | ComplexPrimary
    '''
    p[0] = p[1:]

def p_ComplexPrimary(p):
    '''ComplexPrimary : '(' Expression ')'
    | ComplexPrimaryNoParenthesis
    '''
    p[0] = p[1:]

def p_ComplexPrimaryNoParenthesis(p):
    '''ComplexPrimaryNoParenthesis : LITERAL
    | NUM_LITERAL
    | BOOLLIT
    | ArrayAccess
    | FieldAccess
    | MethodCall
    '''
    p[0] = p[1:]

def p_ArrayAccess(p):
    '''ArrayAccess : QualifiedName '[' Expression ']'
    | ComplexPrimary '[' Expression ']'
    '''
    p[0] = p[1:]

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
    p[0] = p[1:]

def p_MethodAccess(p):
    '''MethodAccess : ComplexPrimaryNoParenthesis
    | SpecialName
    | QualifiedName
    '''
    p[0] = p[1:]

def p_SpecialName(p):
    '''SpecialName : THIS
    '''
    p[0] = p[1:]

def p_ArgumentList(p):
    '''ArgumentList : Expression
    | ArgumentList ',' Expression
    '''
    p[0] = p[1:]

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
    p[0] = p[1:]

def p_DimExpr(p):
    '''DimExpr : '[' Expression ']'
    '''
    p[0] = p[1:]

def p_Dims(p):
    '''Dims : OP_DIM
    | Dims OP_DIM
    '''
    p[0] = p[1:]

def p_PostfixExpression(p):
    '''PostfixExpression : PrimaryExpression
    | RealPostfixExpression
    '''
    p[0] = p[1:]

def p_RealPostfixExpression(p):
    '''RealPostfixExpression : PostfixExpression OP_INC
    | PostfixExpression OP_DEC
    '''
    p[0] = p[1:]

def p_UnaryExpression(p):
    '''UnaryExpression : OP_INC UnaryExpression
    | OP_DEC UnaryExpression
    | ArithmeticUnaryOperator CastExpression
    | LogicalUnaryExpression
    '''
    p[0] = p[1:]

def p_LogicalUnaryExpression(p):
    '''LogicalUnaryExpression : PostfixExpression
    | LogicalUnaryOperator UnaryExpression
    '''
    p[0] = p[1:]

def p_LogicalUnaryOperator(p):
    '''LogicalUnaryOperator : '~'
    | '!'
    '''
    p[0] = p[1:]

def p_ArithmeticUnaryOperator(p):
    '''ArithmeticUnaryOperator : '+'
    | '-'
    '''
    p[0] = p[1:]

def p_CastExpression(p):
    '''CastExpression : UnaryExpression
    | '(' PrimitiveTypeExpression ')' CastExpression
    | '(' ClassTypeExpression ')' CastExpression
    | '(' Expression ')' LogicalUnaryExpression
    '''
    p[0] = p[1:]

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
    p[0] = p[1:]

def p_AdditiveExpression(p):
    '''AdditiveExpression : MultiplicativeExpression
        | AdditiveExpression '+' MultiplicativeExpression
    | AdditiveExpression '-' MultiplicativeExpression
    '''
    p[0] = p[1:]

def p_RelationalExpression(p):
    '''RelationalExpression : AdditiveExpression
    | RelationalExpression '<' AdditiveExpression
    | RelationalExpression '>' AdditiveExpression
    | RelationalExpression OP_LE AdditiveExpression
    | RelationalExpression OP_GE AdditiveExpression
    '''
    p[0] = p[1:]

def p_EqualityExpression(p):
    '''EqualityExpression : RelationalExpression
        | EqualityExpression OP_EQ RelationalExpression
        | EqualityExpression OP_NE RelationalExpression
    '''
    p[0] = p[1:]

def p_AndExpression(p):
    '''AndExpression : EqualityExpression
        | AndExpression '&' EqualityExpression
    '''
    p[0] = p[1:]

def p_ExclusiveOrExpression(p):
    '''ExclusiveOrExpression : AndExpression
    | ExclusiveOrExpression '^' AndExpression
    '''
    p[0] = p[1:]

def p_InclusiveOrExpression(p):
    '''InclusiveOrExpression : ExclusiveOrExpression
    | InclusiveOrExpression '|' ExclusiveOrExpression
    '''
    p[0] = p[1:]

def p_ConditionalAndExpression(p):
    '''ConditionalAndExpression : InclusiveOrExpression
    | ConditionalAndExpression OP_LAND InclusiveOrExpression
    '''
    p[0] = p[1:]

def p_ConditionalOrExpression(p):
    '''ConditionalOrExpression : ConditionalAndExpression
    | ConditionalOrExpression OP_LOR ConditionalAndExpression
    '''
    p[0] = p[1:]

def p_ConditionalExpression(p):
    '''ConditionalExpression : ConditionalOrExpression
    | ConditionalOrExpression '?' Expression ':' ConditionalExpression
    '''
    p[0] = p[1:]

def p_AssignmentExpression(p):
    '''AssignmentExpression : ConditionalExpression
    | UnaryExpression AssignmentOperator AssignmentExpression
    '''
    p[0] = p[1:]

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
    p[0] = p[1:]


def p_Expression(p):
    '''Expression : AssignmentExpression
    '''
    p[0] = p[1:]



parser = yacc.yacc(debug=0)

while True:
    try:
        # input_str = input('calc > ')   # Use raw_input on Python 2
        input_str = sys.stdin.read()
        # if __name__ == '__main__':
            # lex.runmain()
    except EOFError:
        break
    print("\n\n\n==========Tokens Generated============")
    lex.input(input_str)
     # Tokenize
    while True:
     tok = lex.token()
     if not tok:
         break      # No more input
     print(tok)

    x = parser.parse(input_str)
    print("\n\n\n============Parser Output============")
    print(x)
    # print('success')
    print("\n\n\n============Symbol Table=============")
    for symbol in symbol_table:
        if("token" in symbol_table[symbol]):
            print(symbol_table[symbol])

    # print(varg)
