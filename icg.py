from dowhile_lex import symbol_table,keywords
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


stack = []
label = []
var_num = 0
lab_num = -1

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

# def p_OP_DIM(p):
#     'OP_DIM :'
#     pass


def p_CodegenPrefix(p):
    '''codegen_prefix :'''
    global var_num
    cur_var = "t"+str(var_num)

    print(cur_var, ' = ' , flatten(p[-1])[0], flatten(p[-2])[0][0], 1 )
    print(flatten(p[-1])[0], ' = ', cur_var)
    
    var_num+=1

def p_CodegenPostfix(p):
    '''codegen_postfix :'''
    global var_num
    cur_var = "t"+str(var_num)

    print(cur_var, ' = ' , flatten(p[-2])[0], flatten(p[-1])[0][0], 1 )
    print(flatten(p[-2])[0], ' = ', cur_var)
    
    var_num+=1

def p_CodegenShorthand(p):
    '''codegen_shorthand :'''
    global var_num
    cur_var = "t"+str(var_num)

    print(cur_var, ' = ' , stack[-3], stack[-2][0], stack[-1] )
    print(stack[-3], ' = ', cur_var)
    stack.pop()
    stack.pop()
    stack.pop()
    
    var_num+=1

def p_CodegenDeclarator(p):
    '''codegen_declarator :'''
    print(stack[-2],' = ',stack[-1])
    stack.pop()
    stack.pop()

def p_Push(p):
    '''push :'''
    # print(stack,'ancd',p[-1],'abc',p[:])
    if len(stack) == 0 and flatten(p[-2])[0] not in symbol_table:
        stack.append(flatten(p[-2])[0])
    stack.append(flatten(p[-1])[0])

def p_CodegenBinop(p):
    '''codegen_binop :'''
    global var_num
    cur_var = "t"+str(var_num)
    print(cur_var," = ", stack[-3],stack[-2],stack[-1])
    stack.pop()
    stack.pop()
    stack.pop()
    stack.append(cur_var)
    var_num+=1

def p_CodegenAssign(p):
    '''codegen_assign :'''
    global var_num
    print(stack[-2],' = ',stack[-1])
    stack.pop()
    stack.pop()


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
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

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
    p[0] = p[1]

def p_SemiColons(p):
    '''SemiColons : ';'
    | SemiColons ';'
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_CompilationUnit(p):
    '''CompilationUnit : ProgramFile
    '''
    p[0] = p[1]

def p_ProgramFile(p):
    '''ProgramFile : PackageStatement ImportStatements TypeDeclarations
    | PackageStatement ImportStatements
    | PackageStatement                  TypeDeclarations
    |                  ImportStatements TypeDeclarations
    | PackageStatement
    |                  ImportStatements
    |                                   TypeDeclarations
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_PackageStatement(p):
    '''PackageStatement : PACKAGE QualifiedName SemiColons
    '''
    p[0] = p[1:]

def p_TypeDeclarations(p):
    '''TypeDeclarations : TypeDeclarationOptSemi
    | TypeDeclarations TypeDeclarationOptSemi
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
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
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
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
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
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
        symbol_table[p[2]]['modifiers'] = None
    symbol_table[flatten(list(p))[-1]]["valid"] = True
    symbol_table[flatten(list(p))[-1]]["dtype"] = "class"
    p[0] = p[1:]

def p_Modifiers(p):
    '''Modifiers : Modifier
    | Modifiers Modifier
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_Modifier(p):
    '''Modifier : PUBLIC
    | PROTECTED
    | PRIVATE
    | STATIC
    '''
    p[0] = p[1]

def p_ClassWord(p):
    '''ClassWord : CLASS'''
    p[0] = p[1]

def p_FieldDeclarations(p):
    '''FieldDeclarations : FieldDeclarationOptSemi
        | FieldDeclarations FieldDeclarationOptSemi
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_FieldDeclarationOptSemi(p):
    '''FieldDeclarationOptSemi : FieldDeclaration
        | FieldDeclaration SemiColons
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_FieldDeclaration(p):
    '''FieldDeclaration : FieldVariableDeclaration ';'
    | MethodDeclaration
    | ConstructorDeclaration
    | StaticInitializer
    | NonStaticInitializer
    | TypeDeclaration
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_FieldVariableDeclaration(p):
    '''FieldVariableDeclaration : Modifiers TypeSpecifier VariableDeclarators
    |           TypeSpecifier VariableDeclarators
    '''
    if len(list(p))==4:
        for variable in flatten(p[3]):
            if variable in symbol_table:
                symbol_table[variable]['modifiers'] = flatten(p[1])
                symbol_table[variable]['value'] = 'None'
                symbol_table[variable]["valid"] = True
                symbol_table[variable]['dtype'] = flatten(p[2])[0]
                # symbol_table[(variable,True)]['global'] = True
    else:
        for variable in flatten(p[2]):
            if variable in symbol_table:
                symbol_table[variable]['modifiers'] = None
                symbol_table[variable]['value'] = 'None'
                symbol_table[variable]["valid"] = True
                symbol_table[variable]['dtype'] = flatten(p[1])[0]
                # symbol_table[(variable,True)]['global'] = True

    p[0] = p[1:]

def p_VariableDeclarators(p):
    '''VariableDeclarators : VariableDeclarator
    | VariableDeclarators ',' VariableDeclarator
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_VariableDeclarator(p):
    '''VariableDeclarator : DeclaratorName
    | DeclaratorName push '=' VariableInitializer codegen_declarator
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:2]+p[3:5]
    if len(list(p))==5:
        variable = flatten(p[1])[0]
        symbol_table[variable]['value'] = flatten(p[3])[0]#int

def p_VariableInitializer(p):
    '''VariableInitializer : Expression
    | '{' '}'
        | '{' ArrayInitializers '}'
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_ArrayInitializers(p):
    '''ArrayInitializers : VariableInitializer
    | ArrayInitializers ',' VariableInitializer
    | ArrayInitializers ','
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_MethodDeclaration(p):
    '''MethodDeclaration : Modifiers TypeSpecifier MethodDeclarator        MethodBody
    |           TypeSpecifier MethodDeclarator        MethodBody
    '''
    if len(list(p)) == 5:
        method = flatten(p[3])[0]
        symbol_table[method]['modifiers'] = flatten(p[1])
        symbol_table[method]["valid"] = True
        symbol_table[method]['dtype'] = flatten(p[2])[0]
    else:
        method =flatten(p[2])[0]
        symbol_table[method]['modifiers'] = None
        symbol_table[method]["valid"] = True
        symbol_table[method]['dtype'] = flatten(p[1])[0]
    p[0] = p[1:]

def p_MethodDeclarator(p):
    '''MethodDeclarator : DeclaratorName '(' ParameterList ')'
    | DeclaratorName '(' ')'
    | MethodDeclarator OP_DIM
    '''
    if len(list(p)) == 5:
        method = flatten(p[1])[0]
        symbol_table[method]['params'] = []
        for param in flatten(p[3]):
            if param in symbol_table and param not in symbol_table[method]['params']:
                if symbol_table[param]['type'] =='identifier':
                    symbol_table[method]['params'].append(param)
    p[0] = p[1:]

def p_ParameterList(p):
    '''ParameterList : Parameter
    | ParameterList ',' Parameter
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_Parameter(p):
    '''Parameter : TypeSpecifier DeclaratorName
    '''
    variable = flatten(p[2])[0]
    symbol_table[variable]['dtype'] = flatten(p[1])[0]
    symbol_table[variable]['valid'] = True
    p[0] = p[1:]

def p_DeclaratorName(p):
    '''DeclaratorName : IDENTIFIER
    | DeclaratorName OP_DIM
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]


def p_MethodBody(p):
    '''MethodBody : Block
    | ';'
    '''
    p[0] = p[1]

def p_ConstructorDeclaration(p):
    '''ConstructorDeclaration : Modifiers ConstructorDeclarator        Block
    |           ConstructorDeclarator        Block
    '''
    if len(list(p)) == 4:
        method = flatten(p[2])[0]
        symbol_table[method]['modifiers'] = flatten(p[1])
        symbol_table[method]["valid"] = True
    else:
        method =flatten(p[1])[0]
        symbol_table[method]['modifiers'] = None
        symbol_table[method]["valid"] = True
    p[0] = p[1:]

def p_ConstructorDeclarator(p):
    '''ConstructorDeclarator : IDENTIFIER '(' ParameterList ')'
    | IDENTIFIER '(' ')'
    '''
    if len(list(p)) == 5:
        method = flatten(p[1])[0]
        symbol_table[method]['params'] = []
        for param in flatten(p[3]):
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
    p[0] = p[1]

def p_Block(p):
    '''Block : '{' LocalVariableDeclarationsAndStatements '}'
    | '{' '}'
    '''
    p[0] = p[1:]

def p_LocalVariableDeclarationsAndStatements(p):
    '''LocalVariableDeclarationsAndStatements : LocalVariableDeclarationOrStatement
    | LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_LocalVariableDeclarationOrStatement(p):
    '''LocalVariableDeclarationOrStatement : LocalVariableDeclarationStatement
    | Statement
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_LocalVariableDeclarationStatement(p):
    '''LocalVariableDeclarationStatement : TypeSpecifier VariableDeclarators ';'
    '''
    for variable in flatten(p[2]):
        if variable in symbol_table:
            symbol_table[variable]['modifiers'] = None
            # symbol_table[variable]['value'] = 6
            symbol_table[variable]['valid'] = True
            symbol_table[variable]['dtype'] = flatten(p[1])[0]
    p[0] = p[1:]

def p_Statement(p):
    '''Statement : EmptyStatement
    | ExpressionStatement ';'
    | IterationStatement
    | JumpStatement
    | Block
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

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
    | FOR '(' ForInit codegen_for_init ForExpr codegen_for_expr ForIncr codegen_for_inc ')' Statement codegen_for
    | FOR '(' ForInit codegen_for_init ForExpr codegen_for_expr ')' Statement codegen_for
    '''
    p[0] = p[1:]

def p_CodegenForInit(p):
    '''codegen_for_init :'''
    global lab_num
    global label
    for i in range(4): 
        lab_num+=1
        label.append(lab_num)
    print("L"+str(label[-4]),':')

def p_CodegenForExpr(p):
    '''codegen_for_expr :'''
    global var_num
    global label
    temp = 't'+str(var_num)
    print(temp,' = not', stack[-1])
    print('if',temp,'goto L'+str(label[-3]))
    var_num+=1
    print('goto L'+str(label[-2]))
    print('L'+str(label[-1]),':')

def p_CodegenForInc(p):
    '''codegen_for_inc :'''
    global label
    global lab_num
    print('goto L'+str(label[-4]))
    print('L'+str(label[-2]),':')

def p_CodegenFor(p):
    '''codegen_for :'''
    global label
    global lab_num
    print('goto L'+str(label[-1]))
    print('L'+str(label[-3]),':')
    for i in range(4): label.pop()





    
def p_ForInit(p):
    '''ForInit : ExpressionStatements ';'
    | LocalVariableDeclarationStatement
    | ';'
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_ForExpr(p):
    '''ForExpr : Expression ';'
    | ';'
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_ForIncr(p):
    '''ForIncr : ExpressionStatements '''
    p[0] = p[1]

def p_ExpressionStatements(p):
    '''ExpressionStatements : ExpressionStatement
    | ExpressionStatements ',' ExpressionStatement 
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
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
        p[0] = p[1:]

def p_ComplexPrimaryNoParenthesis(p):
    '''ComplexPrimaryNoParenthesis : LITERAL
    | NUM_LITERAL
    | BOOLLIT
    | ArrayAccess
    | FieldAccess
    | MethodCall
    '''
    term = flatten(p[1])[0]
    if term == "true":
        p[0] = True
    elif term == "false":
        p[0] = False
    elif type(term) == str or type(term) == int or type(term) == float:
        p[0] = p[1]
    else:
        p[0] = p[1]

    

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
    p[0] = p[1]

def p_SpecialName(p):
    '''SpecialName : THIS
    '''
    p[0] = p[1]

def p_ArgumentList(p):
    '''ArgumentList : Expression
    | ArgumentList ',' Expression
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_NewAllocationExpression(p):
    '''NewAllocationExpression : PlainNewAllocationExpression
        | QualifiedName '.' PlainNewAllocationExpression
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_PlainNewAllocationExpression(p):
    '''PlainNewAllocationExpression : ArrayAllocationExpression
        | ClassAllocationExpression
        | ArrayAllocationExpression '{' '}'
        | ClassAllocationExpression '{' '}'
        | ArrayAllocationExpression '{' ArrayInitializers '}'
        | ClassAllocationExpression '{' FieldDeclarations '}'
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
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
        p[0] = p[1:]

def p_DimExpr(p):
    '''DimExpr : '[' Expression ']'
    '''
    p[0] = p[1:]

def p_Dims(p):
    '''Dims : OP_DIM
    | Dims OP_DIM
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_PostfixExpression(p):
    '''PostfixExpression : PrimaryExpression
    | RealPostfixExpression
    '''
    p[0] = p[1]

def p_RealPostfixExpression(p):
    '''RealPostfixExpression : PostfixExpression OP_INC codegen_postfix
    | PostfixExpression OP_DEC codegen_postfix
    '''
    p[0] = p[1]

def p_UnaryExpression(p):
    '''UnaryExpression : OP_INC UnaryExpression codegen_prefix
    | OP_DEC UnaryExpression codegen_prefix
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
    elif(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]



def p_LogicalUnaryExpression(p):
    '''LogicalUnaryExpression : PostfixExpression
    | LogicalUnaryOperator UnaryExpression
    '''
    if len(list(p)) == 3:
        operator = p[1]
        if operator == "!":
            p[0] = not(flatten(p[2])[0])
        elif operator == "~":
            p[0] = ~(flatten(p[2])[0])
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
    '''CastExpression : UnaryExpression push
    | '(' PrimitiveTypeExpression ')' CastExpression
    | '(' ClassTypeExpression ')' CastExpression
    | '(' Expression ')' LogicalUnaryExpression
    '''
    if(len(list(p)) == 3):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_PrimitiveTypeExpression(p):
    '''PrimitiveTypeExpression : PrimitiveType
        | PrimitiveType Dims
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]

def p_ClassTypeExpression(p):
    '''ClassTypeExpression : QualifiedName Dims
    '''
    p[0] = p[1:]

def p_MultiplicativeExpression(p):
    '''MultiplicativeExpression : CastExpression 
    | MultiplicativeExpression '*' push  CastExpression codegen_binop
    | MultiplicativeExpression '/' push CastExpression codegen_binop
    | MultiplicativeExpression '%' push CastExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

# Need to add string case.
def p_AdditiveExpression(p):
    '''AdditiveExpression : MultiplicativeExpression
    | AdditiveExpression '+' push MultiplicativeExpression codegen_binop
    | AdditiveExpression '-' push MultiplicativeExpression codegen_binop
    '''
    #search
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_RelationalExpression(p):
    '''RelationalExpression : AdditiveExpression
    | RelationalExpression '<' push AdditiveExpression codegen_binop
    | RelationalExpression '>' push AdditiveExpression codegen_binop
    | RelationalExpression OP_LE push AdditiveExpression codegen_binop
    | RelationalExpression OP_GE push AdditiveExpression codegen_binop
    '''

    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_EqualityExpression(p):
    '''EqualityExpression : RelationalExpression
        | EqualityExpression OP_EQ push RelationalExpression codegen_binop
        | EqualityExpression OP_NE push RelationalExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_AndExpression(p):
    '''AndExpression : EqualityExpression
        | AndExpression '&' push EqualityExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_ExclusiveOrExpression(p):
    '''ExclusiveOrExpression : AndExpression
    | ExclusiveOrExpression '^' push AndExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_InclusiveOrExpression(p):
    '''InclusiveOrExpression : ExclusiveOrExpression
    | InclusiveOrExpression '|' push ExclusiveOrExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_ConditionalAndExpression(p):
    '''ConditionalAndExpression : InclusiveOrExpression
    | ConditionalAndExpression OP_LAND push InclusiveOrExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_ConditionalOrExpression(p):
    '''ConditionalOrExpression : ConditionalAndExpression
    | ConditionalOrExpression OP_LOR push ConditionalAndExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
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
    | UnaryExpression push '=' ConditionalExpression codegen_assign
    | UnaryExpression push  AssignmentOperator push AssignmentExpression codegen_shorthand
    '''
    if len(list(p)) == 6:
        p[0] = p[1:2]+p[3:5]
    elif len(list(p)) == 7:
        p[0] = p[1:2] + p[3:4] + p[5:6]
    else:
        p[0] = p[1]

def p_AssignmentOperator(p):
    '''AssignmentOperator : ASS_MUL
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