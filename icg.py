from dowhile_lex import symbol_table,keywords
from copy import deepcopy
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

def retrieve_assignscope(t):
    for a in reversed(stack):
        if (t,a) in symbol_table:
            if symbol_table[(t,a)]['valid']:
                return a
    return 0

def retrieve_val(t,stack):
    try:
        float(t)
        return float(t)
    except:
        for scope in reversed(stack):
            if (t,scope) in symbol_table:
                return symbol_table[t,scope]['value']
        else:
            return None

def retrieve(t):
    flg = False
    for a in reversed(stack):
        if (t,a) in symbol_table:
            if symbol_table[(t,a)]['valid']:
                t = symbol_table[(t,a)]['value']
                return (t,a)
            else:
                flg=True
    if flg:
        print("error",t,a,(t,a)in symbol_table)
    return (t,0)

def retrieve_scope(t,stack):
    for a in reversed(stack):
        if (t,a) in symbol_table:
            return a
    return 0

stack = [0]

from beautifultable import BeautifulTable
table = BeautifulTable()
table.column_headers = ["Operator", "Argument 1", "Argument 2","Result","Scope"]
ig_list = []
class quadruple:
    def __init__(self,operator,arg1,arg2,result,stack):
        self.operator = operator
        self.arg1 = arg1
        self.arg2 = arg2
        self.result = result
        self.stack = deepcopy(stack)
        # print('asduasdkhaksud',self.stack)
        global table
        table.append_row([operator,arg1,arg2,result,self.stack])
        ig_list.append([operator,arg1,arg2,result,self.stack])



stack_icg = []
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

    quadruple(flatten(p[-2])[0][0],flatten(p[-1])[0],"1",cur_var,stack)
    quadruple("=",cur_var,"",flatten(p[-1])[0],stack)

    symbol_table[(cur_var,0)] = {}
    symbol_table[(cur_var,0)]["type"] = "temporary"
    symbol_table[(cur_var,0)]["valid"] = True
    symbol_table[(cur_var,0)]["value"] = None

    var_num+=1

def p_CodegenPostfix(p):
    '''codegen_postfix :'''
    global var_num
    cur_var = "t"+str(var_num)

    print(cur_var, ' = ' , flatten(p[-2])[0], flatten(p[-1])[0][0], 1 )
    print(flatten(p[-2])[0], ' = ', cur_var)

    quadruple(flatten(p[-1])[0][0],flatten(p[-2])[0],"1",cur_var,stack)
    quadruple("=",cur_var,"",flatten(p[-2])[0],stack)

    symbol_table[(cur_var,0)] = {}
    symbol_table[(cur_var,0)]["type"] = "temporary"
    symbol_table[(cur_var,0)]["valid"] = True
    symbol_table[(cur_var,0)]["value"] = None

    var_num+=1

def p_CodegenShorthand(p):
    '''codegen_shorthand :'''
    global var_num
    cur_var = "t"+str(var_num)

    print(cur_var, ' = ' , stack_icg[-3], stack_icg[-2][0], stack_icg[-1],0)
    print(stack_icg[-3], ' = ', cur_var)

    quadruple(stack_icg[-2][0],stack_icg[-3],stack_icg[-1],cur_var,stack)
    quadruple("=",cur_var,"",stack_icg[-3],stack)

    symbol_table[(cur_var,0)] = {}
    symbol_table[(cur_var,0)]["type"] = "temporary"
    symbol_table[(cur_var,0)]["valid"] = True
    symbol_table[(cur_var,0)]["value"] = None

    stack_icg.pop()
    stack_icg.pop()
    stack_icg.pop()

    var_num+=1

def p_CodegenDeclarator(p):
    '''codegen_declarator :'''
    print(stack_icg[-2],' = ',stack_icg[-1])
    print(stack)
    # print("----------------",stack_icg[-2],retrieve_scope(stack_icg[-2]))
    quadruple("=",stack_icg[-1],"",stack_icg[-2],stack)

    stack_icg.pop()
    stack_icg.pop()

def p_push_icg(p):
    '''push_icg :'''
    # print(stack_icg,'ancd',p[-1],'abc',p[:])
    if len(stack_icg) == 0 and flatten(p[-2])[0] not in symbol_table:
        stack_icg.append(flatten(p[-2])[0])
    stack_icg.append(flatten(p[-1])[0])

def p_CodegenBinop(p):
    '''codegen_binop :'''
    global var_num
    cur_var = "t"+str(var_num)
    print(cur_var," = ", stack_icg[-3],stack_icg[-2],stack_icg[-1])
    quadruple(stack_icg[-2],stack_icg[-3],stack_icg[-1],cur_var,stack)

    symbol_table[(cur_var,0)] = {}
    symbol_table[(cur_var,0)]["type"] = "temporary"
    symbol_table[(cur_var,0)]["valid"] = True
    symbol_table[(cur_var,0)]["value"] = None
    print("\n\n\n============Symbol Table=============")

    for symbol in symbol_table:
        if((symbol_table[symbol]["type"] == "identifier" or symbol_table[symbol]["type"] == "temporary" )and symbol_table[symbol]["valid"] == True):
            print(symbol,"\t",symbol_table[symbol])

    stack_icg.pop()
    stack_icg.pop()
    stack_icg.pop()
    stack_icg.append(cur_var)
    var_num+=1

def p_CodegenAssign(p):
    '''codegen_assign :'''
    global var_num
    print(stack_icg[-2],' = ',stack_icg[-1])
    quadruple("=",stack_icg[-1],"",stack_icg[-2],stack)
    stack_icg.pop()
    stack_icg.pop()


def p_error(p):

    # get formatted representation of stack_icg
    stack_state_str = ' '.join([symbol.type for symbol in parser.symstack_icg][1:])

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
    print("\n\n\n\n\n====================Three Address Quadruple Intermediate Code Generation================")
    print(table)
    global ig_list

    for line in ig_list:
        print(line)

    dead_ig = deepcopy(ig_list)
    print("=============DEAD CODE===============")

    flg = True
    while flg:
        for line in dead_ig:
            if(line[0]=="=" or line[0]=="*" or line[0]=="/" or line[0]=="+" or line[0]=="-"):
                op = line[3]
                for new_line in dead_ig:
                    if new_line!=line:
                        if new_line[1]==op or new_line[2]==op:
                            if(len(line)==4):
                                line.append(True)
                            else:
                                line[4] = True
                            break
                        else:
                            if(len(line)==4):
                                line.append(False)
                            elif len(line)==5:
                                line[4] = False
        flg = False
        len1 = len(dead_ig)
        dead_ig2 = [line for line in dead_ig if len(line)==4 or (len(line)==5 and line[4]==True)]
        len2 = len(dead_ig2)
        if(len1!=len2):
            flg =True
        dead_ig = dead_ig2

    table2 = BeautifulTable()
    table2.column_headers = ["Operator", "Argument 1", "Argument 2","Result","Scope"]
    for line in dead_ig:
        if(len(line)==6):
            if(line[4]==True):
                table2.append_row(line[0:5])
        else:
            table2.append_row(line)

    print(table2)

    print("================CSE===================")
    for i in range(len(ig_list)):
        line = ig_list[i]
        if(line[0]=="+" or line[0]=="-" or line[0]=="*" or line[0]=="/"):
            op1 = line[1]
            op2 = line[2]
            op = line[0]
            lhs = line[3]

            for j in range(i+1,len(ig_list)):
                new_line = ig_list[j]
                if new_line!=line:
                    if new_line[0] == op and new_line[1] == op1 and new_line[2] == op2:
                        new_line[0] = "="
                        new_line[1] = lhs
                        new_line[2] = ""

    table2 = BeautifulTable()
    table2.column_headers = ["Operator", "Argument 1", "Argument 2","Result","Scope"]
    for line in ig_list:
        if(len(line)==6):
            if(line[4]==True):
                table2.append_row(line[0:5])
        else:
            table2.append_row(line)

    print(table2)


    print("=============Constant Folding & Propagation===========")

    for line in ig_list:
            print(line)
            t1 = retrieve_val(line[1],line[4])
            t2 = retrieve_val(line[2],line[4])
            # print('t1,t2',t1,t2)
            # elif(line[1]==1 or line[2]==1)
            #     line[1] =
            if(line[0]=="+" or line[0]=="-" or line[0]=="*" or line[0]=="/" or line[0]=="="):
                op = line[0]
                if(op=="+"):
                    ans = t1 + t2
                elif(op=="-"):
                    ans = t1 - t2
                elif(op=="*"):
                    ans = t1 * t2
                elif(op=='/'):
                    ans = t1 / t2
                elif(op=='='):
                    ans = t1

                line[0] = "="
                line[1] = ans
                line[2] = ""
            scope = retrieve_scope(line[3],line[4])
            symbol_table[(line[3],scope)]['value'] = ans
            # print(line[3],symbol_table[(line[3],scope)])

    for i in range(len(ig_list)):
        line = ig_list[i]
        try:
            float(line[1])
            if(line[2]==""):
                lhs = line[3]
                rhs = line[1]

                for j in range(i+1,len(ig_list)):
                    new_line = ig_list[j]

        except:
            pass

    table2 = BeautifulTable()
    table2.column_headers = ["Operator", "Argument 1", "Argument 2","Result","Scope"]
    for line in ig_list:
        if(len(line)==6):
            if(line[4]==True):
                table2.append_row(line[0:5])
        else:
            table2.append_row(line)

    print(table2)

    print("\n\n\n============Symbol Table=============")

    for symbol in symbol_table:
        if((symbol_table[symbol]["type"] == "identifier" or symbol_table[symbol]["type"] == "temporary" )and symbol_table[symbol]["valid"] == True):
            print(symbol,"\t",symbol_table[symbol])

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
    '''TypeDeclaration : ClassHeader '{' push FieldDeclarations '}' pop
    | ClassHeader '{' push '}' pop
    '''
    p[0] = p[1:]

def p_ClassHeader(p):
    '''ClassHeader : Modifiers ClassWord IDENTIFIER
    |           ClassWord IDENTIFIER
    '''
    if len(list(p))==4:
        symbol_table[(p[3],currentscope)]['modifiers'] = flatten(p[1])
    else:
        symbol_table[(p[2],currentscope)]['modifiers'] = None
    symbol_table[(flatten(list(p))[-1],currentscope)]["valid"] = True
    symbol_table[(flatten(list(p))[-1],currentscope)]["dtype"] = "class"
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
            if (variable,currentscope) in symbol_table:
                symbol_table[(variable,currentscope)]['modifiers'] = flatten(p[1])
                symbol_table[(variable,currentscope)]['value'] = 'None'
                symbol_table[(variable,currentscope)]["valid"] = True
                symbol_table[(variable,currentscope)]['dtype'] = flatten(p[2])[0]
                # symbol_table[(variable,True)]['global'] = True
    else:
        for variable in flatten(p[2]):
            if (variable,currentscope) in symbol_table:
                symbol_table[(variable,currentscope)]['modifiers'] = None
                symbol_table[(variable,currentscope)]['value'] = 'None'
                symbol_table[(variable,currentscope)]["valid"] = True
                symbol_table[(variable,currentscope)]['dtype'] = flatten(p[1])[0]
                # symbol_table[(variable,True)]['global'] = True

    p[0] = p[1:]

def p_VariableDeclarators(p):
    '''VariableDeclarators : VariableDeclarator
    | VariableDeclarators ',' VariableDeclarator
    '''
    if(len(p[:]) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:]


def p_VariableDeclarator(p):
    '''VariableDeclarator : DeclaratorName
    | DeclaratorName push_icg '=' VariableInitializer codegen_declarator
    '''
    if(len(list(p)) == 2):
        p[0] = p[1]
    else:
        p[0] = p[1:2]+p[3:5]
    if len(list(p))==5:
        variable = flatten(p[1])[0]
        symbol_table[(variable,currentscope)]['value'] = flatten(p[3])[0]#int


def p_VariableInitializer(p):
    '''VariableInitializer : Expression
    | '{' push '}' pop
        | '{' push ArrayInitializers '}' pop
    '''
    if(len(p[:]) == 2):
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
        symbol_table[(method,stack[-1])]['modifiers'] = flatten(p[1])
        symbol_table[(method,stack[-1])]["valid"] = True
        symbol_table[(method,stack[-1])]['dtype'] = flatten(p[2])[0]
    else:
        method =flatten(p[2])[0]
        symbol_table[(method,stack[-1])]['modifiers'] = None
        symbol_table[(method,stack[-1])]["valid"] = True
        symbol_table[(method,stack[-1])]['dtype'] = flatten(p[1])[0]
    p[0] = p[1:]

def p_MethodDeclarator(p):
    '''MethodDeclarator : DeclaratorName '(' ParameterList ')'
    | DeclaratorName '(' ')'
    | MethodDeclarator OP_DIM
    '''

    if len(list(p)) == 5:
        method = flatten(p[1])[0]
        symbol_table[(method,currentscope)]['params'] = []
        for param in flatten(p[3]):
            if (param,currentscope) in symbol_table and param not in symbol_table[(method,currentscope)]['params']:
                if symbol_table[(param,currentscope)]['type'] =='identifier':
                    symbol_table[(method,currentscope)]['params'].append(param)
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
    if variable not in keywords:
        symbol_table[(variable,currentscope)]['dtype'] = flatten(p[1])[0]
        symbol_table[(variable,currentscope)]['valid'] = True
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
        symbol_table[(method,currentscope)]['modifiers'] = flatten(p[1])
        symbol_table[(method,currentscope)]["valid"] = True
    else:
        method =flatten(p[1])[0]
        symbol_table[(method,currentscope)]['modifiers'] = None
        symbol_table[(method,currentscope)]["valid"] = True
    p[0] = p[1:]

def p_ConstructorDeclarator(p):
    '''ConstructorDeclarator : IDENTIFIER '(' ParameterList ')'
    | IDENTIFIER '(' ')'
    '''
    if len(list(p)) == 5:
        method = flatten(p[1])[0]
        symbol_table[(method,currentscope)]['params'] = []
        for param in flatten(p[3]):
            if (param,currentscope) in symbol_table and param not in symbol_table[(method,currentscope)]['params']:
                if symbol_table[(param,currentscope)]['type'] =='IDENTIFIER':
                    symbol_table[(method,currentscope)]['params'].append(param)
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
    '''Block : '{' push LocalVariableDeclarationsAndStatements '}' pop
    | '{' push '}' pop
    '''
    p[0] = p[1:]

scopenum = 0
currentscope = 0
def p_push(p):
    '''push :'''
    global scopenum,currentscope
    # scopenum+=1
    scopenum+=1
    currentscope = scopenum
    stack.append(scopenum)



def p_pop(p):
    '''pop :'''
    stack.pop()
    global currentscope
    currentscope = stack[-1]

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
    global currentscope

    for variable in flatten(p[2]):
        if (variable,currentscope) in symbol_table:
            symbol_table[(variable,currentscope)]['modifiers'] = None
            symbol_table[(variable,currentscope)]['scope'] = "s"+str(currentscope)
            symbol_table[(variable,currentscope)]['valid'] = True
            symbol_table[(variable,currentscope)]['dtype'] = flatten(p[1])[0]



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
    '''IterationStatement : DO codegen_do_init Statement  WHILE '(' Expression ')' ';' codegen_do_final
    | FOR '(' ForInit codegen_for_init ForExpr codegen_for_expr ForIncr codegen_for_inc ')' Statement codegen_for
    | FOR '(' ForInit codegen_for_init ForExpr codegen_for_expr ')' Statement codegen_for
    '''
    p[0] = p[1:]

def p_CodegenDoInit(p):
    '''codegen_do_init : '''
    global lab_num
    global label
    for i in range(2):
        lab_num+=1
        label.append(lab_num)
    print("L"+str(label[-2]),':')
    quadruple("Label","","","L"+str(label[-2]))

def p_CodegenDoFinal(p):
    '''codegen_do_final : '''
    global stack_icg
    global label
    global var_num
    temp = 't'+str(var_num)
    print(temp,' = not', stack_icg[-1])
    print('if',temp,'goto L'+str(label[-1]))

    quadruple("!",stack_icg[-1],"",temp)
    quadruple("if",temp,"","L"+str(label[-1]))
    var_num+=1

    print('goto L'+str(label[-2]))
    print('L'+str(label[-1]),':')

    quadruple("goto","","","L"+str(label[-2]))
    quadruple("Label","","","L"+str(label[-1]))

def p_CodegenForInit(p):
    '''codegen_for_init :'''
    global lab_num
    global label
    for i in range(4):
        lab_num+=1
        label.append(lab_num)
    print("L"+str(label[-4]),':')

    quadruple("Label","","","L"+str(label[-4]))

def p_CodegenForExpr(p):
    '''codegen_for_expr :'''
    global var_num
    global label
    temp = 't'+str(var_num)
    print(temp,' = not', stack_icg[-1])
    print('if',temp,'goto L'+str(label[-3]))

    quadruple("!",stack_icg[-1],"",temp)
    quadruple("if",temp,"","L"+str(label[-3]))
    var_num+=1
    print('goto L'+str(label[-2]))
    print('L'+str(label[-1]),':')

    quadruple("goto","","","L"+str(label[-2]))
    quadruple("Label","","","L"+str(label[-1]))

def p_CodegenForInc(p):
    '''codegen_for_inc :'''
    global label
    global lab_num
    print('goto L'+str(label[-4]))
    print('L'+str(label[-2]),':')

    quadruple("goto","","","L"+str(label[-4]))
    quadruple("Label","","",'L'+str(label[-2]))

def p_CodegenFor(p):
    '''codegen_for :'''
    global label
    global lab_num
    print('goto L'+str(label[-1]))
    print('L'+str(label[-3]),':')

    quadruple("goto","","","L"+str(label[-1]))
    quadruple("label","","",'L'+str(label[-3]))
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
        | ArrayAllocationExpression '{' push '}' pop
        | ClassAllocationExpression '{' push '}' pop
        | ArrayAllocationExpression '{' push ArrayInitializers '}' pop
        | ClassAllocationExpression '{' push FieldDeclarations '}' pop
    '''
    if(len(p[:]) == 2):
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
    '''CastExpression : UnaryExpression push_icg
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
    | MultiplicativeExpression '*' push_icg  CastExpression codegen_binop
    | MultiplicativeExpression '/' push_icg CastExpression codegen_binop
    | MultiplicativeExpression '%' push_icg CastExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

# Need to add string case.
def p_AdditiveExpression(p):
    '''AdditiveExpression : MultiplicativeExpression
    | AdditiveExpression '+' push_icg MultiplicativeExpression codegen_binop
    | AdditiveExpression '-' push_icg MultiplicativeExpression codegen_binop
    '''
    #search
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_RelationalExpression(p):
    '''RelationalExpression : AdditiveExpression
    | RelationalExpression '<' push_icg AdditiveExpression codegen_binop
    | RelationalExpression '>' push_icg AdditiveExpression codegen_binop
    | RelationalExpression OP_LE push_icg AdditiveExpression codegen_binop
    | RelationalExpression OP_GE push_icg AdditiveExpression codegen_binop
    '''

    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_EqualityExpression(p):
    '''EqualityExpression : RelationalExpression
        | EqualityExpression OP_EQ push_icg RelationalExpression codegen_binop
        | EqualityExpression OP_NE push_icg RelationalExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_AndExpression(p):
    '''AndExpression : EqualityExpression
        | AndExpression '&' push_icg EqualityExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_ExclusiveOrExpression(p):
    '''ExclusiveOrExpression : AndExpression
    | ExclusiveOrExpression '^' push_icg AndExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_InclusiveOrExpression(p):
    '''InclusiveOrExpression : ExclusiveOrExpression
    | InclusiveOrExpression '|' push_icg ExclusiveOrExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_ConditionalAndExpression(p):
    '''ConditionalAndExpression : InclusiveOrExpression
    | ConditionalAndExpression OP_LAND push_icg InclusiveOrExpression codegen_binop
    '''
    if len(list(p))==6:
        p[0] = p[1:3]+p[4:5]
    else:
        p[0] = p[1]

def p_ConditionalOrExpression(p):
    '''ConditionalOrExpression : ConditionalAndExpression
    | ConditionalOrExpression OP_LOR push_icg ConditionalAndExpression codegen_binop
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
    | UnaryExpression push_icg '=' ConditionalExpression codegen_assign
    | UnaryExpression push_icg  AssignmentOperator push_icg AssignmentExpression codegen_shorthand
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
