symbol_table = {}

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
"while","for"]
for kw in keywords:
    symbol_table[kw] = {}
    symbol_table[kw]['type'] = "keyword"

# Tokens
def t_OP_INC(t) :
 r"\+\+"
 t.type = "OP_INC"
 return t

def t_OP_DEC(t) :
 r"--"
 t.type = "OP_DEC"
 return t

def t_OP_GE(t) :
 r">="
 t.type = "OP_GE"
 return t

def t_OP_LE(t) :
 r"<="
 t.type = "OP_LE"
 return t

def t_OP_EQ(t) :
 r"=="
 t.type = "OP_EQ"
 return t

def t_OP_NE(t) :
 r"!="
 t.type = "OP_NE"
 return t

def t_OP_LAND(t) :
 r"&&"
 t.type = "OP_LAND"
 return t

def t_OP_LOR(t) :
 r"\|\|"
 t.type = "OP_LOR"
 return t

def t_ASS_MUL(t) :
 r"\*="
 t.type = "ASS_MUL"
 return t

def t_ASS_DIV(t) :
 r"/="
 t.type = "ASS_DIV"
 return t

def t_ASS_MOD(t) :
 r"%="
 t.type = "ASS_MOD"
 return t

def t_ASS_ADD(t) :
 r"\+="
 t.type = "ASS_ADD"
 return t

def t_ASS_SUB(t) :
 r"-="
 t.type = "ASS_SUB"
 return t

t_LITERAL  = r'\"(\\.|[^\\"])*\"'

def t_COMMENT(t) :
    r'\/\*\*(\\.|[^\\"])*\*\/'

# t_BOOLLIT  = r'"true"|"false"'
# def t_BOOLLIT(t) :
#  r'"true"|"false"'
#  t.type = "BOOLLIT"
#  return t

literals = [';', '.', ',', '+', '-', '*', '/','~',
            '%','<', '>', '!', '&', '|','^',
            '(', ')', '{', '}', '=']

def t_IDENTIFIER(t):
    r"[A-Za-z$_][A-Za-z$_0-9]*"
    if t.value == "true" or t.value == "false":
        t.type = "BOOLLIT"
        return t
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
