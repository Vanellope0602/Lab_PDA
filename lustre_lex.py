import ply.lex as lex

reserved = {
    'bool': "BOOL",
    'real': "REAL",
    'if': "IF",
    'struct': "STRUCT",
    'false': "FALSE",
    'fby': "FBY",
    'node': "NODE",
    'xor': "XOR",
    'package': "PACKAGE",
    'let': "LET",
    'int': "INT",
    'provides': "PROVIDES",
    'end': "END",
    'function': "FUNCTION",
    'include': "INCLUDE",
    'is': "IS",
    'pre': "PRE",
    'type': "TYPE",
    'need': "NEEDS",
    'unsafefunction': "UNSAFEFUNCTION",
    'externnode': "EXTERNNODE",
    'current': "CURRENT",
    'div': "DIV",
    'assert': "ASSERT",
    'returns': "RETURNS",
    'uses': "USES",
    'mod': "MOD",
    'enum': "ENUM",
    'externfunction': "EXTERNFUNCTION",
    'when': "WHEN",
    'unsafeexternnode': "UNSAFEEXTERNNODE",
    'model': "MODEL",
    'and': "AND",
    'or': "OR",
    'unsafeexternfunction': "UNSAFEEXTERNFUNCTION",
    'tel': "TEL",
    'body': "BODY",
    'nor': "NOR",
    'merge': "MERGE",
    'const': "CONST",
    'unsafenode': "UNSAFENODE",
    'true': "TRUE",
    'with': "WITH",
    'not': "NOT",
    'then': "THEN",
    'else': "ELSE",
    'step': "STEP",
    'var': "VAR"
}

tokens = [
    "INTCONST",  # ++
    "REALCONST",  # ++
    "LV6IDREF",  # ++
    'LESSEQU',  # <=
    'LBRACKET',  # [
    'COMMA',  # ,
    'GREATEREQU',  # >=
    'RBRACKET',  # ]
    'LPARENTHESE',  # (
    'RPARENTHESE',  # )
    'ARROW',  # ->
    'LSHIFT',  # <<
    'STAR',  # *
    'MINUS',  # -
    'PLUS',  # +
    'LBPARENTHESE',  # {
    'RBPARENTHESE',  # }
    'HASHTAG',  # #
    'EQU',  # =
    'COLON',  # :
    'NOEQU',  # <>
    'DIVIDE',  # /
    'SURPLUS',  # %
    'LESS',  # <
    'EXPONENT',  # ^
    'POINT',  # .
    'RSHIFT',  # >>
    'SEMICOLON',  # ;
    'SHIFT',  # =>
    'TPOINT',  # ..
    'GREATER',  # >
    "LV6ID"  # ++
] + list(reserved.values())


def t_LV6IDREF(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*::[a-zA-Z0-9_]+'
    t.str = str(t.value)
    t.value = 'lv6idref'
    return t


def t_REALCONST(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


def t_INTCONST(t):
    r'\d+'
    t.value = int(t.value)
    return t


t_BOOL = r'bool'
t_REAL = r'real'
t_IF = r'if'
t_STRUCT = r'struct'
t_FALSE = r'false'
t_FBY = r'fby'
t_NODE = r'node'
t_XOR = r'xor'
t_PACKAGE = r'package'
t_LET = r'let'
t_INT = r'int'
t_PROVIDES = r'provides'
t_END = r'end'
t_FUNCTION = r'function'
t_INCLUDE = r'include'
t_IS = r'is'
t_PRE = r'pre'
t_TYPE = r'type'
t_NEEDS = r'NEEDS'
t_UNSAFEFUNCTION = r'unsafefunction'
t_EXTERNNODE = r'externnode'
t_CURRENT = r'current'
t_DIV = r'div'
t_ASSERT = r'assert'
t_RETURNS = r'returns'
t_USES = r'uses'
t_MOD = r'mod'
t_ENUM = r'enum'
t_EXTERNFUNCTION = r'externfunction'
t_TEL = r'tel'
t_BODY = r'body'
t_NOR = r'nor'
t_MERGE = r'merge'
t_CONST = r'const'
t_UNSAFENODE = r'unsafenode'
t_TRUE = r'true'
t_WITH = r'with'
t_NOT = r'not'
t_THEN = r'then'
t_ELSE = r'else'
t_STEP = r'step'
t_VAR = r'var'
t_LESSEQU = r'<='
t_LSHIFT = r'<<'
t_NOEQU = r'<>'
t_GREATEREQU = r'>='
t_ARROW = r'\*>'
t_RSHIFT = r'>>'
t_SHIFT = r'=>'
t_TPOINT = r'\.\.'
t_LBRACKET = r'\['
t_COMMA = r','
t_RBRACKET = r'\]'
t_LPARENTHESE = r'\('
t_RPARENTHESE = r'\)'
t_STAR = r'\*'
t_MINUS = r'-'
t_PLUS = r'\+'
t_LBPARENTHESE = r'{'
t_RBPARENTHESE = r'}'
t_HASHTAG = r'\#'
t_EQU = r'='
t_COLON = r':'
t_DIVIDE = r'/'
t_SURPLUS = r'%'
t_LESS = r'<'
t_EXPONENT = r'\^'
t_POINT = r'\.'
t_SEMICOLON = r';'
t_GREATER = r'>'
t_ignore = ' \t'


def t_LV6ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'LV6ID')
    if not reserved.get(t.value):
        t.str = t.value
        t.value = 'lv6id'
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal charactor '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

if __name__ == '__main__':
    data = '''
    node edge (X: bool) returns (Y: bool);
    let
     Y = r_edge (X) or r_edge (not(X));
    tel
    node r_edge (X: bool) returns (Y: bool);
    let
     Y = false -> X and not pre(X);
    tel
    '''
    lexer.input(data)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
        if hasattr(tok, 'str'):
            print(tok.str)