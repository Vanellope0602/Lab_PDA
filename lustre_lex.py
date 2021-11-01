import ply.lex as lex

# 关键字,是一个dict，左侧为该Token的value，右侧字典内容是类型, t.type的值
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

# 词法分析器必须提供一个标记的列表，这个列表将所有可能的标记告诉分析器, 作为token的Type
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
] + list(reserved.values()) # 保留字仅放进来 dict 当中的values, 合并成一个大的list

# 每种标记用一个正则表达式规则来表示，每个规则需要以”t_“开头声明表示该声明是对标记的规则定义
# 紧跟在t_后面的单词，必须跟标记列表中的某个标记名称对应

# 带有一些动作代码的正则表达式规则
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

# 一些简单标记的正则表达式规则
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
t_ignore = ' \t' # 忽略字符

# 首字母应为字母,剩余字母可为字母 数字 下划线, 为自己命名的变量名称(不是保留字的token)
# r'<正则表达式>' 定义一个识别变量名的规则
# lv6id = Lustre V6 ID
def t_LV6ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    # 先检查是否是保留字，如果是保留字，则类型 t.type为 key (也就是reserved.get)
    # 如果不是保留字，则自动将 'LV6ID' 赋值给Type
    t.type = reserved.get(t.value, 'LV6ID')

    if not reserved.get(t.value):
        t.str = t.value  # 用新的属性str来保存该token的值
        t.value = 'lv6id'
        print(t.str)
    return t

# 定义一个识别换行符的规则，这样就可以追踪行数
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# 定义一个出错规则
def t_error(t):
    print("Illegal charactor '%s'" % t.value[0])
    t.lexer.skip(1)

# 根据以上规则构建Lexer词法分析器
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

    # 开始进行词法分析
    while True:
        tok = lexer.token() #重复调用token()方法来获取标记序列
        # 返回下一个LexToken类型的标记实例，如果进行到输入字串的尾部时将返回None
        if not tok: # 没有更多输入了
            break
        print(tok, " " ,tok.type, " ", tok.value) # 打印token的类型和内容(字符串),所在行数, 相对于起始位置的偏移
        if hasattr(tok, 'str'): # 函数用于判断对象tok是否包含'str'的属性(在识别非保留字的函数lv6id中给了t.str这个str属性)
            print("该token不是保留字：", tok.str)
            #print(tok.str)