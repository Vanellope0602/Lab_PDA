import cfg
from lustre_lex import lexer

class Recognition():

    def __init__(self, pda, tokens):
        self.pda = pda
        self.stack = list()
        self.tokens = tokens # 初始化传进来的是token_index

    # strip()函数不加参数时，默认删掉字符串头尾的空白符
    # only accept final state
    def is_accept(self, cur_state, token_index):
        print("cur_state :" + cur_state)
        if cur_state.strip() == cfg.END:
            return True
        if token_index == len(self.tokens):
            if cur_state.strip() == 'Q_loop' and self.stack[-1] == 'Δ':
                return True
            return False

        cur_stack = self.stack.copy()
        for state in self.pda.states[cur_state]:
            cur_token = self.tokens[token_index].value
            if state.read.strip() == cur_token or state.read.strip() == cfg.EPS:
                if state.action.strip() == 'POP' and len(self.stack) and state.value.strip() == self.stack[-1].strip():
                    print("=========pop")
                    print("cur_state: ", cur_state)
                    print("state: ",state)
                    print("Self.Stack: ", self.stack)
                    print("Self.tokens: ", self.tokens[token_index])
                    print("token_index: ", str(token_index) + " , token length: " + str(len(self.tokens)))
                    self.stack.pop()
                    if self.is_accept(state.next, token_index if state.read.strip() == cfg.EPS else token_index + 1):
                        return True
                    else:
                        self.stack = cur_stack.copy()
                elif state.action.strip() == 'PUSH':
                    print("=========push")
                    print("cur_state: ", cur_state)
                    print("state: ", state)
                    print("Self.Stack: ", self.stack)
                    print("Self.tokens: ", self.tokens[token_index])
                    print("token_index: ", str(token_index) + " , token length: " + str(len(self.tokens)))
                    self.stack.append(state.value)
                    if self.is_accept(state.next, token_index if state.read.strip() == cfg.EPS else token_index + 1):
                        return True
                    else:
                        self.stack = cur_stack.copy()
        return False

def create_pda(grammarStr):
    # Create the grammar from the string
    grammar = cfg.ContextFreeGrammar(grammarStr)

    # # Simplify the grammar (simplifies using basic rules)
    # grammar.simplify()

    # # Removes any duplicate productions from any state
    # grammar.removeDuplicates()

    # Converts the grammar to a pushdown automata object
    pda = grammar.toPDA()
    print(pda)
    # Prints a textual representation of the grammar
    return pda


if __name__ == '__main__':
    # grammarString = """
    # S -> 0 S 0 | T
    # T -> 1 T 1
    #   -> $
    # """
    # grammarString = """
    # SimpleExp -> $
    #             -> SimpleExp - SimpleExp
    #             -> SimpleExp + SimpleExp
    #             -> SimpleExp / SimpleExp
    #             -> SimpleExp * SimpleExp
    #             -> if SimpleExp then SimpleExp else SimpleExp
    # Constant -> true | false | IntConst | RealConst
    # SimpleTuple -> ( SimpleExpList ) | $
    # """
    grammarString = ""
    with open("lustre_part.txt") as r_f: # lustre_part 测试成功
        for line in r_f.readlines():
            grammarString += line
            if line == "":
                print("空行, 截断文法单元: \n", grammarString)
    # inputString = '''
    # node EDGE (X: bool) returns (Y: bool);
    # let
    #     Y = current (X);
    # tel
    # '''
    # 下面是自己写的实验lustre程序语句
    inputString = '''
    let
    
    tel
    ''' # 可通过

    inputString = '''
    let
        X = (Y);
    tel
    ''' # 可通过

    inputString = '''
    let
        X = pre (Y);
    tel
    ''' # 可通过

    inputString = '''
    let
        X = pre (Y);
        assert not (X);
    tel
    ''' # 可通过

    inputString = '''
    let
        assert (Y);
        assert not (X);
    tel
    ''' # 可通过
    # input String是要识别的程序, lexer.input()将输入分成不同的词

    # lexer.input(inputString)
    # string_tokens = list()
    # while True:
    #     tok = lexer.token() #会调用到lustre_lex.py当中的正则分析
    #     if not tok:
    #         break
    #     string_tokens.append(tok)

    # print("Tokens:")
    # for string_token in string_tokens:
    #     print(string_token) # 打印词法分析内容


    # 开始下推自动机内容
    print("===========")
    print("Pda:")
    pda = create_pda(grammarString) # grammarstring是喂进去的文法
    print("===========")
    
    # print("Recognition:")
    # reconigtion = Recognition(pda, string_tokens) # 给创建好的PDA和分析好的词法
    # result = reconigtion.is_accept('Start',0) # 当前状态cur_state : Start, token_index:0
    # print("===========")

    # if result:
    #     print("result : True")
    # else:
    #     print("result : False")
    # String_2 = '''
    # node r_edge (X: bool) returns (Y: bool);
    # let
    #  Y = false -> X and not pre(X);
    # tel
    # '''

