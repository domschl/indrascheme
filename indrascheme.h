#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <functional>

using std::cout;
using std::endl;
using std::map;
using std::string;
using std::vector;

namespace insch {

class ISAtom {
  public:
    enum TokType { NIL = 0,
                   ERROR = 1,
                   INT,
                   FLOAT,
                   STRING,
                   BOOLEAN,
                   SYMBOL,
                   QUOTE,
                   BRANCH };
    TokType t;
    int val;
    double valf;
    string vals;
    ISAtom *pNext;
    ISAtom *pChild;
    ISAtom() {
        pNext = nullptr;
        pChild = nullptr;
        t = NIL;
        val = 0;
        valf = 0.0;
        vals = "";
    }
    string str(bool decor = false) {
        string out;
        switch (t) {
        case ISAtom::TokType::NIL:
            if (decor)
                out = "⦉";
            else
                out = "";
            break;
        case ISAtom::TokType::ERROR:
            out = "<Error: " + vals + ">";
            break;
        case ISAtom::TokType::BRANCH:
            out = "(";
            break;
        case ISAtom::TokType::INT:
            out = std::to_string(val);
            break;
        case ISAtom::TokType::FLOAT:
            out = std::to_string(valf);
            break;
        case ISAtom::TokType::STRING:
            if (decor)
                out = "\"" + vals + "\"";
            else
                out = vals;
            break;
        case ISAtom::TokType::BOOLEAN:
            if (val == 0)
                out = "#f";
            else
                out = "#t";
            break;
        case ISAtom::TokType::QUOTE:
            out = "'";
            break;
        case ISAtom::TokType::SYMBOL:
            if (decor)
                out = "⧼𝔰⧽" + vals;
            else
                out = vals;
            break;
        default:
            out = "<UNEXPECTED>";
            break;
        }
        return out;
    }
};

class IndraScheme {
  public:
    map<string, std::function<ISAtom *(ISAtom *, map<string, ISAtom *> &)>> inbuilts;
    map<string, ISAtom *> symbols;
    map<string, ISAtom *> funcs;
    vector<string> tokTypeNames = {"Nil", "Error", "Int", "Float", "String", "Boolean", "Symbol", "Quote", "Branch"};
    vector<ISAtom *> gctr;

    IndraScheme() {
        for (auto cm_op : "+-*/%") {
            if (cm_op == 0) continue;
            string m_op{cm_op};
            inbuilts[m_op] = [this, m_op](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return math_2ops(pisa, local_symbols, m_op); };
        }
        for (auto cmp_op : {"==", "!=", ">=", "<=", "<", ">", "and", "or"}) {
            string m_op{cmp_op};
            inbuilts[m_op] = [this, m_op](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return cmp_2ops(pisa, local_symbols, m_op); };
        }
        inbuilts["define"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return makeDefine(pisa, local_symbols); };
        inbuilts["let"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return makeLocalDefine(pisa, local_symbols); };
        inbuilts["if"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return evalIf(pisa, local_symbols); };
        inbuilts["while"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return evalWhile(pisa, local_symbols); };
        inbuilts["print"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return evalPrint(pisa, local_symbols); };
        inbuilts["load"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return evalLoad(pisa, local_symbols); };
        inbuilts["list"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return listList(pisa, local_symbols); };
        inbuilts["cons"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return listCons(pisa, local_symbols); };
        // inbuilts["car"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return listCar(pisa, local_symbols); };
        // inbuilts["cdr"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return listCdr(pisa, local_symbols); };
        inbuilts["len"] = [&](ISAtom *pisa, map<string, ISAtom *> &local_symbols) -> ISAtom * { return listLen(pisa, local_symbols); };
    }

    ISAtom *gca(ISAtom *src = nullptr) {
        ISAtom *nisa;
        if (src)
            nisa = new ISAtom(*src);
        else
            nisa = new ISAtom();
        gctr.push_back(nisa);
        return nisa;
    }

    void gc(ISAtom *current) {
    }

    bool is_int(string token, bool nat = false) {
        if (!nat) {
            if (token.length() && token[0] == '-')
                token = token.substr(1);
        }
        if (token.length() == 0)
            return false;
        string isn = "0123456789";
        for (unsigned int i = 0; i < token.length(); i++) {
            if (isn.find(token[i]) == string::npos) {
                return false;
            }
        }
        return true;
    }

    bool is_float(string token) {
        size_t pos = token.find('.');
        if (pos == string::npos) return false;
        string s1 = token.substr(0, pos);
        if (s1.length() > 0 && !is_int(s1)) return false;
        string s2 = token.substr(pos + 1);
        if (s1.length() == 0 && s2.length() == 0) return false;
        pos = s2.find('e');
        if (pos == string::npos) pos = s2.find('E');
        if (pos == string::npos) {
            if (!is_int(s2, true) && s2.length() > 0) return false;
            return true;
        } else {
            string s1 = s2.substr(0, pos);
            if (!is_int(s1, true) && s1.length() > 0) return false;
            string s3 = s2.substr(pos + 1);
            if (!is_int(s3)) return false;
            return true;
        }
    }

    bool is_string(string token) {
        if (token.length() > 1 && token[0] == '"' && token[token.length() - 1] == '"')
            return true;
        else
            return false;
    }

    bool is_boolean(string token) {
        if (token == "#t" or token == "#f")
            return true;
        else
            return false;
    }

    bool is_symbol(string token) {
        if (token.length() == 0) return false;
        string inv_first = "0123456790'\"\\";
        string inv_next = "'\"\\";
        if (inv_first.find(token[0]) != string::npos) return false;
        for (auto c : token) {
            if (inv_next.find(c) != string::npos) return false;
        }
        return true;
    }

    bool is_quote(string token) {
        if (token == "'") {
            return true;
        }
        return false;
    }

    ISAtom *parseTok(ISAtom *pisa, string symbol) {
        if (is_int(symbol)) {
            pisa->t = ISAtom::TokType::INT;
            pisa->val = atoi(symbol.c_str());
            return pisa;
        }
        if (is_float(symbol)) {
            pisa->t = ISAtom::TokType::FLOAT;
            pisa->valf = atof(symbol.c_str());
            return pisa;
        }
        if (is_string(symbol)) {
            pisa->t = ISAtom::TokType::STRING;
            pisa->vals = symbol.substr(1, symbol.length() - 2);
            return pisa;
        }
        if (is_boolean(symbol)) {
            pisa->t = ISAtom::TokType::BOOLEAN;
            if (symbol == "#t")
                pisa->val = 1;
            else
                pisa->val = 0;
            return pisa;
        }
        if (is_symbol(symbol)) {
            pisa->t = ISAtom::TokType::SYMBOL;
            pisa->vals = symbol;
            return pisa;
        }
        if (is_quote(symbol)) {
            pisa->t = ISAtom::TokType::QUOTE;
            pisa->vals = symbol;
            return pisa;
        }
        pisa->t = ISAtom::TokType::ERROR;
        pisa->vals = "Can't parse: <" + symbol + ">";
        return pisa;
    }

    ISAtom *_insert_curSymbol(ISAtom *pNode, string *pCurSymbol) {
        parseTok(pNode, *pCurSymbol);
        pNode->pNext = new ISAtom();
        *pCurSymbol = "";
        return pNode->pNext;
    }

    ISAtom *parse(string &input, ISAtom *pNode = nullptr, int level = 0) {
        enum ParseState { START,
                          STRING,
                          COMMENT };
        ParseState state = START;
        string curSymbol = "";
        char c;
        bool is_esc = false;
        ISAtom *pStart;

        if (pNode == nullptr) pNode = new ISAtom();
        pStart = pNode;

        while (input.length() > 0) {
            c = input[0];
            input = input.substr(1);
            switch (state) {
            case START:
                switch (c) {
                case '(':
                    if (curSymbol.length() > 0) {
                        pNode = _insert_curSymbol(pNode, &curSymbol);
                    }
                    pNode->t = ISAtom::TokType::BRANCH;
                    pNode->pChild = parse(input, nullptr, level + 1);
                    pNode->pNext = new ISAtom();
                    pNode = pNode->pNext;
                    break;
                case ')':
                    if (curSymbol.length() > 0) {
                        pNode = _insert_curSymbol(pNode, &curSymbol);
                    }
                    return pStart;
                    break;
                case ';':
                    if (curSymbol.length() > 0) {
                        pNode = _insert_curSymbol(pNode, &curSymbol);
                    }
                    state = COMMENT;
                    break;
                    /*  ?? What was that?
                case '\'':
                    if (curSymbol.length() == 0) {
                        curSymbol += c;
                        pNode = _insert_curSymbol(pNode, &curSymbol);
                    } else {
                        curSymbol += c;  // This should probably generate an illegal state...
                    }
                    break;
                    */
                case ' ':
                case '\n':
                case '\r':
                case '\t':
                    if (curSymbol.length() > 0) {
                        pNode = _insert_curSymbol(pNode, &curSymbol);
                    }
                    break;
                case '"':
                    if (curSymbol.length() == 0) {
                        state = STRING;
                        is_esc = false;
                        curSymbol = "\"";
                    } else {
                        cout << "STRING-FALLTHROUGH-UNHANDLED <" << curSymbol << "> ";
                    }
                    break;
                default:
                    curSymbol += c;
                    break;
                }
                break;
            case COMMENT:
                switch (c) {
                case '\n':
                    state = START;
                    curSymbol = "";
                    continue;
                default:
                    break;
                }
                break;
            case STRING:
                switch (c) {
                case '\\':
                    is_esc = true;
                    break;
                case '"':
                    if (!is_esc) {
                        curSymbol += c;
                        pNode = _insert_curSymbol(pNode, &curSymbol);
                        state = START;
                    } else {
                        is_esc = false;
                        curSymbol += c;
                    }
                    break;
                default:
                    if (is_esc) {
                        switch (c) {
                        case 'n':
                            curSymbol += '\n';
                            break;
                        case 'r':
                            curSymbol += '\r';
                            break;
                        case 't':
                            curSymbol += '\t';
                            break;
                        case '\\':
                            curSymbol += '\\';
                            break;
                        default:
                            curSymbol += "\\";
                            curSymbol += c;
                        }
                        is_esc = false;
                    } else {
                        curSymbol += c;
                        break;
                    }
                }
                break;
            default:
                cout << "Parser: broken state." << endl;
                return nullptr;
                break;
            }
        }
        return pStart;
    }

    void print(ISAtom *pisa, bool decor = true) {
        string out = pisa->str(decor);
        ISAtom *pN = pisa->pNext;
        cout << out;
        if (pisa->pChild != nullptr) {
            print(pisa->pChild, decor);
            cout << ")";
        }
        if (pN != nullptr) {
            if (pisa->t != ISAtom::TokType::QUOTE) cout << " ";
            print(pN, decor);
        }
    }

    ISAtom *
    cmp_2ops(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols, string m_op) {
        // cout << m_op << endl;
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *p = pisa, *pn = nullptr;
        ISAtom *pRes = new ISAtom();

        if (getListLen(pisa) != 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Not enough operands for <" + m_op + "> operation";
            return pRes;
        }
        ISAtom *pl = pisa, *pr = pisa->pNext;
        pl = chainEval(pl, local_symbols);
        pr = chainEval(pr, local_symbols);
        if (pl->t != pr->t) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Error: compare " + m_op + " requires two operands of same type, got: " + tokTypeNames[pl->t] + " and " + tokTypeNames[pr->t];
            return pRes;
        }
        pRes->t = ISAtom::TokType::BOOLEAN;
        pRes->val = 0;
        bool a, b, r;
        switch (pl->t) {
        case ISAtom::TokType::INT:
            if (m_op == "==")
                r = (pl->val == pr->val);
            else if (m_op == ">=")
                r = (pl->val >= pr->val);
            else if (m_op == "<=")
                r = (pl->val <= pr->val);
            else if (m_op == "!=")
                r = (pl->val != pr->val);
            else if (m_op == ">")
                r = (pl->val > pr->val);
            else if (m_op == "<")
                r = (pl->val < pr->val);
            else {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Unsupported compare operation: " + m_op + " for type " + tokTypeNames[pl->t];
                return pRes;
            }
            if (r)
                pRes->val = 1;
            else
                pRes->val = 0;
            return pRes;
            break;
        case ISAtom::TokType::FLOAT:
            if (m_op == "==")
                r = (pl->valf == pr->valf);
            else if (m_op == ">=")
                r = (pl->valf >= pr->valf);
            else if (m_op == "<=")
                r = (pl->valf <= pr->valf);
            else if (m_op == "!=")
                r = (pl->valf != pr->valf);
            else if (m_op == ">")
                r = (pl->valf > pr->valf);
            else if (m_op == "<")
                r = (pl->valf < pr->valf);
            else {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Unsupported compare operation: " + m_op + " for type " + tokTypeNames[pl->t];
                return pRes;
            }
            if (r)
                pRes->val = 1;
            else
                pRes->val = 0;
            return pRes;
            break;
        case ISAtom::TokType::SYMBOL:
        case ISAtom::TokType::STRING:
            if (m_op == "==")
                r = (pl->vals == pr->vals);
            else if (m_op == ">=")
                r = (pl->vals >= pr->vals);
            else if (m_op == "<=")
                r = (pl->vals <= pr->vals);
            else if (m_op == "!=")
                r = (pl->vals != pr->vals);
            else if (m_op == ">")
                r = (pl->vals > pr->vals);
            else if (m_op == "<")
                r = (pl->vals < pr->vals);
            else {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Unsupported compare operation: " + m_op + " for type " + tokTypeNames[pl->t];
                return pRes;
            }
            if (r)
                pRes->val = 1;
            else
                pRes->val = 0;
            return pRes;
            break;
        case ISAtom::TokType::BOOLEAN:
            if (pl->val)
                a = true;
            else
                a = false;
            if (pr->val)
                b = true;
            else
                b = false;
            if (m_op == "==")
                r = (a == b);
            else if (m_op == "and")
                r = (a && b);
            else if (m_op == "or")
                r = (a || b);
            else if (m_op == "!=")
                r = (a != b);
            else {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Unsupported compare operation: " + m_op + " for type " + tokTypeNames[pl->t];
                return pRes;
            }
            if (r)
                pRes->val = 1;
            else
                pRes->val = 0;
            return pRes;
            break;
        default:
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Can't compare " + m_op + " for type: " + tokTypeNames[pl->t];
            return pRes;
        }
    }

    ISAtom *math_2ops(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols, string m_op) {
        // cout << m_op << endl;
        ISAtom *pisa = copyList(pisa_o);
        int res = 0;
        double fres = 0.0;
        ISAtom *p = pisa, *pn = nullptr;
        bool fl = false;
        bool first = true;
        ISAtom *pRes = new ISAtom();

        if (getListLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Not enough operands for <" + m_op + "> operation";
            return pRes;
        }

        while (p != nullptr) {
            ISAtom *pn = p->pNext;
            p = chainEval(p, local_symbols);
            p->pNext = pn;
            if (p->t == ISAtom::TokType::INT) {
                if (first) {
                    res = p->val;
                    first = false;
                } else {
                    if (m_op == "+") {
                        if (fl)
                            fres += p->val;
                        else
                            res += p->val;
                    } else if (m_op == "-") {
                        if (fl)
                            fres -= p->val;
                        else
                            res -= p->val;
                    } else if (m_op == "*") {
                        if (fl)
                            fres = fres * p->val;
                        else
                            res = res * p->val;
                    } else if (m_op == "/") {
                        if (p->val == 0) {
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "DIV/ZERO!";
                            return pRes;
                        } else {
                            if (fl) {
                                fres /= p->val;
                            } else {
                                res /= p->val;
                            }
                        }
                    } else if (m_op == "%") {
                        if (p->val == 0) {
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "DIV/ZERO!";
                            return pRes;
                        } else {
                            if (fl)
                                fres = (int)fres % p->val;
                            else
                                res = res % p->val;
                        }
                    } else {
                        pRes->t = ISAtom::TokType::ERROR;
                        pRes->vals = "Op-not-impl: " + m_op;
                        return pRes;
                    }
                }
                p = p->pNext;
            } else if (p->t == ISAtom::TokType::FLOAT) {
                if (first) {
                    fres = p->valf;
                    first = false;
                    fl = true;
                } else {
                    if (m_op == "+") {
                        if (!fl) {
                            fres = res;
                            fres += p->valf;
                            fl = true;
                        } else
                            fres += p->valf;
                    } else if (m_op == "-") {
                        if (!fl) {
                            fres = res;
                            fres -= p->valf;
                            fl = true;
                        } else
                            fres -= p->valf;
                    } else if (m_op == "*") {
                        if (!fl) {
                            fres = res;
                            fres *= p->valf;
                            fl = true;
                        } else
                            fres *= p->valf;
                    } else if (m_op == "/") {
                        if (p->valf == 0) {
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "DIV/ZERO!";
                            return pRes;
                        } else {
                            if (!fl) {
                                fres = res;
                                fres /= p->valf;
                                fl = true;
                            } else {
                                fres /= p->valf;
                            }
                        }
                    } else if (m_op == "%") {
                        if (p->valf == 0) {
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "DIV/ZERO!";
                            return pRes;
                        } else {
                            if (!fl) {
                                fres = res;
                                fres = (int)fres % (int)p->valf;
                                fl = true;
                            } else {
                                fres = (int)fres % (int)p->valf;
                            }
                        }
                    } else {
                        pRes->t = ISAtom::TokType::ERROR;
                        pRes->vals = "Op-not-impl: " + m_op;
                        return pRes;
                    }
                }
                p = p->pNext;
            } else if (p->t == ISAtom::TokType::NIL) {
                // cout << "SKIP: " << p->t << " " << p->vals << endl;
                p = p->pNext;
            } else {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Op: " + m_op + ", unhandled tokType: " + tokTypeNames[p->t];
                if (p->t == ISAtom::TokType::ERROR) pRes->vals += ": " + p->vals;
                return pRes;
            }
        }
        if (fl) {
            pRes->t = ISAtom::TokType::FLOAT;
            pRes->valf = fres;
        } else {
            pRes->t = ISAtom::TokType::INT;
            pRes->val = res;
        }
        return pRes;
    }

    int getListLen(ISAtom *pisa) {  // XXX NIL is counted!
        int len = 1;
        ISAtom *p = pisa;
        while (p->pNext) {
            p = p->pNext;
            ++len;
        }
        return len;
    }

    ISAtom *copyList(ISAtom *pisa) {
        // return pisa;

        if (pisa == nullptr) return nullptr;
        ISAtom *c = new ISAtom(*pisa);
        if (pisa->pChild) c->pChild = copyList(pisa->pChild);
        if (pisa->pNext) c->pNext = copyList(pisa->pNext);
        return c;
    }

    void deleteList(ISAtom *pisa) {
        if (pisa == nullptr) return;
        deleteList(pisa->pChild);
        deleteList(pisa->pNext);
        delete pisa;
    }

    bool is_defined(string name, map<string, ISAtom *> &local_symbols) {
        if (symbols.find(name) == symbols.end() && funcs.find(name) == funcs.end() && local_symbols.find(name) == local_symbols.end()) return false;
        return true;
    }

    bool is_defined_symbol(string name, map<string, ISAtom *> &local_symbols) {
        if (symbols.find(name) == symbols.end() && local_symbols.find(name) == local_symbols.end()) return false;
        return true;
    }

    bool is_defined_func(string name) {
        if (funcs.find(name) == funcs.end()) return false;
        return true;
    }

    ISAtom *makeDefine(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = new ISAtom();
        if (getListLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'define' requires 2 operands: name and value(s)";
            return pRes;
        }
        ISAtom *pN = pisa;
        ISAtom *pV = pN->pNext;
        ISAtom *pNa;
        int n = 0;
        bool err = false;
        switch (pN->t) {
        case ISAtom::TokType::SYMBOL:
            if (getListLen(pisa) > 3) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Symbol-'define' requires exactly 2 operands: name and value";
                return pRes;
            }
            symbols[pN->vals] = copyList(chainEval(pV, local_symbols));
            return symbols[pN->vals];
            break;
        case ISAtom::TokType::BRANCH:
            pNa = pN->pChild;
            n = 0;
            err = false;
            while (pNa && !err) {
                if (pNa->t == ISAtom::TokType::NIL) break;
                if (pNa->t == ISAtom::TokType::SYMBOL) {
                    n = n + 1;
                } else {
                    err = true;
                    pRes->t = ISAtom::TokType::ERROR;
                    pRes->vals = "'define' function requires symbol as first operand (name) and optional symbols identifying parameters, type " + tokTypeNames[pNa->t] + " is invalid, symbol required.";
                    break;
                }
                pNa = pNa->pNext;
            }
            if (n == 0 && !err) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'define' function requires symbol as first operand (name) and optional symbols identifying parameters";
            } else {
                if (!err) {
                    pNa = pN->pChild;
                    funcs[pNa->vals] = pisa;
                    pRes->t = ISAtom::TokType::NIL;
                }
            }
            return pRes;
            break;
        default:
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'define' requires symbol as first operand (name)";
            return pRes;
            break;
        }
    }

    ISAtom *makeLocalDefine(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = new ISAtom();
        if (getListLen(pisa) < 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'let' requires at least 1 operand";
            return pRes;
        }

        if (pisa->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'let' no primary list: required are a list of key values pairs: ((k v ), ..) [ ()]";
            return pRes;
        }
        ISAtom *pDef = pisa->pChild;
        while (pDef && pDef->t != ISAtom::TokType::NIL) {
            if (pDef->t != ISAtom::TokType::BRANCH) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'let' list entries must be list: required are a list of key values pairs: ((k v ), ..) [ ()]";
                return pRes;
            }
            if (getListLen(pDef->pChild) != 3) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'let' list entries must be list of exactly two entries: required are a list of key values pairs: ((k v ), ..) [ ()]";
                return pRes;
            }
            ISAtom *pName = pDef->pChild;
            if (pName->t != ISAtom::TokType::SYMBOL) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'let' list entries must be list of exactly two entries, and first must be a symbol: required are a list of key values pairs: ((k v ), ..) [ ()]";
                return pRes;
            }
            ISAtom *pVal = pName->pNext;
            local_symbols[pName->vals] = copyList(chainEval(pVal, local_symbols));
            pDef = pDef->pNext;
        }

        if (pisa->pNext) {
            ISAtom *pExpr = pisa->pNext;
            ISAtom *pNa, *pR;
            pR = pisa;
            while (pExpr && pExpr->t != ISAtom::TokType::NIL) {
                pNa = pExpr->pNext;
                ISAtom *pNc = copyList(pExpr);
                pNc->pNext = nullptr;
                pR = eval(pNc, local_symbols);
                pExpr = pNa;
            }
            return copyList(pR);
            // return eval(pV->pNext, local_symbols);
        } else {
            return copyList(pisa);
        }
    }

    ISAtom *evalIf(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *pRes = new ISAtom();
        if (getListLen(pisa) != 3 && getListLen(pisa) != 4) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'if' requires 2 or 3 operands: <condition> <true-expr> [<false-expr>]";
            return pRes;
        }
        ISAtom *pC = new ISAtom(*pisa);
        pC->pNext = nullptr;
        ISAtom *pT = copyList(pisa->pNext);
        ISAtom *pF = copyList(pT->pNext);
        pT->pNext = nullptr;

        ISAtom *pR = chainEval(pC, local_symbols);
        if (pR->t != ISAtom::TokType::BOOLEAN) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'if' condition should result in boolean, but we got: " + tokTypeNames[pR->t];
            return pRes;
        }
        if (pR->val) {
            pRes = eval(pT, local_symbols);
        } else {
            if (pF) {
                pRes = eval(pF, local_symbols);
            } else {
                pRes->t = ISAtom::TokType::NIL;
            }
        }
        return pRes;
    }

    ISAtom *evalWhile(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *pRes = new ISAtom();
        if (getListLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'while' requires at least 2 operands: <condition> <expr> [<expr>]...";
            return pRes;
        }
        ISAtom *pC = new ISAtom(*pisa);
        pC->pNext = nullptr;
        ISAtom *pL = pisa->pNext;
        ISAtom *pN;

        ISAtom *pCR = chainEval(pC, local_symbols);
        if (pCR->t != ISAtom::TokType::BOOLEAN) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'while' condition should result in boolean, but we got: " + tokTypeNames[pCR->t];
            return pRes;
        }
        int n = 20;
        ISAtom *pCalc = pL;
        while (pCR->val) {
            pL = copyList(pCalc);
            while (pL && pL->t != ISAtom::TokType::NIL) {
                pN = pL->pNext;
                ISAtom *pLc = copyList(pL);
                pRes = eval(pLc, local_symbols);
                pL = pN;
            }
            pCR = chainEval(pC, local_symbols);
            if (pCR->t != ISAtom::TokType::BOOLEAN) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'while' condition should result in boolean, but we got: " + tokTypeNames[pCR->t];
                return pRes;
            }
        }
        return pRes;
    }

    ISAtom *evalPrint(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = new ISAtom();
        if (getListLen(pisa) < 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'print' requires at least 1 operand: <expr> [<expr>]...";
            return pRes;
        }
        ISAtom *pP = pisa;
        ISAtom *pResS = chainEval(pP, local_symbols);
        print(pResS, false);
        return pResS;
    }

    ISAtom *listLen(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = new ISAtom();
        if (pisa->t == ISAtom::TokType::QUOTE) pisa = pisa->pNext;
        if (getListLen(pisa) != 2 || pisa->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'len' requires one list or quoted list operand";
            return pRes;
        }
        pRes->t = ISAtom::TokType::INT;
        pRes->val = getListLen(pisa->pChild) - 1;
        return pRes;
    }

    ISAtom *listList(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = new ISAtom();
        ISAtom *pStart;
        pStart = pRes;
        pRes->t = ISAtom::TokType::QUOTE;
        pRes->pNext = new ISAtom();
        pRes = pRes->pNext;
        pRes->t = ISAtom::TokType::BRANCH;
        pRes->pChild = new ISAtom(*pisa);
        pRes = pRes->pChild;
        pRes->pNext = nullptr;
        pisa = pisa->pNext;
        while (pisa) {
            pRes->pNext = new ISAtom(*pisa);
            pRes = pRes->pNext;
            pRes->pNext = nullptr;
            pisa = pisa->pNext;
        }
        print(pStart);
        return pStart;
    }

    ISAtom *listCons(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = new ISAtom(), *pStart;
        pStart = pRes;
        if (getListLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cons' requires two args";
            return pRes;
        }
        ISAtom *c1 = pisa;
        ISAtom *c2 = eval(pisa->pNext, local_symbols);
        if (c2->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cons' 2nd arg needs to eval to list (e.g. quoted list)";
            return pRes;
        }
        pRes->t = ISAtom::TokType::QUOTE;
        pRes->pNext = new ISAtom();
        pRes = pRes->pNext;
        pRes->t = ISAtom::TokType::BRANCH;
        pRes->pChild = new ISAtom(*c1);
        pRes = pRes->pChild;
        pRes->pNext = nullptr;
        pisa = c2->pChild;
        while (pisa) {
            pRes->pNext = new ISAtom(*pisa);
            pRes = pRes->pNext;
            pRes->pNext = nullptr;
            pisa = pisa->pNext;
        }
        print(pStart);
        return pStart;
    }

    ISAtom *evalLoad(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = new ISAtom();
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'load' requires one string operand, a filename, got: " + std::to_string(getListLen(pisa));
            return pRes;
        }
        ISAtom *pP = pisa;
        if (pP->t != ISAtom::TokType::STRING) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'load' requires a string operand, a filename";
            return pRes;
        }

        char buf[129];
        int nb;
        string cmd = "";
        FILE *fp = fopen(pP->vals.c_str(), "r");
        if (fp) {
            while (!feof(fp)) {
                nb = fread(buf, 1, 128, fp);
                buf[nb] = 0;
                cmd += buf;
            }
        }
        // replaceAll(cmd, "\\n", "\n");
        ISAtom *pisa_p = parse(cmd);
        // map<string, ISAtom *> ls;
        pisa = chainEval(pisa_p, local_symbols);

        return pisa;
    }

    bool is_inbuilt(string funcName) {
        if (inbuilts.find(funcName) == inbuilts.end()) return false;
        return true;
    }

    ISAtom *eval_symbol(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols) {
        ISAtom *p, *pn;
        ISAtom *pisa = copyList(pisa_o);
        if (is_defined_symbol(pisa->vals, local_symbols)) {
            if (local_symbols.find(pisa->vals) != local_symbols.end())
                p = local_symbols[pisa->vals];
            else
                p = symbols[pisa->vals];
            while (p->t == ISAtom::TokType::SYMBOL) {
                if (is_defined_symbol(p->vals, local_symbols)) {
                    if (local_symbols.find(p->vals) != local_symbols.end()) {
                        p = local_symbols[p->vals];
                    } else {
                        p = symbols[p->vals];
                    }
                } else {
                    break;
                }
            }
            p->pNext = pisa->pNext;
            return copyList(p);
        } else {
            // can't eval
            return pisa;
        }
    }

    ISAtom *eval_func(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *p, *pn, *pDef;
        // ISAtom *pisa = copyList(pisa_o);
        map<string, ISAtom *> function_arguments = local_symbols;
        if (is_defined_func(pisa->vals)) {
            pDef = funcs[pisa->vals];

            ISAtom *pNa = pDef->pChild;
            vector<string> localNames;
            int n = 0;
            bool err = false;
            string func_name;
            ISAtom *pRes = new ISAtom();
            while (pNa && !err) {
                if (pNa->t == ISAtom::TokType::NIL) break;
                if (pNa->t == ISAtom::TokType::SYMBOL) {
                    n = n + 1;
                    if (n > 1) {
                        localNames.push_back(pNa->vals);
                        // cout << "localVar" << n-1 << ", " << pNa->vals << endl;
                    } else {
                        func_name = pNa->vals;
                    }
                    pNa = pNa->pNext;
                } else {
                    err = true;
                    pRes->t = ISAtom::TokType::ERROR;
                    pRes->vals = "'define' function requires symbol as first operand (name) and optional symbols identifying parameters, type " + tokTypeNames[pNa->t] + " is invalid, symbol required.";
                    return pRes;
                    break;
                }
            }
            if (getListLen(pisa) - 2 != n - 1) {  // Pisa: fun-name and nil: 2, n: fun-name: 1
                err = true;
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Function " + func_name + " requires " + std::to_string(n - 1) + " arguments, " + std::to_string(getListLen(pisa) - 2) + " given";
                return pRes;
            }
            ISAtom *pCurVar;
            ISAtom *pInp = pisa;
            for (auto var_name : localNames) {
                pInp = pInp->pNext;
                pCurVar = new ISAtom(*pInp);  // XXX gc!
                pCurVar->pNext = nullptr;
                function_arguments[var_name] = pCurVar;
            }
            p = eval(pDef->pNext, function_arguments);
            return p;
        } else {
            // can't eval
            return pisa;
        }
    }

    ISAtom *eval(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols, bool func_only = false) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pisan, *pN, *pRet, *pReti;
        ISAtom *pCur = pisa, *pRetCur = nullptr;

        pN = pCur->pNext;
        switch (pCur->t) {
        case ISAtom::TokType::QUOTE:
            pCur = pN;
            return pCur;
            break;
        case ISAtom::TokType::BRANCH:
            if (!pRetCur) {
                pRet = eval(pCur->pChild, local_symbols, true);
                pRetCur = pRet;
            } else {
                pReti = eval(pCur->pChild, local_symbols, func_only);
                pRetCur->pNext = pReti;
                pRetCur = pReti;
            }
            pCur = pN;
            break;
        case ISAtom::TokType::SYMBOL:
            // if (pRetCur) break;
            if (is_inbuilt(pCur->vals)) {
                // cout << "inbuilt: " << pCur->vals << endl;
                pReti = inbuilts[pCur->vals](pCur->pNext, local_symbols);
            } else if (is_defined_func(pCur->vals)) {
                // cout << "func: " << pCur->vals << endl;
                pReti = eval_func(pCur, local_symbols);
            } else if (!func_only && is_defined_symbol(pCur->vals, local_symbols)) {
                // cout << "symbol: " << pCur->vals << endl;
                pReti = eval_symbol(pCur, local_symbols);
            } else {
                pisan = new ISAtom();  // XXX That will loose mem! (Maybe insert error into chain?)
                pisan->t = ISAtom::TokType::ERROR;
                pisan->vals = "Undefined function: " + pisa->vals;
                return pisan;
            }
            if (!pRetCur) {
                pRet = pReti;
                pRetCur = pRet;
            } else {
                pRetCur->pNext = pReti;
                pRetCur = pReti;
            }
            return pReti;
            // pCur = nullptr;  // pN;
            break;
        case ISAtom::TokType::ERROR:
            // cout << "Error: " << pisa->vals << endl;
            return pCur;
            break;
        default:
            if (func_only) {
                pisan = new ISAtom();  // XXX That will loose mem! (Maybe insert error into chain?)
                pisan->t = ISAtom::TokType::ERROR;
                pisan->vals = "Undefined expression: " + pisa->str();
            } else {
                return pCur;
            }
            break;
        }
        return pRet;
    }

    ISAtom *chainEval(ISAtom *pisa_o, map<string, ISAtom *> &local_symbols) {
        // ISAtom *p = new ISAtom(*pisa);  // XXX
        // ISAtom *pn = new ISAtom(*p);    // XXX
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *p = pisa, *pn;  //  *pn, *pRes, *pPrev, *pCur;
        // pRes = pisa;
        //  pPrev = nullptr;
        ISAtom *pCE = nullptr, *pCEi;
        ISAtom *pCERes = new ISAtom();

        while (p && p->t != ISAtom::TokType::NIL) {
            pn = p->pNext;
            p->pNext = new ISAtom();  // nullptr;
            switch (p->t) {
            case ISAtom::TokType::QUOTE:
                pCEi = pn;
                break;
            case ISAtom::TokType::NIL:
                pCEi = new ISAtom();
                break;
            case ISAtom::TokType::SYMBOL:
                pCEi = eval_symbol(copyList(p), local_symbols);
                break;
            case ISAtom::TokType::BRANCH:
                pCEi = eval(copyList(p), local_symbols, true);
                break;
            default:
                pCEi = new ISAtom(*p);
                break;
            }
            p = pn;
            if (pCEi) {
                if (!pCE) {
                    pCE = new ISAtom(*pCEi);
                    pCERes = pCE;
                } else {
                    pCE->pNext = new ISAtom(*pCEi);
                    pCE = pCE->pNext;
                    pCE->pNext = new ISAtom();
                }
            }
        }
        return pCERes;
    }
};

}  // namespace insch
