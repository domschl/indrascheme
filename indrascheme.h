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
    string symbol;
    ISAtom *pNext;
    ISAtom *pChild;
    ISAtom() {
        pNext = nullptr;
        pChild = nullptr;
        t = NIL;
        val = 0;
        vals = "";
        symbol = "";
    }
};

class IndraScheme {
  public:
    map<string, std::function<ISAtom *(ISAtom *, map<string, ISAtom *> &)>> inbuilts;
    map<string, ISAtom *> symbols;
    map<string, ISAtom *> funcs;
    vector<string> tokTypeNames = {"Nil", "Error", "Int", "Float", "String", "Boolean", "Symbol", "Quote", "Branch"};

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
                case '\'':
                    if (curSymbol.length() == 0) {
                        curSymbol += c;
                        pNode = _insert_curSymbol(pNode, &curSymbol);
                    } else {
                        curSymbol += c;  // This should probably generate an illegal state...
                    }
                    break;
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
                        curSymbol += c;
                        is_esc = false;
                    }
                    break;
                default:
                    curSymbol += c;
                    is_esc = false;
                    break;
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

    void print(ISAtom *pisa) {
        switch (pisa->t) {
        case ISAtom::TokType::NIL:
            cout << "⦉";
            break;
        case ISAtom::TokType::ERROR:
            cout << "<Error: " + pisa->vals + ">";
            break;
        case ISAtom::TokType::BRANCH:
            cout << "(";
            break;
        case ISAtom::TokType::INT:
            cout << pisa->val;
            break;
        case ISAtom::TokType::FLOAT:
            cout << pisa->valf;
            break;
        case ISAtom::TokType::STRING:
            cout << "\"" << pisa->vals << "\"";
            break;
        case ISAtom::TokType::BOOLEAN:
            if (pisa->val == 0)
                cout << "#f";
            else
                cout << "#t";
            break;
        case ISAtom::TokType::QUOTE:
            cout << "'";
            break;
        case ISAtom::TokType::SYMBOL:
            cout << "⧼𝔰⧽" << pisa->vals;
            break;
        default:
            cout << "<UNEXPECTED>";
            break;
        }
        if (pisa->pChild != nullptr) {
            print(pisa->pChild);
            cout << ")";
        }
        if (pisa->pNext != nullptr) {
            cout << " ";
            print(pisa->pNext);
        }
    }

    ISAtom *_simplify(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        // ISAtom *p = new ISAtom(*pisa);  // XXX
        // ISAtom *pn = new ISAtom(*p);    // XXX
        ISAtom *p = pisa, *pn, *pRes, *pPrev, *pCur;
        pRes = pisa;
        pPrev = nullptr;
        while (p) {
            pn = p->pNext;
            if (p->t == ISAtom::TokType::SYMBOL) {
                p = eval_symbol(p, local_symbols);
                p->pNext = pn;
            }
            if (p->t == ISAtom::TokType::BRANCH) {
                // cout << "sub-eval!" << endl;
                p = eval(p, local_symbols);
                p->pNext = pn;
            }
            pCur = new ISAtom(*p);
            if (pPrev) {
                pPrev->pNext = pCur;
                // pRes = pCur;
            } else {
                pRes = pCur;
            }
            pPrev = pCur;
            p = p->pNext;
        }
        return pRes;
    }

    ISAtom *
    cmp_2ops(ISAtom *pisa, map<string, ISAtom *> &local_symbols, string m_op) {
        // cout << m_op << endl;
        ISAtom *p = pisa, *pn = nullptr;
        ISAtom *pRes = new ISAtom();

        if (listLen(pisa) != 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Not enough operands for <" + m_op + "> operation";
            return pRes;
        }
        ISAtom *pl = pisa, *pr = pisa->pNext;
        pl = _simplify(pl, local_symbols);
        pr = _simplify(pr, local_symbols);
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

    ISAtom *math_2ops(ISAtom *pisa, map<string, ISAtom *> &local_symbols, string m_op) {
        // cout << m_op << endl;
        int res = 0;
        double fres = 0.0;
        ISAtom *p = pisa, *pn = nullptr;
        bool fl = false;
        bool first = true;
        ISAtom *pRes = new ISAtom();

        if (listLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Not enough operands for <" + m_op + "> operation";
            return pRes;
        }

        while (p != nullptr) {
            ISAtom *pn = p->pNext;
            p = _simplify(p, local_symbols);
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

    int listLen(ISAtom *pisa) {
        int len = 1;
        ISAtom *p = pisa;
        while (p->pNext) {
            p = p->pNext;
            ++len;
        }
        return len;
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

    ISAtom *makeDefine(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *pRes = new ISAtom();
        if (listLen(pisa) < 3) {
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
            if (listLen(pisa) > 3) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Symbol-'define' requires exactly 2 operands: name and value";
                return pRes;
            }
            symbols[pN->vals] = new ISAtom(*_simplify(pV, local_symbols));
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
                    // cout << "Defined function: " << pNa->vals << " as: ";
                    // print(pisa);
                    // cout << endl;
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

    ISAtom *makeLocalDefine(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *pRes = new ISAtom();
        if (listLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'let' requires at least 2 operands: name and value";
            return pRes;
        }
        ISAtom *pN = pisa;
        ISAtom *pV = pN->pNext;
        ISAtom *pNa;
        int n = 0;
        bool err = false;
        switch (pN->t) {
        case ISAtom::TokType::SYMBOL:
            local_symbols[pN->vals] = new ISAtom(*_simplify(pV, local_symbols));
            pN = local_symbols[pN->vals];
            if (pV->pNext) {
                return eval(pV->pNext, local_symbols);
            } else {
                return pN;
            }
            break;
        default:
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'define' requires symbol as first operand (name)";
            return pRes;
            break;
        }
    }

    ISAtom *evalIf(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *pRes = new ISAtom();
        if (listLen(pisa) != 3 && listLen(pisa) != 4) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'if' requires 2 or 3 operands: <condition> <true-expr> [<false-expr>]";
            return pRes;
        }
        ISAtom *pC = new ISAtom(*pisa);
        pC->pNext = nullptr;
        ISAtom *pT = new ISAtom(*pisa->pNext);
        ISAtom *pF = pT->pNext;
        pT->pNext = nullptr;

        ISAtom *pR = _simplify(pC, local_symbols);
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
        if (listLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'while' requires at least 2 operands: <condition> <expr> [<expr>]...";
            return pRes;
        }
        ISAtom *pC = new ISAtom(*pisa);
        pC->pNext = nullptr;
        ISAtom *pL = pisa->pNext;
        ISAtom *pN;

        ISAtom *pCR = _simplify(pC, local_symbols);
        if (pCR->t != ISAtom::TokType::BOOLEAN) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'while' condition should result in boolean, but we got: " + tokTypeNames[pCR->t];
            return pRes;
        }
        int n = 20;
        ISAtom *pCalc = pL;
        while (pCR->val) {
            pL = pCalc;
            while (pL) {
                pN = pL->pNext;
                pRes = eval(pL, local_symbols);
                pL = pN;
            }
            pCR = _simplify(pC, local_symbols);
            if (pCR->t != ISAtom::TokType::BOOLEAN) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'while' condition should result in boolean, but we got: " + tokTypeNames[pCR->t];
                return pRes;
            }
        }
        return pRes;
    }

    ISAtom *evalPrint(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *pRes = new ISAtom();
        if (listLen(pisa) < 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'print' requires at least 1 operand: <expr> [<expr>]...";
            return pRes;
        }
        ISAtom *pP = pisa;
        print(_simplify(pP, local_symbols));
        return pisa;
    }

    bool is_inbuilt(string funcName) {
        if (inbuilts.find(funcName) == inbuilts.end()) return false;
        return true;
    }

    ISAtom *eval_symbol(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *p, *pn;
        if (is_defined_symbol(pisa->vals, local_symbols)) {
            if (local_symbols.find(pisa->vals) != local_symbols.end())
                p = local_symbols[pisa->vals];
            else
                p = symbols[pisa->vals];
            while (p->t == ISAtom::TokType::SYMBOL) {
                if (is_defined_symbol(p->vals, local_symbols)) {
                    if (local_symbols.find(pisa->vals) != local_symbols.end())
                        p = local_symbols[pisa->vals];
                    else
                        p = symbols[pisa->vals];
                } else {
                    break;
                }
            }
            p->pNext = pisa->pNext;
            return p;
        } else {
            // can't eval
            return pisa;
        }
    }

    ISAtom *eval_func(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *p, *pn, *pDef;
        map<string, ISAtom *> function_arguments = local_symbols;
        if (is_defined_func(pisa->vals)) {
            pDef = funcs[pisa->vals];

            ISAtom *pNa = pDef->pChild;
            vector<string> localNames;
            int n = 0;
            bool err = false;
            ISAtom *pRes = new ISAtom();
            while (pNa && !err) {
                if (pNa->t == ISAtom::TokType::NIL) break;
                if (pNa->t == ISAtom::TokType::SYMBOL) {
                    n = n + 1;
                    if (n > 1) {
                        localNames.push_back(pNa->vals);
                        // cout << "localVar" << n-1 << ", " << pNa->vals << endl;
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
            if (listLen(pisa) - 2 != n - 1) {  // Pisa: fun-name and nil: 2, n: fun-name: 1
                err = true;
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Function " + localNames[0] + " requires " + std::to_string(n - 1) + " arguments, " + std::to_string(listLen(pisa) - 2) + " given";
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

    ISAtom *eval(ISAtom *pisa, map<string, ISAtom *> &local_symbols) {
        ISAtom *pisan, *pN, *pRet;
        switch (pisa->t) {
        case ISAtom::TokType::BRANCH:
            pN = pisa->pNext;
            pRet = eval(pisa->pChild, local_symbols);
            pRet->pNext = pN;
            // if (pRet->pNext)
            //     return eval(pRet->pNext, local_symbols);
            // else
            return pRet;
            break;
        case ISAtom::TokType::SYMBOL:
            if (is_inbuilt(pisa->vals)) {
                // cout << "calling: " << pisa->vals << endl;
                return inbuilts[pisa->vals](pisa->pNext, local_symbols);
            } else if (is_defined_symbol(pisa->vals, local_symbols)) {
                ISAtom *p = eval_symbol(pisa, local_symbols);
                return p;
            } else if (is_defined_func(pisa->vals)) {
                ISAtom *p = eval_func(pisa, local_symbols);
                return p;
            }
            cout << "Not implemented: " << pisa->vals << endl;
            pisan = new ISAtom();  // XXX That will loose mem! (Maybe insert error into chain?)
            pisan->t = ISAtom::TokType::ERROR;
            pisan->vals = "Undefined symbol: " + pisa->vals;
            return pisan;
            break;
        case ISAtom::TokType::ERROR:
            // cout << "Error: " << pisa->vals << endl;
            return pisa;
            break;
        default:
            return pisa;
            break;
        }
    }
};

}  // namespace insch
