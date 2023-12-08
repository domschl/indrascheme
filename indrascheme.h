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
    map<string, std::function<ISAtom *(ISAtom *)>> inbuilts;
    map<string, ISAtom *> symbols;

    IndraScheme() {
        for (auto cm_op : "+-*/%") {
            if (cm_op == 0) continue;
            string m_op{cm_op};
            inbuilts[m_op] = [this, m_op](ISAtom *pisa) -> ISAtom * { return math_2ops(pisa, m_op); };
        }
        inbuilts["define"] = [&](ISAtom *pisa) -> ISAtom * { return makeDefine(pisa); };
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
        if (!is_int(s1)) return false;
        string s2 = token.substr(pos + 1);
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
        case ISAtom::TokType::QUOTE:
            cout << "'";
            break;
        case ISAtom::TokType::SYMBOL:
            cout << "(s)" << pisa->vals;
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

    ISAtom *math_2ops(ISAtom *pisa, string m_op) {
        // cout << m_op << endl;
        int res = 0;
        double fres = 0.0;
        ISAtom *p = pisa, *pn = nullptr;
        bool fl = false;
        bool first = true;
        ISAtom *pRes = new ISAtom();

        while (p != nullptr) {
            pn = p;
            if (p->t == ISAtom::TokType::SYMBOL) {
                if (symbols.find(p->vals) != symbols.end()) {
                    p = symbols[p->vals];
                }
            }
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
                p = pn->pNext;
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
                p = pn->pNext;
            } else if (p->t == ISAtom::TokType::BRANCH) {
                // cout << "sub-eval!" << endl;
                p = eval(p);
                // sum += p->val;
                // p = p->pNext;
            } else {
                // cout << "SKIP: " << p->t << " " << p->vals << endl;
                p = pn->pNext;
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

    ISAtom *makeDefine(ISAtom *pisa) {
        return pisa;
        // symbols[name] = new ISAtom(*pisa);
    }

    bool
    is_inbuilt(string funcName) {
        if (inbuilts.find(funcName) == inbuilts.end()) return false;
        return true;
    }

    ISAtom *eval(ISAtom *pisa) {
        ISAtom *pisan;
        switch (pisa->t) {
        case ISAtom::TokType::BRANCH:
            return eval(pisa->pChild);
            break;
        case ISAtom::TokType::SYMBOL:
            if (is_inbuilt(pisa->vals)) {
                // cout << "calling: " << pisa->vals << endl;
                return inbuilts[pisa->vals](pisa->pNext);
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
