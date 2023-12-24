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
    enum DecorType { NONE = 0,
                     ASCII = 1,
                     UNICODE = 2 };
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
    string str(const DecorType decor = NONE) const {
        string out;
        switch (t) {
        case ISAtom::TokType::NIL:
            switch (decor) {
            case UNICODE:
                out = "⦉";
                break;
            case ASCII:
                out = "<";
                break;
            case NONE:
                out = "";
                break;
            }
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
            switch (decor) {
            case ASCII:
            case UNICODE:
                out = "\"" + vals + "\"";
                break;
            case NONE:
                out = vals;
                break;
            }
            break;
        case ISAtom::TokType::BOOLEAN:
            switch (decor) {
            case UNICODE:
                if (val == 0)
                    out = "↳ⓕ";
                else
                    out = "↳ⓣ";
                break;
                break;
            case ASCII:
            case NONE:
                if (val == 0)
                    out = "#f";
                else
                    out = "#t";
                break;
            }
            break;
        case ISAtom::TokType::QUOTE:
            out = "'";
            break;
        case ISAtom::TokType::SYMBOL:
            switch (decor) {
            case UNICODE:
                out = "ⓢ " + vals;
                break;
            case ASCII:
                out = "(s)" + vals;
                break;
            case NONE:
                out = vals;
                break;
            }
            break;
            // default:
            //     out = "<UNEXPECTED: " + tokTypeNames[];
            //     break;
        }
        return out;
    }
};

class IndraScheme {
  public:
    map<string, std::function<ISAtom *(ISAtom *, vector<map<string, ISAtom *>> &)>> inbuilts;
    map<string, ISAtom *> symbols;
    map<string, ISAtom *> funcs;
    vector<string> tokTypeNames = {"Nil", "Error", "Int", "Float", "String", "Boolean", "Symbol", "Quote", "Branch"};
    map<ISAtom *, size_t> gctr;
    bool memDbg = false;

    IndraScheme() {
        for (auto cm_op : "+-*/%") {
            if (cm_op == 0) continue;
            string m_op{cm_op};
            inbuilts[m_op] = [this, m_op](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return math_2ops(pisa, local_symbols, m_op); };
        }
        for (auto cmp_op : {"==", "!=", ">=", "<=", "<", ">", "and", "or"}) {
            string m_op{cmp_op};
            inbuilts[m_op] = [this, m_op](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return cmp_2ops(pisa, local_symbols, m_op); };
        }
        inbuilts["define"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return makeDefine(pisa, local_symbols); };
        inbuilts["let"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return makeLocalDefine(pisa, local_symbols); };
        inbuilts["set!"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return setLocalDefine(pisa, local_symbols); };
        inbuilts["begin"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalBegin(pisa, local_symbols); };
        inbuilts["if"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalIf(pisa, local_symbols); };
        inbuilts["while"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalWhile(pisa, local_symbols); };
        inbuilts["print"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalPrint(pisa, local_symbols); };
        inbuilts["load"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalLoad(pisa, local_symbols); };
        inbuilts["list"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listList(pisa, local_symbols); };
        inbuilts["cons"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listCons(pisa, local_symbols); };
        inbuilts["car"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listCar(pisa, local_symbols); };
        inbuilts["cdr"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listCdr(pisa, local_symbols); };
        inbuilts["len"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listLen(pisa, local_symbols); };
        inbuilts["eval"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalEval(pisa, local_symbols); };
    }

    ISAtom *gca(ISAtom *src = nullptr, bool bRegister = true) {
        ISAtom *nisa;
        if (src == nullptr) {
            nisa = new ISAtom();
        } else {
            nisa = new ISAtom(*src);
            nisa->pNext = nullptr;
            nisa->pChild = nullptr;
        }
        if (bRegister) gctr[nisa] = 1;
        return nisa;
    }
    
    map<ISAtom *, string> gctr_del_ctx;
    
    void gcd(ISAtom *pisa, string context) {
        // XXX DOES NOT DELETE!
        if (!pisa) return;
        auto pos = gctr.find(pisa);
        if (pos == gctr.end()) {
            if (memDbg) {
                cout << "Trying to delete unaccounted allocation at " << context << " of: " << pisa << ", ";
                vector<map<string, ISAtom *>> lh;
                lh.push_back({});
                print(pisa, lh, ISAtom::DecorType::UNICODE, true);
                cout << endl;
            
            if (gctr_del_ctx.find(pisa) != gctr_del_ctx.end()) {
                cout << "This has been deleted at context: " << gctr_del_ctx[pisa] << endl;
            }
            }
            // delete pisa;
        } else {
                gctr_del_ctx[pisa] = context;
                //cout << "DELETE: " << context  << endl;
                //delete pisa;
                gctr.erase(pos);
        }
    }

    void gc_clear(const ISAtom *current, vector<map<string, ISAtom *>> &local_symbols, int start_index = 0) {
        size_t gc_size, gc_freed;
        for (auto it = gctr.cbegin(); it != gctr.cend();) {
            delete it->first;
            it = gctr.erase(it);
        }
    }

    size_t gc_size() {
        return gctr.size();
    }

    int getListLen(const ISAtom *pisa) {  // XXX NIL is counted!
        int len = 1;
        const ISAtom *p = pisa;
        while (p->pNext) {
            if (p->t != ISAtom::TokType::QUOTE) ++len;
            p = p->pNext;
        }
        return len;
    }

    ISAtom *copyList(const ISAtom *pisa, bool bRegister = true) {
        if (pisa == nullptr) return nullptr;
        ISAtom *c = gca((ISAtom *)pisa, bRegister);
        if (pisa->pChild) c->pChild = copyList(pisa->pChild, bRegister);
        if (pisa->pNext) c->pNext = copyList(pisa->pNext, bRegister);
        return c;
    }

    void deleteList(ISAtom *pisa, string context) {
        if (pisa == nullptr) return;
        deleteList(pisa->pChild, context+"_C");
        deleteList(pisa->pNext, context+"_N");
        gcd(pisa, context);
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

    void parseTok(ISAtom *pisa, string symbol) {  // XXX API mess (const!)
        if (is_int(symbol)) {
            pisa->t = ISAtom::TokType::INT;
            pisa->val = atoi(symbol.c_str());
            return;
        }
        if (is_float(symbol)) {
            pisa->t = ISAtom::TokType::FLOAT;
            pisa->valf = atof(symbol.c_str());
            return;
        }
        if (is_string(symbol)) {
            pisa->t = ISAtom::TokType::STRING;
            pisa->vals = symbol.substr(1, symbol.length() - 2);
            return;
        }
        if (is_boolean(symbol)) {
            pisa->t = ISAtom::TokType::BOOLEAN;
            if (symbol == "#t")
                pisa->val = 1;
            else
                pisa->val = 0;
            return;
        }
        if (is_symbol(symbol)) {
            pisa->t = ISAtom::TokType::SYMBOL;
            pisa->vals = symbol;
            return;
        }
        if (is_quote(symbol)) {
            pisa->t = ISAtom::TokType::QUOTE;
            pisa->vals = symbol;
            return;
        }
        pisa->t = ISAtom::TokType::ERROR;
        pisa->vals = "Can't parse: <" + symbol + ">";
        return;
    }

    ISAtom *_insert_curSymbol(ISAtom *pNode, string *pCurSymbol) {
        parseTok(pNode, *pCurSymbol);
        pNode->pNext = gca();
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
        bool bError = false;
        string parsed = "";
        string errMsg = "";
        ISAtom *pStart;

        if (pNode == nullptr)
            pStart = gca();
        else
            pStart = pNode;
        ISAtom *pCurNode = pStart;

        while (input.length() > 0 && !bError) {
            c = input[0];
            parsed += c;
            input = input.substr(1);
            switch (state) {
            case START:
                switch (c) {
                case '(':
                    if (curSymbol.length() > 0) {
                        pCurNode = _insert_curSymbol(pCurNode, &curSymbol);
                    }
                    pCurNode->t = ISAtom::TokType::BRANCH;
                    pCurNode->pChild = parse(input, nullptr, level + 1);
                    pCurNode->pNext = gca();
                    pCurNode = pCurNode->pNext;
                    break;
                case ')':
                    if (curSymbol.length() > 0) {
                        pCurNode = _insert_curSymbol(pCurNode, &curSymbol);
                    }
                    return pStart;
                    break;
                case ';':
                    if (curSymbol.length() > 0) {
                        pCurNode = _insert_curSymbol(pCurNode, &curSymbol);
                    }
                    state = COMMENT;
                    break;
                case '\'':
                    if (curSymbol.length() == 0) {
                        curSymbol += c;
                        pCurNode = _insert_curSymbol(pCurNode, &curSymbol);
                    } else {
                        errMsg = "Unexpected ' within expression";
                        bError = true;
                        continue;
                    }
                    break;
                case ' ':
                case '\n':
                case '\r':
                case '\t':
                    if (curSymbol.length() > 0) {
                        pCurNode = _insert_curSymbol(pCurNode, &curSymbol);
                    }
                    break;
                case '"':
                    if (curSymbol.length() == 0) {
                        state = STRING;
                        is_esc = false;
                        curSymbol = "\"";
                    } else {
                        errMsg = "Unexpected \" within expression";
                        bError = true;
                        continue;
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
                        pCurNode = _insert_curSymbol(pCurNode, &curSymbol);
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
                errMsg = "Broken state";
                bError = true;
                continue;
                break;
            }
        }
        if (bError) {
            string fullErr = "Parser Error: " + errMsg + " at: " + parsed;
            ISAtom *errRes = gca();
            errRes->t = ISAtom::TokType::ERROR;
            errRes->vals = fullErr;
            return errRes;
        }
        // map<string, ISAtom *> ls = {};
        //  print(pStart, ls, ISAtom::DecorType::UNICODE, true);
        return pStart;
    }

    void print(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols, ISAtom::DecorType decor, bool bAutoSeparators) {
        string out = pisa->str(decor);
        ISAtom *pN = pisa->pNext;
        if (decor) {
            if (is_inbuilt(pisa->vals) || is_defined_func(pisa->vals)) out = "ⓕ " + out;
            if (is_defined_symbol(pisa->vals, local_symbols)) out = "ⓢ " + out;
        }
        cout << out;
        if (pisa->pChild != nullptr) {
            print(pisa->pChild, local_symbols, decor, bAutoSeparators);
            cout << ")";
        }
        if (pN != nullptr) {
            if (bAutoSeparators && pN->t != ISAtom::TokType::QUOTE) {
                if (pisa->t != ISAtom::TokType::QUOTE) cout << " ";
            }
            print(pN, local_symbols, decor, bAutoSeparators);
        }
    }

    ISAtom *cmp_2ops(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols, string m_op) {
        // cout << m_op << endl;
        // ISAtom *pisa = copyList(pisa_o);
        const ISAtom *p = pisa, *pn = nullptr;
        ISAtom *pRes = gca();
        vector<const ISAtom *> pAllocs;

        if (getListLen(pisa) != 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Not enough operands for <" + m_op + "> operation";
            return pRes;
        }
        ISAtom *pl = gca((ISAtom *)pisa);
        if (pisa->pChild) pl->pChild = copyList(pisa->pChild);
        pAllocs.push_back(pl);
        pl = eval(pl, local_symbols);
        pAllocs.push_back(pl);

        ISAtom *pr = gca(pisa->pNext);
        if (pisa->pNext->pChild) pr->pChild = copyList(pisa->pNext->pChild);
        pAllocs.push_back(pr);
        pr = eval(pr, local_symbols);
        pAllocs.push_back(pr);

        if (pl->t != pr->t) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Error: compare " + m_op + " requires two operands of same type, got: " + tokTypeNames[pl->t] + " and " + tokTypeNames[pr->t];
            for (const ISAtom *p : pAllocs) {
                deleteList((ISAtom *)p, "cmp_2ops 1");
            }
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
                for (const ISAtom *p : pAllocs) {
                    deleteList((ISAtom *)p, "cmp_2ops 2");
                }
                return pRes;
            }
            if (r)
                pRes->val = 1;
            else
                pRes->val = 0;
            for (const ISAtom *p : pAllocs) {
                deleteList((ISAtom *)p, "cmp_2ops 3");
            }
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
                for (const ISAtom *p : pAllocs) {
                    deleteList((ISAtom *)p, "cmp_2ops 4");
                }
                return pRes;
            }
            if (r)
                pRes->val = 1;
            else
                pRes->val = 0;
            for (const ISAtom *p : pAllocs) {
                deleteList((ISAtom *)p, "cmp_2ops 5");
            }
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
                for (const ISAtom *p : pAllocs) {
                    deleteList((ISAtom *)p, "cmp_2ops 6");
                }
                return pRes;
            }
            if (r)
                pRes->val = 1;
            else
                pRes->val = 0;
            for (const ISAtom *p : pAllocs) {
                deleteList((ISAtom *)p, "cmp_2ops 7");
            }
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
                for (const ISAtom *p : pAllocs) {
                    deleteList((ISAtom *)p, "cmp_2ops 8");
                }
                return pRes;
            }
            if (r)
                pRes->val = 1;
            else
                pRes->val = 0;
            for (const ISAtom *p : pAllocs) {
                deleteList((ISAtom *)p, "cmp_2ops 9");
            }
            return pRes;
            break;
        default:
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Can't compare " + m_op + " for type: " + tokTypeNames[pl->t];
            for (const ISAtom *p : pAllocs) {
                deleteList((ISAtom *)p, "cmp_2ops 10");
            }
            return pRes;
        }
    }

    ISAtom *math_2ops(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols, string m_op) {
        // ISAtom *pisa = copyList(pisa_o);
        int res = 0;
        double fres = 0.0;
        ISAtom *p = (ISAtom *)pisa, *pn = nullptr;
        bool fl = false;
        bool first = true;
        ISAtom *pRes = gca();

        if (getListLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Not enough operands for <" + m_op + "> operation";
            return pRes;
        }
        vector<ISAtom *> pAllocs;
        while (p != nullptr) {
            p = chainEval(p, local_symbols, true);
            pAllocs.push_back(p);
            // p->pNext = pn;
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
                            for (auto p : pAllocs) {
                                deleteList(p, "math_2ops 1");
                            }
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
                            for (auto p : pAllocs) {
                                deleteList(p, "math_ops 2");
                            }
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
                        for (auto p : pAllocs) {
                            deleteList(p, "math_ops 3");
                        }
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
                            for (auto p : pAllocs) {
                                deleteList(p, "math_ops 4");
                            }
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
                            for (auto p : pAllocs) {
                                deleteList(p, "math_ops 5");
                            }
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
                        for (auto p : pAllocs) {
                            deleteList(p, "math_ops 6");
                        }
                        return pRes;
                    }
                }
                p = p->pNext;
            } else if (p->t == ISAtom::TokType::NIL) {
                // cout << "SKIP: " << p->t << " " << p->vals << endl;
                p = p->pNext;
            } else {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Op: " + m_op + ", unhandled tokType: " + std::to_string( p->t);
                if (p->t == ISAtom::TokType::ERROR) pRes->vals += ": " + p->vals;
            }
        }
        if (fl) {
            pRes->t = ISAtom::TokType::FLOAT;
            pRes->valf = fres;
        } else {
            pRes->t = ISAtom::TokType::INT;
            pRes->val = res;
        }
        for (auto p : pAllocs) {
            deleteList(p, "math_ops 7");
        }
        return pRes;
    }

    bool is_defined(string name, vector<map<string, ISAtom *>> &local_symbols) {
        if (symbols.find(name) == symbols.end() && funcs.find(name) == funcs.end() && is_defined_local_symbol(name, local_symbols) == false) return false;
        return true;
    }

    bool is_defined_local_symbol(const string &token, const vector<map<string, ISAtom *>> &local_symbols) {
        size_t len = local_symbols.size();
        for (int in = (int)len - 1; in >= 0; in--) {
            if (local_symbols[in].find(token) != local_symbols[in].end()) return true;
        }
        return false;
    }

    ISAtom *get_local_symbol(const string &token, vector<map<string, ISAtom *>> &local_symbols) {
        size_t len = local_symbols.size();
        for (int in = (int)len - 1; in >= 0; in--) {
            if (local_symbols[in].find(token) != local_symbols[in].end()) {
                return copyList(local_symbols[in][token]);
            }
        }
        return nullptr;
    }

    void set_local_symbol(const string &token, ISAtom *val, vector<map<string, ISAtom *>> &local_symbols) {
        size_t len = local_symbols.size();
        if (len == 0) {
            cout << "can't define local variable without local stack!" << endl;
            return;
        }
        for (int in = (int)len - 1; in >= 0; in--) {
            if (local_symbols[in].find(token) != local_symbols[in].end()) {
                deleteList(local_symbols[in][token], "set_local_sym");
                local_symbols[in][token] = val;
                return;
            }
            break;
        }
        local_symbols[len - 1][token] = val;
    }

    void pop_local_symbols(vector<map<string, ISAtom *>> &local_symbols) {
        size_t len = local_symbols.size();
        if (len > 0) {
            int n=0;
            for (auto p : local_symbols[len - 1]) {
                deleteList(p.second, "pop_local_sym_"+std::to_string(n));
                ++n;
            }
            local_symbols.pop_back();
            //cout << "cur local sym-table len = " << local_symbols.size() << endl;
        }
    }

    bool is_defined_symbol(const string name, vector<map<string, ISAtom *>> &local_symbols) {
        if (symbols.find(name) == symbols.end() && is_defined_local_symbol(name, local_symbols) == false) return false;
        return true;
    }

    bool is_defined_func(string name) {
        if (funcs.find(name) == funcs.end()) return false;
        return true;
    }

    ISAtom *makeDefine(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        // ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = gca();
        if (getListLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'define' requires 2 operands: name and value(s)";
            return pRes;
        }
        const ISAtom *pN = pisa;
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
            if (pV->t == ISAtom::TokType::QUOTE) {
                symbols[pN->vals] = copyList(pV, false);
                // gctr[symbols[pN->vals]] = gctr[symbols[pN->vals]] + 1;
            } else {
                ISAtom *pT = chainEval(pV, local_symbols, true);
                symbols[pN->vals] = copyList(pT, false);
                deleteList(pT, "makeDefine 1");
                // gctr[symbols[pN->vals]] = gctr[symbols[pN->vals]] + 1;
            }
            deleteList(pRes, "makeDefine 2");
            return copyList(symbols[pN->vals]);
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
                    ISAtom *pDef = copyList(pN, false);
                    funcs[pNa->vals] = pDef;
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

    ISAtom *makeLocalDefine(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        vector<ISAtom *> pAllocs;
        ISAtom *pRes = gca();
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
        local_symbols.push_back({});
        ISAtom *pDef = pisa->pChild;
        while (pDef && pDef->t != ISAtom::TokType::NIL) {
            if (pDef->t != ISAtom::TokType::BRANCH) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'let' list entries must be list: required are a list of key values pairs: ((k v ), ..) [ ()]";
                for (ISAtom *pA : pAllocs) {
                    deleteList(pA, "makeLocalDefine 1");
                }
                pop_local_symbols(local_symbols);
                return pRes;
            }
            if (getListLen(pDef->pChild) != 3) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'let' list entries must be list of exactly two entries: required are a list of key values pairs: ((k v ), ..) [ ()]";
                for (ISAtom *pA : pAllocs) {
                    deleteList(pA, "makeLocalDefine 2");
                }
                pop_local_symbols(local_symbols);
                return pRes;
            }
            ISAtom *pName = pDef->pChild;
            if (pName->t != ISAtom::TokType::SYMBOL) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'let' list entries must be list of exactly two entries, and first must be a symbol: required are a list of key values pairs: ((k v ), ..) [ ()]";
                for (ISAtom *pA : pAllocs) {
                    deleteList(pA, "makeLocalDefine 3");
                }
                pop_local_symbols(local_symbols);
                return pRes;
            }
            ISAtom *pVal = pName->pNext;
            if (pVal->t == ISAtom::TokType::QUOTE) {
                ISAtom *pSym = copyList(pVal, false);
                set_local_symbol(pName->vals, pSym, local_symbols);
                pAllocs.push_back(pSym);
                // gctr[local_symbols[pName->vals]] = gctr[local_symbols[pName->vals]] + 1;
            } else {
                ISAtom *pT = chainEval(pVal, local_symbols, true);
                ISAtom *pSym = copyList(pT, false);
                set_local_symbol(pName->vals, pSym, local_symbols);
                pAllocs.push_back(pSym);
                deleteList(pT, "makeLocalDefine 4");
                // gctr[local_symbols[pName->vals]] = gctr[local_symbols[pName->vals]] + 1;
            }
            pDef = pDef->pNext;
        }

        if (pisa->pNext) {
            ISAtom *pExpr = pisa->pNext;
            ISAtom *pNa;
            ISAtom *pR;
            pR = nullptr;
            while (pExpr && pExpr->t != ISAtom::TokType::NIL) {
                if (pR) deleteList(pR, "makeLocalDefine 4.1");
                pNa = pExpr->pNext;
                ISAtom *pNc = copyList(pExpr);
                pNc->pNext = nullptr;
                pR = eval(pNc, local_symbols);
                deleteList(pNc, "makeLocalDefine 5");
                pExpr = pNa;
            }
            for (ISAtom *pA : pAllocs) {
                deleteList(pA, "makeLocalDefine 6");
            }
            deleteList(pRes, "makeLocalDefine 7");
            pop_local_symbols(local_symbols);
            return pR;
            // return eval(pV->pNext, local_symbols);
        } else {
            for (ISAtom *pA : pAllocs) {
                deleteList(pA, "makeLocalDefine 8");
            }
            deleteList(pRes, "makeLocalDefine 9");
            pop_local_symbols(local_symbols);
            return copyList(pisa);
        }
    }
    
    ISAtom *setLocalDefine(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'set!' requires 2 params: <existing-local-varname> <val>";
            return pRes;
        }
        string varname = pisa->vals;
        if (!is_defined_local_symbol(varname, local_symbols)) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'set!' requires existing local var name as first param";
            return pRes;
        }
        ISAtom *newVal = eval(pisa->pNext, local_symbols);
        size_t len = local_symbols.size();
            bool bUpd=false;
        for (int in = (int)len - 1; in >= 0; in--) {
            if (local_symbols[in].find(varname) != local_symbols[in].end()) {
                cout << "update " << varname << " from ";
                print(local_symbols[in][varname], local_symbols, ISAtom::DecorType::UNICODE, true);
                cout << " to ";
                deleteList(local_symbols[in][varname], "Set! 1");
                local_symbols[in][varname] = copyList(newVal);
                print(local_symbols[in][varname], local_symbols, ISAtom::DecorType::UNICODE, true);
                cout << endl;
                bUpd=true;
                break;
            }
        }
        if (!bUpd) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'set!' requires existing local var name as first param, couldn't find variable, internal error!";
            deleteList(newVal, "Set! 2");
            return pRes;
        }
        deleteList(pRes, "Set! 3");
        return newVal;
    }

    ISAtom *evalBegin(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        return chainEval(pisa, local_symbols, false);
        }

    ISAtom *evalIf(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 3 && getListLen(pisa) != 4) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'if' requires 2 or 3 operands: <condition> <true-expr> [<false-expr>]";
            return pRes;
        }
        //ISAtom *pTt = gca((ISAtom *)pisa);
        ISAtom *pC = gca((ISAtom *)pisa);
        if (pisa->pChild) pC->pChild=copyList((ISAtom *)pisa->pChild);
        
        ISAtom *pT = gca(pisa->pNext), *pF=nullptr;
        if (pisa->pNext->pChild) pT->pChild = copyList((ISAtom *)pisa->pNext->pChild);
        
        if (pisa->pNext->pNext) {
            pF = gca(pisa->pNext->pNext);
            if (pisa->pNext->pNext->pChild) pF->pChild = copyList(pisa->pNext->pNext->pChild);
        } else {
            pF= nullptr;
        }

        ISAtom *pR = eval(pC, local_symbols);
        if (pR->t != ISAtom::TokType::BOOLEAN) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'if' condition should result in boolean, but we got: " + tokTypeNames[pR->t];
            deleteList(pC, "if 1");
            deleteList(pR, "if 2");
            deleteList(pT, "if 3");
            deleteList(pF, "if 4");
            return pRes;
        }
        if (pR->val) {
            deleteList(pRes, "if 5");
            pRes = eval(pT, local_symbols);
        } else {
            if (pF) {
                deleteList(pRes, "if 6");
                pRes = eval(pF, local_symbols);
            }
        }
        deleteList(pC, "if 7");
        deleteList(pR, "if 8");
        deleteList(pT, "if 9");
        deleteList(pF, "if 10");
        return pRes;
    }

    ISAtom *evalWhile(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'while' requires at least 2 operands: <condition> <expr> [<expr>]...";
            return pRes;
        }
        ISAtom *pC = gca((ISAtom *)pisa);
        if (pisa->pChild) pC->pChild = copyList(pisa->pChild);

        ISAtom *pL = pisa->pNext;
        ISAtom *pN;

        ISAtom *pCR = eval(pC, local_symbols);
        if (pCR->t != ISAtom::TokType::BOOLEAN) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'while' condition should result in boolean, but we got: " + tokTypeNames[pCR->t];
            return pRes;
        }
        int n = 20;
        ISAtom *pCalc = pL;
        while (pCR->val) {
            pL = copyList(pCalc);
            pRes = chainEval(pL, local_symbols, true);
            //while (pL && pL->t != ISAtom::TokType::NIL) {
            //    pN = pL->pNext;
            //    ISAtom *pLc = copyList(pL);
            //    pRes = eval(pLc, local_symbols);
            //    pL = pN;
            //}
            pCR = eval(pC, local_symbols);
            if (pCR->t != ISAtom::TokType::BOOLEAN) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'while' condition should result in boolean, but we got: " + tokTypeNames[pCR->t];
                return pRes;
            }
        }
        return pRes;
    }

    ISAtom *evalPrint(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        // ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = gca();
        if (getListLen(pisa) < 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'print' requires at least 1 operand: <expr> [<expr>]...";
            return pRes;
        }
        // ISAtom *pP = pisa;
        ISAtom *pResS = chainEval(pisa, local_symbols, true);
        print(pResS, local_symbols, ISAtom::DecorType::NONE, false);
        return pResS;
    }

    ISAtom *evalEval(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = gca();
        if (getListLen(pisa) < 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'eval' requires at least 1 operand: <expr> [<expr>]...";
            return pRes;
        }
        ISAtom *pP = pisa;

        ISAtom *pResS = chainEval(pP, local_symbols, true);
        if (pResS->t == ISAtom::TokType::QUOTE) {
            pResS = chainEval(copyList(pResS->pNext), local_symbols, true);
        }

        return pResS;
    }

    ISAtom *listLen(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = gca();
        if (pisa->t == ISAtom::TokType::QUOTE) pisa = pisa->pNext;
        if (getListLen(pisa) != 2 || pisa->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'len' requires one list or quoted list operand";
            deleteList(pisa, "listLen 1");
            return pRes;
        }
        pRes->t = ISAtom::TokType::INT;
        pRes->val = getListLen(pisa->pChild) - 1;
        deleteList(pisa, "listLen 2");
        return pRes;
    }

    ISAtom *listList(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = gca();
        ISAtom *pStart;
        pStart = pRes;
        pRes->t = ISAtom::TokType::QUOTE;
        pRes->pNext = gca();
        pRes = pRes->pNext;
        pRes->t = ISAtom::TokType::BRANCH;
        pRes->pChild = gca(pisa);
        if (pisa->pChild) pRes->pChild->pChild = copyList(pisa->pChild);
        pRes = pRes->pChild;
        pisa = pisa->pNext;
        while (pisa) {
            pRes->pNext = gca(pisa);
            if (pisa->pChild) pRes->pNext->pChild = copyList(pisa->pChild);
            pRes = pRes->pNext;
            pisa = pisa->pNext;
        }
        deleteList(pisa, "listList 1");
        return pStart;
    }

    ISAtom *listCons(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = gca(), *pStart;
        pStart = pRes;
        if (getListLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cons' requires two args";
            return pRes;
        }
        ISAtom *c1 = pisa;
        ISAtom *c2 = eval(pisa->pNext, local_symbols, true);
        if (c2->t == ISAtom::TokType::QUOTE) c2 = c2->pNext;  // eval(c2->pNext, local_symbols, true);  // XXX Is this consistent?!
        if (c2->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cons' 2nd arg needs to eval to list (e.g. quoted list)";
            return pRes;
        }
        pRes->t = ISAtom::TokType::QUOTE;
        pRes->pNext = gca();
        pRes = pRes->pNext;
        pRes->t = ISAtom::TokType::BRANCH;
        pRes->pChild = gca(c1);
        if (c1->pChild) pRes->pChild->pChild = copyList(c1->pChild);
        pRes = pRes->pChild;
        pisa = c2->pChild;
        while (pisa) {
            pRes->pNext = gca(pisa);
            if (pisa->pChild) pRes->pNext->pChild = copyList(pisa->pChild);
            pRes = pRes->pNext;
            pisa = pisa->pNext;
        }
        return pStart;
    }

    ISAtom *listCar(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'car' requires one list arg";
            return pRes;
        }
        ISAtom *pC = pisa;
        if (pC->t == ISAtom::TokType::QUOTE)
            pC = pC->pNext;
        else {
            pC = eval(pC, local_symbols);
            if (pC->t == ISAtom::TokType::QUOTE) pC = pC->pNext;
        }
        if (pC->t == ISAtom::TokType::BRANCH)
            pC = pC->pChild;
        else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'car' requires one list arg";
            return pRes;
        }
        pRes = gca(pC);
        if (pC->pChild) pRes->pChild = copyList(pC->pChild);
        return pRes;
    }

    ISAtom *listCdr(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cdr' requires one list arg";
            return pRes;
        }
        pRes = copyList(pisa);
        ISAtom *pC = pRes;
        if (pC->t == ISAtom::TokType::QUOTE) pC = pC->pNext;
        if (pC->t == ISAtom::TokType::BRANCH) {
            pC->pChild = pC->pChild->pNext;
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cdr' requires list as second arg";
            return pRes;
        }
        return pRes;
    }

    ISAtom *evalLoad(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'load' requires one string operand, a filename, got: " + std::to_string(getListLen(pisa));
            deleteList(pisa, "evalLoad 1");
            return pRes;
        }
        ISAtom *pP = pisa;
        if (pP->t != ISAtom::TokType::STRING) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'load' requires a string operand, a filename";
            deleteList(pisa, "evalLoad 2");
            return pRes;
        }
        
        char buf[129];
        size_t nb;
        string cmd = "";
        FILE *fp = fopen(pP->vals.c_str(), "r");
        if (fp) {
            while (!feof(fp)) {
                nb = fread(buf, 1, 128, fp);
                buf[nb] = 0;
                cmd += buf;
            }
        }
        if (cmd != "") {
            ISAtom *pisa_p = parse(cmd);
            ISAtom *pisa_res = chainEval(pisa_p, local_symbols, false);
            deleteList(pisa, "evalLoad 3");
            deleteList(pisa_p, "evalLoad 4");
            deleteList(pRes, "evalLoad 5");
            return pisa_res;
        } else {
            pRes->t=ISAtom::TokType::ERROR;
            pRes->vals="Could not read file: "+pisa->vals;
            deleteList(pisa, "evalLoad 6");
            return pRes;
        }
    }

    bool is_inbuilt(string funcName) {
        if (inbuilts.find(funcName) == inbuilts.end()) return false;
        return true;
    }

    ISAtom *eval_symbol(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *p, *pn, *pRet;
        ISAtom *pisa = copyList(pisa_o);
        if (is_defined_symbol(pisa->vals, local_symbols)) {
            if (is_defined_local_symbol(pisa->vals, local_symbols))
                p = get_local_symbol(pisa->vals, local_symbols);
            else if (symbols.find(pisa->vals) != symbols.end()) {
                p = symbols[pisa->vals];
            } else {
                pRet = gca();
                pRet->t = ISAtom::TokType::ERROR;
                pRet->vals = "Invalid state when resolving symbol: " + pisa->vals;
                deleteList(pisa, "eval_symbol 1");
                return pRet;
            }
            while (p->t == ISAtom::TokType::SYMBOL) {
                if (is_defined_symbol(p->vals, local_symbols)) {
                    if (is_defined_local_symbol(p->vals, local_symbols)) {
                        pn = get_local_symbol(pisa->vals, local_symbols);
                    } else {
                        pn = symbols[p->vals];
                    }
                    deleteList(p, "eval_symbol 2");
                    p=pn;
                } else {
                    break;
                }
            }
            deleteList(pisa, "eval_symbol 4");
            return p; // copyList(p);
        } else {
            pRet = gca();
            pRet->t = ISAtom::TokType::ERROR;
            pRet->vals = "Undefined symbol: " + pisa->vals;
            deleteList(pisa, "eval_symbol 3");
            return pRet;
        }
    }

    ISAtom *eval_func(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *p, *pn, *pDef;
        if (is_defined_func(pisa->vals)) {
            local_symbols.push_back({});
            pDef = funcs[pisa->vals];
            ISAtom *pNa = pDef->pChild;
            vector<string> localNames;
            int n = 0;
            bool err = false;
            string func_name;
            ISAtom *pRes = gca();
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
                    pop_local_symbols(local_symbols);
                    return pRes;
                    break;
                }
            }
            if (getListLen(pisa) - 2 != n - 1) {  // Pisa: fun-name and nil: 2, n: fun-name: 1
                err = true;
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Function " + func_name + " requires " + std::to_string(n - 1) + " arguments, " + std::to_string(getListLen(pisa) - 2) + " given";
                pop_local_symbols(local_symbols);
                return pRes;
            }
            ISAtom *pCurVar;
            const ISAtom *pInp = pisa;
            for (auto var_name : localNames) {
                pInp = pInp->pNext;
                
                cout << "Eval param ";
                print(pInp, local_symbols, ISAtom::DecorType::UNICODE, true);
                
                ISAtom *pT = eval(pInp, local_symbols);
                
                cout << " to ";
                print(pT, local_symbols, ISAtom::DecorType::UNICODE, true);
                cout << endl;
                
                // pT->pNext = nullptr;
                set_local_symbol(var_name, pT, local_symbols);
                deleteList(pT, "func_eval set vars");
            }
            p = eval(pDef->pNext, local_symbols);
            pop_local_symbols(local_symbols);
            deleteList(pRes, "eval_func");
            return p;
        } else {
            cout << "undefined function! " << pisa->vals << endl;
            return copyList(pisa);
        }
    }

    ISAtom *eval(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols, bool func_only = false) {
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *pN, *pRet;  //, *pReti;

        pN = pisa->pNext;
        
        switch (pisa->t) {
        case ISAtom::TokType::QUOTE:
            pRet = copyList(pN);
            deleteList(pisa, "eval 1");
            return pRet;
            break;
        case ISAtom::TokType::BRANCH:
            pRet = eval(pisa->pChild, local_symbols, true);
            deleteList(pisa, "eval 2");
            return pRet;
            break;
        case ISAtom::TokType::SYMBOL:
            if (is_inbuilt(pisa->vals)) {
                pRet = inbuilts[pisa->vals](pisa->pNext, local_symbols);
            } else if (is_defined_func(pisa->vals)) {
                pRet = eval_func(pisa, local_symbols);
            } else if (!func_only && is_defined_symbol(pisa->vals, local_symbols)) {
                pRet = eval_symbol(pisa, local_symbols);
            } else {
                pRet = gca();
                pRet->t = ISAtom::TokType::ERROR;
                pRet->vals = "Undefined function: " + pisa->vals;
            }
            deleteList(pisa, "eval 3");
            return pRet;
            break;
        case ISAtom::TokType::ERROR:
            pRet = pisa;
            return pRet;
            break;
        default:
            if (func_only) {
                pRet = gca();
                pRet->t = ISAtom::TokType::ERROR;
                pRet->vals = "Undefined expression: " + pisa->str();
                deleteList(pisa, "eval 5");
                return pRet;
            } else {
                pRet = pisa;
                pRet->pNext = nullptr;
                return pRet;
            }
            break;
        }
    }

    ISAtom *chainEval(const ISAtom *pisa_o, vector<map<string, ISAtom *>> &local_symbols, bool bChainResult) {
        size_t start_index = gc_size();
        
        ISAtom *pisa = copyList(pisa_o);
        ISAtom *p = pisa, *pn;  //  *pn, *pRes, *pPrev, *pCur;
        ISAtom *pCE = nullptr, *pCEi, *pCE_c=nullptr;
        bool is_quote = false;

        vector<ISAtom *> pAllocs;
        pAllocs.push_back(pisa);
        while (p) {
            if (p->t == ISAtom::TokType::NIL) {
                break;
            }
            pn = p->pNext;
            p->pNext = nullptr;
            switch (p->t) {
            case ISAtom::TokType::QUOTE:
                if (is_quote) {
                    pCEi = copyList(p);
                    pAllocs.push_back(pCEi);
                    is_quote = false;
                } else {
                    pCEi = nullptr;  // copyList(p->pNext);  // nullptr;
                    is_quote = true;
                }
                break;
            case ISAtom::TokType::SYMBOL:
                if (is_quote) {
                    pCEi = copyList(p);
                    is_quote = false;
                } else {
                    pCEi = eval_symbol(p, local_symbols);
                }
                pAllocs.push_back(pCEi);
                break;
            case ISAtom::TokType::BRANCH:
                if (is_quote) {
                    pCEi = copyList(p);
                    is_quote = false;
                } else {
                    pCEi = eval(p, local_symbols, true);
                }
                pAllocs.push_back(pCEi);
                break;
            default:
                is_quote = false;
                pCEi = gca(p);
                if (p->pChild) pCEi->pChild = copyList(p->pChild);
                for (auto p : pAllocs) {
                    if (p == pCEi) {
                        cout << "It's already in!" << endl;
                    }}
                pAllocs.push_back(pCEi);
                break;
            }
            p->pNext = pn;
            p = pn;
            if (pCEi) {
                if (!bChainResult) {
                    if (pCE) deleteList(pCE, "chain 1");
                    pCE= pCEi;
                } else {
                     if (pCEi->pNext) {cout << "pCEi->pNext shouldn't be set! [check quote case]" << endl; }
                     if (!pCE) {
                         pCE_c = copyList(pCEi);
                         pCE = pCE_c;
                     } else {
                         pCE_c->pNext = copyList(pCEi);
                         pCE_c = pCE_c->pNext;
                     }
                     // if (pT->pChild) deleteList(pT->pChild, "chainEval 3");
                     // delete pT;
                    
                }
            }
        }
        int n=0;
        for (auto p : pAllocs) {
            deleteList(p, "chainEval 4-"+std::to_string(n));
            ++n;
        }
        //cout << "LOCAL[" << start_index << "] [" << gc_size() << "]" << endl;
        return pCE;
    }
};

}  // namespace insch
