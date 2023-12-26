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
    bool memDbg = true;

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
        inbuilts["stringify"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalStringify(pisa, local_symbols); };
        inbuilts["listfunc"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalListfunc(pisa, local_symbols); };
        inbuilts["load"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalLoad(pisa, local_symbols); };
        inbuilts["parse"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalParse(pisa, local_symbols); };
        inbuilts["quote"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalQuote(pisa, local_symbols); };
        inbuilts["list"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listList(pisa, local_symbols); };
        inbuilts["cons"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listCons(pisa, local_symbols); };
        inbuilts["car"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listCar(pisa, local_symbols); };
        inbuilts["cdr"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listCdr(pisa, local_symbols); };
        inbuilts["length"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listLen(pisa, local_symbols); };
        inbuilts["append"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listAppend(pisa, local_symbols); };
        inbuilts["reverse"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listReverse(pisa, local_symbols); };
        inbuilts["eval"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalEval(pisa, local_symbols); };
    }

    ISAtom *gca(const ISAtom *src = nullptr, bool bRegister = true) {
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

    void gcd(ISAtom *pisa, string context, bool bUnregistered = false) {
        // XXX DOES NOT DELETE!
        if (!pisa) return;
        auto pos = gctr.find(pisa);
        if (pos == gctr.end()) {
            if (memDbg && !bUnregistered) {
                cout << "Trying to delete unaccounted allocation at " << context << " of: " << pisa << ", ";
                vector<map<string, ISAtom *>> lh;
                lh.push_back({});
                print(pisa, lh, ISAtom::DecorType::UNICODE, true);
                cout << endl;

                if (gctr_del_ctx.find(pisa) != gctr_del_ctx.end()) {
                    cout << "This has been deleted at context: " << gctr_del_ctx[pisa] << endl;
                }
            } else {
                delete pisa;
            }
        } else {
            gctr_del_ctx[pisa] = context;
            // cout << "DELETE: " << context  << endl;
            delete pisa;
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

    void deleteList(ISAtom *pisa, string context, bool bUnregistered = false) {
        if (pisa == nullptr) return;
        deleteList(pisa->pChild, context + "_C", bUnregistered);
        deleteList(pisa->pNext, context + "_N", bUnregistered);
        gcd(pisa, context, bUnregistered);
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

    ISAtom *parse(string &input, ISAtom *pNode, int &level) {
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
            int lvl;
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
                    lvl = level + 1;
                    pCurNode->pChild = parse(input, nullptr, lvl);
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
                case '\'':  // Quote
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
        bool showParse = false;
        if (showParse) {
            vector<map<string, ISAtom *>> ls = {};
            if (pStart) {
                cout << "Parse: (" << level << ") ";
                print(pStart, ls, ISAtom::DecorType::UNICODE, true);
                cout << endl;
            }
        }
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

    string stringify(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols, ISAtom::DecorType decor, bool bAutoSeparators) {
        string out = pisa->str(decor);
        ISAtom *pN = pisa->pNext;
        if (decor) {
            if (is_inbuilt(pisa->vals) || is_defined_func(pisa->vals)) out = "ⓕ " + out;
            if (is_defined_symbol(pisa->vals, local_symbols)) out = "ⓢ " + out;
        }
        if (pisa->pChild != nullptr) {
            out += stringify(pisa->pChild, local_symbols, decor, bAutoSeparators);
            out += ")";
        }
        if (pN != nullptr) {
            if (bAutoSeparators && pN->t != ISAtom::TokType::QUOTE) {
                if (pisa->t != ISAtom::TokType::QUOTE) out += " ";
            }
            out += stringify(pN, local_symbols, decor, bAutoSeparators);
        }
        return out;
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
        ISAtom *pev = chainEval(pisa, local_symbols, true);
        pAllocs.push_back(pev);

        ISAtom *pl = gca((ISAtom *)pev);
        if (pev->pChild) pl->pChild = copyList(pev->pChild);
        pAllocs.push_back(pl);

        ISAtom *pr = gca(pev->pNext);
        if (pev->pNext->pChild) pr->pChild = copyList(pev->pNext->pChild);
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
        string sres = "";
        ISAtom *p = (ISAtom *)pisa, *pn = nullptr;
        // bool fl = false;
        ISAtom::TokType dt = ISAtom::TokType::NIL;
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

            // print(p, local_symbols, ISAtom::DecorType::UNICODE, true);
            // cout << endl;

            pAllocs.push_back(p);
            // p->pNext = pn;
            switch (p->t) {
            case ISAtom::TokType::INT:
                if (first) {
                    res = p->val;
                    dt = ISAtom::TokType::INT;
                    first = false;
                } else {
                    if (m_op == "+") {
                        switch (dt) {
                        case ISAtom::TokType::INT:
                            res += p->val;
                            break;
                        case ISAtom::TokType::FLOAT:
                            fres += p->val;
                            break;
                        case ISAtom::TokType::STRING:
                            sres += p->str();
                            break;
                        default:
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "Unsupported operand-type for '+': " + tokTypeNames[dt];
                            for (auto p : pAllocs) {
                                deleteList(p, "math_2ops +1");
                            }
                            return pRes;
                            break;
                        }
                    } else if (m_op == "-") {
                        switch (dt) {
                        case ISAtom::TokType::INT:
                            res -= p->val;
                            break;
                        case ISAtom::TokType::FLOAT:
                            fres -= p->val;
                            break;
                        default:
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "Unsupported operand-type for '-': " + tokTypeNames[dt];
                            for (auto p : pAllocs) {
                                deleteList(p, "math_2ops -1");
                            }
                            return pRes;
                            break;
                        }
                    } else if (m_op == "*") {
                        string smult = "";
                        switch (dt) {
                        case ISAtom::TokType::INT:
                            res *= p->val;
                            break;
                        case ISAtom::TokType::FLOAT:
                            fres *= p->val;
                            break;
                        case ISAtom::TokType::STRING:
                            for (auto i = 0; i < p->val; i++)
                                smult += sres;
                            sres = smult;
                            break;
                        default:
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "Unsupported operand-type for '*': " + tokTypeNames[dt];
                            for (auto p : pAllocs) {
                                deleteList(p, "math_2ops *1");
                            }
                            return pRes;
                            break;
                        }
                    } else if (m_op == "/") {
                        if (p->val == 0) {
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "DIV/ZERO!";
                            for (auto p : pAllocs) {
                                deleteList(p, "math_2ops /1-0");
                            }
                            return pRes;
                        } else {
                            switch (dt) {
                            case ISAtom::TokType::INT:
                                res /= p->val;
                                break;
                            case ISAtom::TokType::FLOAT:
                                fres /= p->val;
                                break;
                            default:
                                pRes->t = ISAtom::TokType::ERROR;
                                pRes->vals = "Unsupported operand-type for '/': " + tokTypeNames[dt];
                                for (auto p : pAllocs) {
                                    deleteList(p, "math_2ops /1");
                                }
                                return pRes;
                                break;
                            }
                        }
                    } else if (m_op == "%") {
                        if (p->val == 0) {
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "DIV/ZERO!";
                            for (auto p : pAllocs) {
                                deleteList(p, "math_ops %1-0");
                            }
                            return pRes;
                        } else {
                            switch (dt) {
                            case ISAtom::TokType::INT:
                                res = res % p->val;
                                break;
                            case ISAtom::TokType::FLOAT:
                                fres = (int)fres % p->val;
                                break;
                            default:
                                pRes->t = ISAtom::TokType::ERROR;
                                pRes->vals = "Unsupported operand-type for '%': " + tokTypeNames[dt];
                                for (auto p : pAllocs) {
                                    deleteList(p, "math_2ops %1");
                                }
                                return pRes;
                                break;
                            }
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
                break;
            case ISAtom::TokType::FLOAT:
                if (first) {
                    fres = p->valf;
                    first = false;
                    dt = ISAtom::TokType::FLOAT;
                } else {
                    if (m_op == "+") {
                        switch (dt) {
                        case ISAtom::TokType::INT:
                            fres = res + p->valf;
                            dt = ISAtom::TokType::FLOAT;
                            break;
                        case ISAtom::TokType::FLOAT:
                            fres += p->valf;
                            break;
                        case ISAtom::TokType::STRING:
                            sres += p->str();
                            break;
                        default:
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "Unsupported operand-type for '+': " + tokTypeNames[dt];
                            for (auto p : pAllocs) {
                                deleteList(p, "math_2ops +2");
                            }
                            return pRes;
                            break;
                        }
                    } else if (m_op == "-") {
                        switch (dt) {
                        case ISAtom::TokType::INT:
                            fres = res - p->valf;
                            dt = ISAtom::TokType::FLOAT;
                            break;
                        case ISAtom::TokType::FLOAT:
                            fres -= p->valf;
                            break;
                        default:
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "Unsupported operand-type for '-': " + tokTypeNames[dt];
                            for (auto p : pAllocs) {
                                deleteList(p, "math_2ops -2");
                            }
                            return pRes;
                            break;
                        }
                    } else if (m_op == "*") {
                        switch (dt) {
                        case ISAtom::TokType::INT:
                            fres = res * p->valf;
                            dt = ISAtom::TokType::FLOAT;
                            break;
                        case ISAtom::TokType::FLOAT:
                            fres *= p->valf;
                            break;
                        default:
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "Unsupported operand-type for '*': " + tokTypeNames[dt];
                            for (auto p : pAllocs) {
                                deleteList(p, "math_2ops *2");
                            }
                            return pRes;
                            break;
                        }
                    } else if (m_op == "/") {
                        if (p->valf == 0) {
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "DIV/ZERO!";
                            for (auto p : pAllocs) {
                                deleteList(p, "math_ops /2-0");
                            }
                            return pRes;
                        } else {
                            switch (dt) {
                            case ISAtom::TokType::INT:
                                fres = res / p->valf;
                                dt = ISAtom::TokType::FLOAT;
                                break;
                            case ISAtom::TokType::FLOAT:
                                fres /= p->valf;
                                break;
                            default:
                                pRes->t = ISAtom::TokType::ERROR;
                                pRes->vals = "Unsupported operand-type for '/': " + tokTypeNames[dt];
                                for (auto p : pAllocs) {
                                    deleteList(p, "math_2ops /2");
                                }
                                return pRes;
                                break;
                            }
                        }
                    } else if (m_op == "%") {
                        if (p->valf == 0) {
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "DIV/ZERO!";
                            for (auto p : pAllocs) {
                                deleteList(p, "math_ops %2-0");
                            }
                            return pRes;
                        } else {
                            switch (dt) {
                            case ISAtom::TokType::INT:
                                fres = res % (int)p->valf;
                                dt = ISAtom::TokType::FLOAT;
                                break;
                            case ISAtom::TokType::FLOAT:
                                fres = (int)fres % (int)p->valf;
                                break;
                            default:
                                pRes->t = ISAtom::TokType::ERROR;
                                pRes->vals = "Unsupported operand-type for '%': " + tokTypeNames[dt];
                                for (auto p : pAllocs) {
                                    deleteList(p, "math_2ops %2");
                                }
                                return pRes;
                                break;
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
                break;
            case ISAtom::TokType::STRING:
                if (first) {
                    sres = p->vals;
                    first = false;
                    dt = ISAtom::TokType::STRING;
                } else {
                    if (m_op == "+") {
                        switch (dt) {
                        case ISAtom::TokType::STRING:
                            sres = sres + p->vals;
                            break;
                        default:
                            pRes->t = ISAtom::TokType::ERROR;
                            pRes->vals = "Unsupported operand-type for 'String-+': " + tokTypeNames[dt];
                            for (auto p : pAllocs) {
                                deleteList(p, "math_2ops %2");
                            }
                            return pRes;
                            break;
                        }
                    }
                }
                break;

            case ISAtom::TokType::NIL:
                break;
            default:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Op: " + m_op + ", unhandled tokType: " + std::to_string(p->t);
                if (p->t == ISAtom::TokType::ERROR) pRes->vals += ": " + p->vals;
                for (auto p : pAllocs) {
                    deleteList(p, "math_2ops +1");
                }
                return pRes;
                break;
            }
            p = p->pNext;
        }
        switch (dt) {
        case ISAtom::TokType::INT:
            pRes->t = ISAtom::TokType::INT;
            pRes->val = res;
            break;
        case ISAtom::TokType::FLOAT:
            pRes->t = ISAtom::TokType::FLOAT;
            pRes->valf = fres;
            break;
        case ISAtom::TokType::STRING:
            pRes->t = ISAtom::TokType::STRING;
            pRes->vals = sres;
            break;
        default:
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Data type not implemented as math result: " + tokTypeNames[dt];
            break;
        }
        for (auto p : pAllocs) {
            deleteList(p, "math_ops 7");
        }
        return pRes;
    }

    bool
    is_defined(string name, vector<map<string, ISAtom *>> &local_symbols) {
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
            int n = 0;
            for (auto p : local_symbols[len - 1]) {
                deleteList(p.second, "pop_local_sym_" + std::to_string(n) + " " + p.first);
                ++n;
            }
            local_symbols.pop_back();
            // cout << "cur local sym-table len = " << local_symbols.size() << endl;
        }
    }

    bool is_defined_symbol(const string name, vector<map<string, ISAtom *>> &local_symbols) {
        if (symbols.find(name) == symbols.end() && is_defined_local_symbol(name, local_symbols) == false) return false;
        return true;
    }

    bool is_defined_global_symbol(const string name) {
        if (symbols.find(name) == symbols.end()) return false;
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
                ISAtom *pT = gca(pV->pNext, false);
                if (pV->pNext && pV->pNext->pChild) pT->pChild = copyList(pV->pNext->pChild, false);
                symbols[pN->vals] = pT;
            } else {
                ISAtom *pT = chainEval(pV, local_symbols, true);
                symbols[pN->vals] = copyList(pT, false);
                deleteList(pT, "makeDefine 1");  // XXX only 2nd argument? See QUOTE case
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

    void deleteDefine(string &name) {
        if (is_defined_func(name)) {
            deleteList(funcs[name], "DeleteFuncDefine " + name, true);
        }
        if (is_defined_global_symbol(name)) {
            deleteList(symbols[name], "DeleteSymDefine " + name, true);
        }
    }

    void deleteAllDefines() {
        for (auto sp : symbols) {
            string name = sp.first;
            deleteDefine(name);
        }
        for (auto sp : funcs) {
            string name = sp.first;
            deleteDefine(name);
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
                ISAtom *pSym = gca(pVal->pNext);
                if (pVal->pNext && pVal->pNext->pChild) pSym->pChild = copyList(pVal->pNext->pChild);
                set_local_symbol(pName->vals, pSym, local_symbols);
                // gctr[local_symbols[pName->vals]] = gctr[local_symbols[pName->vals]] + 1;
            } else {
                ISAtom *pT = chainEval(pVal, local_symbols, true);
                ISAtom *pSym = copyList(pT);
                set_local_symbol(pName->vals, pSym, local_symbols);
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
                /*
                pNa = pExpr->pNext;
                ISAtom *pNc = copyList(pExpr);
                pNc->pNext = nullptr;
                pR = eval(pNc, local_symbols);
                deleteList(pNc, "makeLocalDefine 5");
                pExpr = pNa;
                */
                pR = eval(pExpr, local_symbols);
                pExpr = pExpr->pNext;
            }
            for (ISAtom *pA : pAllocs) {
                deleteList(pA, "makeLocalDefine 6");
            }
            deleteList(pRes, "makeLocalDefine 7");
            pop_local_symbols(local_symbols);
            if (!pR) pR = gca();  // XXX Maybe set to last symbol?!
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
        bool bUpd = false;
        for (int in = (int)len - 1; in >= 0; in--) {
            if (local_symbols[in].find(varname) != local_symbols[in].end()) {
                deleteList(local_symbols[in][varname], "Set! 1");
                local_symbols[in][varname] = copyList(newVal);
                bUpd = true;
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
        // ISAtom *pTt = gca((ISAtom *)pisa);
        ISAtom *pC = gca((ISAtom *)pisa);
        if (pisa->pChild) pC->pChild = copyList((ISAtom *)pisa->pChild);

        ISAtom *pT = gca(pisa->pNext), *pF = nullptr;
        if (pisa->pNext->pChild) pT->pChild = copyList((ISAtom *)pisa->pNext->pChild);

        if (pisa->pNext->pNext) {
            pF = gca(pisa->pNext->pNext);
            if (pisa->pNext->pNext->pChild) pF->pChild = copyList(pisa->pNext->pNext->pChild);
        } else {
            pF = nullptr;
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
            deleteList(pC, "while 1");
            deleteList(pCR, "while 2");
            return pRes;
        }
        int n = 20;
        ISAtom *pLast = nullptr;
        while (pCR->val) {
            if (pLast) deleteList(pLast, "while 3");
            pLast = chainEval(pL, local_symbols, true);

            deleteList(pCR, "while 3.1");
            pCR = eval(pC, local_symbols);
            if (pCR->t != ISAtom::TokType::BOOLEAN) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'while' condition should result in boolean, but we got: " + tokTypeNames[pCR->t];
                deleteList(pC, "while 4");
                deleteList(pCR, "while 5");
                deleteList(pLast, "while 6");
                return pRes;
            }
        }
        deleteList(pC, "while 7");
        deleteList(pCR, "while 8");
        deleteList(pRes, "while 9");
        return pLast;
    }

    ISAtom *evalPrint(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        // ISAtom *pisa = copyList(pisa_o);
        if (getListLen(pisa) < 1) {
            ISAtom *pRes = gca();
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'print' requires at least 1 operand: <expr> [<expr>]...";
            return pRes;
        }
        // ISAtom *pP = pisa;
        ISAtom *pResS = chainEval(pisa, local_symbols, true);
        print(pResS, local_symbols, ISAtom::DecorType::NONE, false);
        deleteList(pResS, "evalPrint 1");
        pResS = gca();
        return pResS;
    }

    ISAtom *evalStringify(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        // ISAtom *pisa = copyList(pisa_o);
        if (getListLen(pisa) < 1) {
            ISAtom *pRes = gca();
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'stringify' requires at least 1 operand: <expr> [<expr>]...";
            return pRes;
        }
        ISAtom *pResS = chainEval(pisa, local_symbols, true);
        string st = stringify(pResS, local_symbols, ISAtom::DecorType::NONE, true);
        deleteList(pResS, "evalStringify 1");
        ISAtom *pRes = gca();
        pRes->t = ISAtom::TokType::STRING;
        pRes->vals = st;
        return pRes;
    }

    ISAtom *evalListfunc(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) < 1) {
            ISAtom *pRes = gca();
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listfunc' requires at least 1 operand: <expr> [<expr>]...";
            return pRes;
        }
        ISAtom *pP;
        if (pisa->t == ISAtom::TokType::STRING || pisa->t == ISAtom::TokType::SYMBOL) {
            pP = copyList(pisa);
        } else {
            pP = chainEval(pisa, local_symbols, true);
            if (pP->t != ISAtom::TokType::STRING && pP->t != ISAtom::TokType::SYMBOL) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'listfunc' requires a string that is a function name";
                deleteList(pP, "Listfunc 0");
                return pRes;
            }
        }
        string funcname = pP->vals;
        deleteList(pP, "Listfunc 1");
        if (!is_defined_func(funcname)) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listfunc' function >" + funcname + "< is not defined";
            return pRes;
        }
        string listfunc = stringify(funcs[funcname], local_symbols, ISAtom::DecorType::NONE, true);
        pRes->t = ISAtom::TokType::STRING;
        pRes->vals = listfunc;
        return pRes;
    }

    ISAtom *evalQuote(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = copyList(pisa);
        return pRes;
    }

    ISAtom *evalEval(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        if (getListLen(pisa) < 1) {
            ISAtom *pRes = gca();
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'eval' requires at least 1 operand: <expr> [<expr>]...";
            return pRes;
        }
        ISAtom *p;
        if (pisa->t == ISAtom::TokType::QUOTE)
            p = pisa->pNext;
        else
            p = (ISAtom *)pisa;

        ISAtom *pResS = chainEval(p, local_symbols, true);
        ISAtom *pResS_r = chainEval(pResS, local_symbols, true);
        deleteList(pResS, "evalEval 1");
        pResS = pResS_r;
        /*
        cout << endl
             << "pre-eval: ";
        print(p, local_symbols, ISAtom::DecorType::UNICODE, true);
        cout << " -> after: >";
        print(pResS, local_symbols, ISAtom::DecorType::UNICODE, true);
        cout << "<" << endl;
        */
        return pResS;
    }

    ISAtom *listLen(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *p = (ISAtom *)pisa;

        ISAtom *pls = chainEval(pisa, local_symbols, true);

        if (getListLen(pls) != 1 || pls->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'len' requires or quoted list operand, len=" + std::to_string(getListLen(pls)) + ", got type: " + tokTypeNames[pls->t];
            deleteList(pls, "listLen 1");
            return pRes;
        }
        pRes->t = ISAtom::TokType::INT;
        pRes->val = getListLen(pls->pChild) - 1;
        deleteList(pls, "listLen 2");
        return pRes;
    }

    ISAtom *listList(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pStart;
        ISAtom *pRes = gca();
        pStart = pRes;

        ISAtom *pls = chainEval(pisa, local_symbols, true);

        // pRes->t = ISAtom::TokType::QUOTE;
        // pRes->pNext = gca();
        // pRes = pRes->pNext;
        pRes->t = ISAtom::TokType::BRANCH;
        pRes->pChild = copyList(pls);
        deleteList(pls, "listList 1");
        return pStart;
    }

    ISAtom *listCons(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pStart = pRes;
        if (getListLen(pisa) < 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cons' requires two args";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        ISAtom *c1 = gca(pls);
        if (pls->pChild) c1->pChild = copyList(pls->pChild);

        ISAtom *c2 = copyList(pls->pNext);
        deleteList(pls, "listCons 0");
        // if (c2->t == ISAtom::TokType::QUOTE) {
        //     ISAtom *c2_n = copyList(c2->pNext);
        //     deleteList(c2, "listCons 1");
        //     c2 = c2_n;
        // }
        if (c2->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cons' 2nd arg needs to eval to list (e.g. quoted list)";
            deleteList(c2, "listCons 2");
            deleteList(c1, "listCons 2.1");
            return pRes;
        }
        // pRes->t = ISAtom::TokType::QUOTE;
        // pRes->pNext = gca();
        // pRes = pRes->pNext;
        pRes->t = ISAtom::TokType::BRANCH;
        pRes->pChild = gca(c1);
        pRes = pRes->pChild;
        if (c1->pChild) pRes->pChild = copyList(c1->pChild);
        pRes->pNext = copyList(c2->pChild);
        deleteList(c2, "cons 3");
        deleteList(c1, "cons 3.1");
        // pisa = c2->pChild;
        // while (pisa) {
        //     pRes->pNext = gca(pisa);
        //     if (pisa->pChild) pRes->pNext->pChild = copyList(pisa->pChild);
        //     pRes = pRes->pNext;
        //     pisa = pisa->pNext;
        // }
        return pStart;
    }

    ISAtom *listCar(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'car' requires one list arg";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (pls->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'car' requires one arg as list";
            deleteList(pls, "car 1");
            return pRes;
        }
        ISAtom *pCar = gca(pls->pChild);
        if (pls->pChild->pChild) pCar->pChild = copyList(pls->pChild->pChild);
        deleteList(pRes, "car 2");
        deleteList(pls, "car 3");
        return pCar;
    }

    ISAtom *listCdr(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cdr' requires one list arg";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (pls->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cdr' requires one arg as list";
            deleteList(pls, "cdr 1");
            return pRes;
        }
        if (!pls->pChild->pNext) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cdr' requires one arg as non-empty list";
            deleteList(pls, "cdr 2");
            return pRes;
        }
        ISAtom *pCdr = gca();
        pCdr->t = ISAtom::TokType::BRANCH;
        pCdr->pChild = copyList(pls->pChild->pNext);
        deleteList(pRes, "cdr 3");
        deleteList(pls, "cdr 4");
        return pCdr;
    }

    ISAtom *listAppend(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listAppend' requires two args";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pisa) != 3) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listAppend' (aft. eval) requires two args";
            deleteList(pls, "listAppend 1");
            return pRes;
        }
        if (pls->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listAppend' requires first arg to be a list";
            deleteList(pls, "listAppend 2");
            return pRes;
        }

        ISAtom *pIns = gca(pls->pNext);
        if (pls->pNext->pChild) pIns->pChild = copyList(pls->pNext->pChild);

        ISAtom *pApp = gca(pls);
        if (pls->pChild) pApp->pChild = copyList(pls->pChild);

        ISAtom *p = pApp->pChild;
        ISAtom *pL = nullptr;
        bool first = true;
        bool ins = false;
        while (p) {
            if (p->t == ISAtom::TokType::NIL) {
                if (first) {
                    pApp->pChild = pIns;
                    pIns->pNext = p;
                    ins = true;
                    break;
                } else {
                    pL->pNext = pIns;
                    pIns->pNext = p;
                    ins = true;
                    break;
                }
            } else {
                first = false;
                pL = p;
                p = p->pNext;
            }
        }

        deleteList(pls, "listAppend 4");
        if (!ins) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Append failed, cannot find terminating NIL of source-list";
            deleteList(pApp, "no-nil-append");
            return pRes;
        }

        deleteList(pRes, "listAppend 3");
        return pApp;
    }

    ISAtom *listReverse(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listReverse' requires one args";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listReverse' (aft. eval) requires one args";
            return pRes;
        }
        if (pls->t != ISAtom::TokType::BRANCH) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listReverse' requires first arg to be a list";
            deleteList(pls, "listReverse 2");
            return pRes;
        }
        vector<ISAtom *> lst;
        lst.push_back(pls->pChild);
        ISAtom *p = pls->pChild;
        while (p && p->pNext) {
            lst.push_back(p->pNext);
            p = p->pNext;
        }
        if (lst.size() == 0) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listReverse' invalid list size 0";
            deleteList(pls, "listReverse 4");
            return pRes;
        }
        p = nullptr;
        if (lst[lst.size() - 1]->t != ISAtom::TokType::NIL) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listReverse' NIL not found internal error";
            deleteList(pls, "listReverse 3");
            return pRes;
        }
        for (int i = lst.size() - 2; i >= 0; i--) {
            if (i == lst.size() - 2) {
                pls->pChild = lst[i];
                p = pls->pChild;
            } else {
                p->pNext = lst[i];
                p = p->pNext;
            }
            if (i == 0) {
                p->pNext = lst[lst.size() - 1];
            }
        }
        deleteList(pRes, "listAppend 3");
        return pls;
    }

    ISAtom *evalParse(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'parse' requires one operand that is parsed as expression: got " + std::to_string(getListLen(pisa));
            return pRes;
        }
        ISAtom *pP = chainEval(pisa, local_symbols, true);
        if (pP->t != ISAtom::TokType::STRING) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'parse' requires a string to be parsed as parameter";
            deleteList(pP, "evalParse 3");
            return pRes;
        }
        string cmd = pP->vals;
        deleteList(pP, "EvalParse 0");
        cmd += " ";

        if (cmd != "") {
            int lvl = 0;
            ISAtom *pisa_p = parse(cmd, nullptr, lvl);
            ISAtom *pisa_res = chainEval(pisa_p, local_symbols, true);
            deleteList(pisa_p, "evalParse 1");
            deleteList(pRes, "evalParse 2");
            return pisa_res;
        } else {
            pRes->t = ISAtom::TokType::NIL;
            return pRes;
        }
    }

    ISAtom *evalLoad(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();

        ISAtom *pls = chainEval(pisa, local_symbols, true);

        if (getListLen(pls) != 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'load' requires one string operand, a filename, got: " + std::to_string(getListLen(pisa));
            deleteList(pls, "load 1");
            return pRes;
        }
        ISAtom *pP = (ISAtom *)pls;
        if (pP->t != ISAtom::TokType::STRING) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'load' requires a string operand, a filename";
            deleteList(pls, "load 2");
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
            fclose(fp);
        }
        if (cmd != "") {
            int lvl = 0;
            ISAtom *pisa_p = parse(cmd, nullptr, lvl);
            ISAtom *pisa_res = chainEval(pisa_p, local_symbols, false);
            deleteList(pisa_p, "evalLoad 4");
            deleteList(pRes, "evalLoad 5");
            deleteList(pls, "evalLoad 5.1");
            return pisa_res;
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Could not read file: " + pisa->vals;
            deleteList(pls, "evalLoad 5.2");
            return pRes;
        }
    }

    bool is_inbuilt(string funcName) {
        if (inbuilts.find(funcName) == inbuilts.end()) return false;
        return true;
    }

    ISAtom *eval_symbol(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *p, *pn, *pRet;
        if (is_defined_symbol(pisa->vals, local_symbols)) {
            if (is_defined_local_symbol(pisa->vals, local_symbols))
                p = get_local_symbol(pisa->vals, local_symbols);
            else if (symbols.find(pisa->vals) != symbols.end()) {
                p = copyList(symbols[pisa->vals]);
            } else {
                pRet = gca();
                pRet->t = ISAtom::TokType::ERROR;
                pRet->vals = "Invalid state when resolving symbol: " + pisa->vals;
                return pRet;
            }
            while (p->t == ISAtom::TokType::SYMBOL) {
                if (is_defined_symbol(p->vals, local_symbols)) {
                    if (is_defined_local_symbol(p->vals, local_symbols)) {
                        pn = get_local_symbol(pisa->vals, local_symbols);
                    } else {
                        pn = copyList(symbols[p->vals]);
                    }
                    deleteList(p, "eval_symbol 2");
                    p = pn;
                } else {
                    break;
                }
            }
            return p;  // copyList(p);
        } else {
            pRet = gca();
            pRet->t = ISAtom::TokType::ERROR;
            pRet->vals = "Undefined symbol: " + pisa->vals;
            return pRet;
        }
    }

    ISAtom *lambda_eval(const ISAtom *input_data, vector<map<string, ISAtom *>> &local_symbols, const ISAtom *pvars, const ISAtom *pfunc, int skipper = 0) {
        ISAtom *p, *pn;
        ISAtom *pRes = gca();
        local_symbols.push_back({});
        // pDef = funcs[pisa->vals];
        ISAtom *pNa = pvars->pChild;  // pDef->pChild;
        vector<string> localNames;
        int n = 0;
        bool err = false;
        while (pNa && !err) {
            if (pNa->t == ISAtom::TokType::NIL) break;
            if (pNa->t == ISAtom::TokType::SYMBOL) {
                n = n + 1;
                if (n > skipper) {
                    localNames.push_back(pNa->vals);
                    // cout << "localVar " << n - skipper << ": " << pNa->vals << endl;
                }
                pNa = pNa->pNext;
            } else {
                err = true;
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'lambda' function requires symbols as operands, type " + tokTypeNames[pNa->t] + " is invalid, symbol required.";
                pop_local_symbols(local_symbols);
                return pRes;
                break;
            }
        }
        if (getListLen(input_data) - 1 != n - 1) {
            err = true;
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Lambda requires " + std::to_string(n - 1 + skipper) + " arguments, " + std::to_string(getListLen(input_data) - 1) + " given";
            pop_local_symbols(local_symbols);
            return pRes;
        }

        ISAtom *pCurVar;
        const ISAtom *pInp = input_data;
        for (auto var_name : localNames) {
            ISAtom *pT = eval(pInp, local_symbols);
            // cout << "lambda var " << var_name << " = ";
            // print(pT, local_symbols, ISAtom::DecorType::NONE, false);
            // cout << endl;
            set_local_symbol(var_name, pT, local_symbols);
            pInp = pInp->pNext;
        }
        p = eval(pfunc, local_symbols);
        pop_local_symbols(local_symbols);
        deleteList(pRes, "lambda 1");
        return p;
    }

    ISAtom *eval_func(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (is_defined_func(pisa->vals)) {
            ISAtom *pDef = funcs[pisa->vals];
            ISAtom *pvars = gca(pDef);
            if (pDef->pChild) pvars->pChild = copyList(pDef->pChild);
            ISAtom *pfunc = pDef->pNext;
            ISAtom *p = lambda_eval(pisa->pNext, local_symbols, pvars, pfunc, 1);
            deleteList(pRes, "ev_func 1");
            deleteList(pvars, "ev_func 2");
            return p;
        }
        /*

                    ISAtom *p, *pn, *pDef;
                    local_symbols.push_back({});
                    pDef = funcs[pisa->vals];
                    ISAtom *pNa = pDef->pChild;
                    vector<string> localNames;
                    int n = 0;
                    bool err = false;
                    string func_name;
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
                        ISAtom *pT = eval(pInp, local_symbols);
                        set_local_symbol(var_name, pT, local_symbols);
                    }
                    p = eval(pDef->pNext, local_symbols);
                    pop_local_symbols(local_symbols);
                    deleteList(pRes, "eval_func");
                    return p;
    }
                    */
        else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Undefined function >" + pisa->vals + "< Internal error.";
            return pRes;
        }
    }

    ISAtom *
    eval(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols, bool func_only = false) {
        ISAtom *pN, *pRet;  //, *pReti;

        ISAtom *p = (ISAtom *)pisa;
        pN = p->pNext;

        switch (p->t) {
        case ISAtom::TokType::QUOTE:
            pRet = copyList(pN);
            return pRet;
            break;
        case ISAtom::TokType::BRANCH:
            pRet = eval(pisa->pChild, local_symbols, true);
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
            return pRet;
            break;
        case ISAtom::TokType::ERROR:
            pRet = copyList(p);
            return pRet;
            break;
        default:
            if (func_only) {
                pRet = gca();
                pRet->t = ISAtom::TokType::ERROR;
                pRet->vals = "Undefined expression: " + pisa->str();
                return pRet;
            } else {
                pRet = gca(p);
                if (p->pChild) pRet->pChild = copyList(p->pChild);
                return pRet;
            }
            break;
        }
    }

    ISAtom *chainEval(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols, bool bChainResult) {
        size_t start_index = gc_size();

        ISAtom *p = (ISAtom *)pisa, *pi, *pn;
        ISAtom *pCE = nullptr, *pCEi, *pCE_c = nullptr;
        bool is_quote = false;

        vector<ISAtom *> pAllocs;
        while (p) {
            if (p->t == ISAtom::TokType::NIL) {
                break;
            }
            pn = p->pNext;
            p->pNext = nullptr;

            pi = gca(p);
            if (p->pChild) pi->pChild = copyList(p->pChild);
            pAllocs.push_back(pi);

            switch (pi->t) {
            case ISAtom::TokType::QUOTE:
                if (is_quote) {
                    pCEi = copyList(pi);
                    pAllocs.push_back(pCEi);
                    is_quote = false;
                } else {
                    pCEi = nullptr;  // copyList(p->pNext);  // nullptr;
                    is_quote = true;
                }
                break;
            case ISAtom::TokType::SYMBOL:
                if (is_quote) {
                    pCEi = copyList(pi);
                    is_quote = false;
                } else {
                    pCEi = eval_symbol(pi, local_symbols);
                }
                pAllocs.push_back(pCEi);
                break;
            case ISAtom::TokType::BRANCH:
                if (is_quote) {
                    pCEi = copyList(pi);
                    is_quote = false;
                } else {
                    pCEi = eval(pi->pChild, local_symbols, true);
                }
                pAllocs.push_back(pCEi);
                break;
            default:
                is_quote = false;
                pCEi = gca(pi);
                if (p->pChild) pCEi->pChild = copyList(p->pChild);
                pAllocs.push_back(pCEi);
                break;
            }
            p->pNext = pn;
            p = pn;
            if (pCEi) {
                if (pCEi->t == ISAtom::TokType::ERROR) {
                    if (pCE) deleteList(pCE, "Error on EvalChain");
                    pCE = copyList(pCEi);
                    break;
                }

                if (!bChainResult) {
                    if (pCE) deleteList(pCE, "chainEval not chain1");
                    pCE = copyList(pCEi);
                } else {
                    if (!pCE) {
                        pCE_c = copyList(pCEi);
                        pCE = pCE_c;
                    } else {
                        pCE_c->pNext = copyList(pCEi);
                        pCE_c = pCE_c->pNext;
                    }
                }
            }
        }
        int n = 0;
        for (auto p : pAllocs) {
            deleteList(p, "chainEval 4-" + std::to_string(n));
            ++n;
        }
        if (!pCE) pCE = gca();
        // cout << "LOCAL[" << start_index << "] [" << gc_size() << "]" << endl;
        return pCE;
    }
};

}  // namespace insch
