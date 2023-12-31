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
                   LIST,
                   INVALID };
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
        case ISAtom::TokType::LIST:
            out = "(";
            break;
        case ISAtom::TokType::INT:
            switch (decor) {
            case ASCII:
                out = "(i)";
                break;
            case UNICODE:
                out = "ⓘ ";
                break;
            case NONE:
                out = "";
                break;
            }
            out += std::to_string(val);
            break;
        case ISAtom::TokType::FLOAT:
            switch (decor) {
            case ASCII:
                out = "(f)";
                break;
            case UNICODE:
                out = "ⓕ ";
                break;
            case NONE:
                out = "";
                break;
            }
            out += std::to_string(valf);
            break;
        case ISAtom::TokType::STRING:
            switch (decor) {
            case ASCII:
                out = "(s)\"" + vals + "\"";
                break;
            case UNICODE:
                out = "ⓢ \"" + vals + "\"";
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
                    out = "ⓑ ḟ";
                else
                    out = "ⓑ ṫ";
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
                out = "⒮ " + vals;
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
    vector<string> tokTypeNames = {"Nil", "Error", "Int", "Float", "String", "Boolean", "Symbol", "Quote", "List", "Invalid: internal error"};
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
        inbuilts["indentedstringify"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalIndentedStringify(pisa, local_symbols); };
        inbuilts["stringify"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalStringify(pisa, local_symbols); };
        inbuilts["listfunc"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalListfunc(pisa, local_symbols); };
        inbuilts["load"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalLoad(pisa, local_symbols); };
        inbuilts["parse"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalParse(pisa, local_symbols); };
        inbuilts["quote"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalQuote(pisa, local_symbols); };
        inbuilts["list"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listList(pisa, local_symbols); };
        inbuilts["cons"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listCons(pisa, local_symbols); };
        inbuilts["car"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listCar(pisa, local_symbols); };
        inbuilts["cdr"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listCdr(pisa, local_symbols); };
        inbuilts["length"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listLength(pisa, local_symbols); };
        inbuilts["append"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listAppend(pisa, local_symbols); };
        inbuilts["reverse"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listReverse(pisa, local_symbols); };
        inbuilts["index"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listIndex(pisa, local_symbols); };
        inbuilts["range"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listRange(pisa, local_symbols); };
        inbuilts["sublist"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return listSublist(pisa, local_symbols); };
        inbuilts["splitstring"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return stringSplitstring(pisa, local_symbols); };
        inbuilts["substring"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return stringSubstring(pisa, local_symbols); };
        inbuilts["lowercase"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return stringLowercase(pisa, local_symbols); };
        inbuilts["uppercase"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return stringUppercase(pisa, local_symbols); };
        inbuilts["replacestring"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return stringReplace(pisa, local_symbols); };
        inbuilts["find"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalFind(pisa, local_symbols); };

        inbuilts["eval"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalEval(pisa, local_symbols); };
        inbuilts["type"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalType(pisa, local_symbols); };
        inbuilts["convtype"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalConvtype(pisa, local_symbols); };

        inbuilts["every"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalEvery(pisa, local_symbols); };
        inbuilts["map"] = [&](ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) -> ISAtom * { return evalMap(pisa, local_symbols); };
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

    int getRawListLen(const ISAtom *pisa) {  // XXX NIL is counted!
        int len = 1;
        if (!pisa) return 0;
        const ISAtom *p = pisa;
        while (p->pNext) {
            if (p->t != ISAtom::TokType::QUOTE) ++len;
            p = p->pNext;
        }
        return len;
    }

    int getListLen(const ISAtom *pisa) {
        int len = 0;
        const ISAtom *p = pisa;
        while (p) {
            if (p->t != ISAtom::TokType::QUOTE && p->t != ISAtom::TokType::NIL) ++len;
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
            c = input.c_str()[0];
            if (c != input.substr(0, 1)[0]) {
                cout << "ERROR: Input parser Unicode collapse at >" << input.substr(0, 1) << "<"
                     << " vs. >" << c << "<"
                     << endl;
            }
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
                    pCurNode->t = ISAtom::TokType::LIST;
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
            deleteList(pStart, "Parser Error");
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
        if (!pisa) {
            cout << "NULLPTR!";
            return;
        }
        string out = pisa->str(decor);
        ISAtom *pN = pisa->pNext;
        if (decor) {
            if (is_inbuilt(pisa->vals) || is_defined_func(pisa->vals)) out = "⒡ " + out;
            if (is_defined_symbol(pisa->vals, local_symbols)) out = "⒮ " + out;
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

    string _indent(int tab_size, int level) {
        string s = "";
        if (tab_size > 0 /*&& ((level + 1) % 2) == 0*/) {
            s += "\n";
            for (int i = 0; i < level; i++) {
                for (int j = 0; j < tab_size; j++)
                    s += " ";
            }
        }
        return s;
    }

    string stringify(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols, ISAtom::DecorType decor, bool bAutoSeparators, int tab_size = 0, int level = 0) {
        string out = pisa->str(decor);
        ISAtom *pN = pisa->pNext;
        if (decor) {
            if (is_inbuilt(pisa->vals) || is_defined_func(pisa->vals))
                out = "ⓕ " + out;
            else if (is_defined_symbol(pisa->vals, local_symbols))
                out = "ⓢ " + out;
        }
        if (pisa->pChild != nullptr) {
            // out += _indent(tab_size, level);
            out += stringify(pisa->pChild, local_symbols, decor, bAutoSeparators, tab_size, level + 1);
            if (out.length() > 0 && out[out.length() - 1] == ' ') {
                out[out.length() - 1] = ')';
            } else {
                out += ")";
            }
            out += _indent(tab_size, level);
        }
        if (pN != nullptr) {
            if (bAutoSeparators && pN->t != ISAtom::TokType::QUOTE) {
                if (pisa->t != ISAtom::TokType::QUOTE) out += " ";
            }
            out += stringify(pN, local_symbols, decor, bAutoSeparators, tab_size, level);
        }
        return out;
    }

    ISAtom *cmp_2ops(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols, string m_op) {
        // cout << m_op << endl;
        // ISAtom *pisa = copyList(pisa_o);
        const ISAtom *p = pisa, *pn = nullptr;
        ISAtom *pRes = gca();
        vector<const ISAtom *> pAllocs;

        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Two operands required for <" + m_op + "> operation";
            return pRes;
        }
        ISAtom *pev = chainEval(pisa, local_symbols, true);
        pAllocs.push_back(pev);
        if (getListLen(pev) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Two operands required for <" + m_op + "> operation";
            deleteList(pev, "cmp_2ops parcheck");
            return pRes;
        }

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
        ISAtom *pn = nullptr;
        // bool fl = false;
        ISAtom::TokType dt = ISAtom::TokType::NIL;
        bool first = true;
        ISAtom *pRes = gca();
        vector<ISAtom *> pAllocs;

        if (getListLen(pisa) < 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Not enough operands for <" + m_op + "> operation";

            // cout << "Not-enough operands: Op: " << m_op << " length: " << getRawListLen(pisa) << " : ";
            // print(pisa, local_symbols, ISAtom::DecorType::UNICODE, true);
            // cout << endl;

            return pRes;
        }

        ISAtom *pev = chainEval(pisa, local_symbols, true);
        pAllocs.push_back(pev);
        ISAtom *p = (ISAtom *)pev;

        // cout << "Op: " << m_op << ": ";
        // print(pev, local_symbols, ISAtom::DecorType::UNICODE, true);
        // cout << endl;

        while (p != nullptr) {
            // p = chainEval(p, local_symbols, true);

            // print(p, local_symbols, ISAtom::DecorType::UNICODE, true);
            // cout << endl;

            // pAllocs.push_back(p);
            //  p->pNext = pn;
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
                pRes->vals = "Op: " + m_op + ", unhandled tokType: " + tokTypeNames[p->t] + " -> " + p->str();
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
        if (getListLen(pisa) != 2) {
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
            if (getListLen(pisa) != 2) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Symbol-'define' requires exactly 2 operands: name and value";
                return pRes;
            }
            if (pV->t == ISAtom::TokType::QUOTE) {
                ISAtom *pT = gca(pV->pNext, false);
                if (pV->pNext && pV->pNext->pChild) pT->pChild = copyList(pV->pNext->pChild, false);
                symbols[pN->vals] = pT;
            } else {
                if (symbols.find(pN->vals) != symbols.end()) deleteList(symbols[pN->vals], "DelSymOnUpdate", true);
                ISAtom *pT = chainEval(pV, local_symbols, true);
                symbols[pN->vals] = copyList(pT, false);
                deleteList(pT, "makeDefine 1");  // XXX only 2nd argument? See QUOTE case
            }
            deleteList(pRes, "makeDefine 2");
            return copyList(symbols[pN->vals]);
            break;
        case ISAtom::TokType::LIST:
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
                    if (funcs.find(pNa->vals) != funcs.end()) deleteList(funcs[pNa->vals], "DelFuncOnUpdate", true);
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
        if (getListLen(pisa) < 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'let' requires at least 1 operand";
            return pRes;
        }

        if (pisa->t != ISAtom::TokType::LIST) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'let' no primary list: required are a list of key values pairs: ((k v ), ..) [ ()]";
            return pRes;
        }
        local_symbols.push_back({});
        ISAtom *pDef = pisa->pChild;
        while (pDef && pDef->t != ISAtom::TokType::NIL) {
            if (pDef->t != ISAtom::TokType::LIST) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'let' list entries must be list: required are a list of key values pairs: ((k v ), ..) [ ()]";
                for (ISAtom *pA : pAllocs) {
                    deleteList(pA, "makeLocalDefine 1");
                }
                pop_local_symbols(local_symbols);
                return pRes;
            }
            if (getListLen(pDef->pChild) != 2) {
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
        if (getListLen(pisa) != 2) {
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
        if (getListLen(pisa) != 2 && getListLen(pisa) != 3) {
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
        if (getListLen(pisa) < 2) {
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
        if (pLast == nullptr) {
            pLast = gca();
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

    ISAtom *evalIndentedStringify(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pls) < 2 || pls->t != ISAtom::TokType::INT) {
            ISAtom *pRes = gca();
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'stringify' requires at least 2 operands, an INT tab-size for indentation and expression(s): <expr> [expr...]";
            return pRes;
        }
        int tab_size = pls->val;
        string st = stringify(pls->pNext, local_symbols, ISAtom::DecorType::NONE, true, tab_size);
        deleteList(pls, "evalStringify 1");
        ISAtom *pRes = gca();
        pRes->t = ISAtom::TokType::STRING;
        pRes->vals = st;
        return pRes;
    }

    ISAtom *evalStringify(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pls) < 1) {
            ISAtom *pRes = gca();
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'stringify' requires at least 1 operand: <expr> [expr...]";
            return pRes;
        }
        string st = stringify(pls, local_symbols, ISAtom::DecorType::NONE, true);
        deleteList(pls, "evalStringify 1");
        ISAtom *pRes = gca();
        pRes->t = ISAtom::TokType::STRING;
        pRes->vals = st;
        return pRes;
    }

    ISAtom *evalListfunc(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        ISAtom *pRes = gca();
        if (getListLen(pls) < 1 || (pls->t != ISAtom::TokType::STRING && pls->t != ISAtom::TokType::SYMBOL) || (getListLen(pls) == 2 && pls->pNext->t != ISAtom::TokType::INT) ||
            (getListLen(pls) > 2)) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listfunc' requires at STRING or (quoted) SYMBOL function name and an optional INT tab_size for indentation";
            deleteList(pls, "listfunc 1");
            return pRes;
        }
        string funcname = pls->vals;
        int tab_size = 0;
        if (getListLen(pls) == 2) tab_size = pls->pNext->val;
        deleteList(pls, "Listfunc 2");
        if (!is_defined_func(funcname)) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listfunc' function >" + funcname + "< is not defined";
            return pRes;
        }
        string listfunc = stringify(funcs[funcname], local_symbols, ISAtom::DecorType::NONE, true, tab_size);
        while (listfunc.length() > 0 and listfunc[listfunc.length() - 1] == ' ')
            listfunc = listfunc.substr(0, listfunc.length() - 1);  // remove trailing spaces
        pRes->t = ISAtom::TokType::STRING;
        pRes->vals = listfunc;
        return pRes;
    }

    ISAtom *evalQuote(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = copyList(pisa);
        return pRes;
    }

    ISAtom *evalEvery(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) < 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'evalEvery' requires at least 2 operands";
            return pRes;
        }

        ISAtom *pL = eval(pisa->pNext, local_symbols);
        if (pL->t != ISAtom::TokType::LIST) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'evalEvery' requires a list as 2nd operand, got: " + tokTypeNames[pL->t] + " " + pL->vals;
            deleteList(pL, "evalEvery 1");
            return pRes;
        }
        ISAtom *p, *pFi, *pC, *pCn;
        pC = gca();
        pC->t = ISAtom::TokType::LIST;
        pCn = pC;
        bool first = true;

        p = pL->pChild;
        while (p && p->t != ISAtom::TokType::NIL) {
            pFi = gca();
            pFi->t = ISAtom::TokType::LIST;
            pFi->pChild = gca(pisa);
            if (pisa->pChild) pFi->pChild->pChild = copyList(pisa->pChild);
            pFi->pChild->pNext = gca(p);
            if (p->pChild) pFi->pChild->pNext->pChild = copyList(p->pChild);
            pFi->pChild->pNext->pNext = gca();
            pFi->pChild->pNext->pNext->t = ISAtom::TokType::NIL;
            ISAtom *pR = eval(pFi, local_symbols);
            if (first) {
                pCn->pChild = pR;
                pCn = pCn->pChild;
                first = false;
            } else {
                pCn->pNext = pR;
                pCn = pCn->pNext;
            }
            deleteList(pFi, "eval every 2");
            p = p->pNext;
        }
        if (first) {
            pC->pChild = gca();
        }
        deleteList(pL, "eval every 3");
        deleteList(pRes, "eval every 4");
        return pC;
    }

    ISAtom *evalMap(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        int rawNumArgs = getListLen(pisa);
        if (rawNumArgs < 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'evalMap' requires at least 2 operands";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa->pNext, local_symbols, true);
        ISAtom *p = pls;
        int arg_len = -1;
        vector<ISAtom *> pParams;
        for (int i = 0; i < rawNumArgs - 1; i++) {
            if (p->t != ISAtom::TokType::LIST) {
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "'evalMap' requires a list as 2nd and following operand, got: " + tokTypeNames[p->t] + " " + p->vals;
                deleteList(pls, "evalMap 1");
                return pRes;
            }
            if (i == 0)
                arg_len = getListLen(p->pChild);
            else {
                if (arg_len != getListLen(p->pChild)) {
                    pRes->t = ISAtom::TokType::ERROR;
                    pRes->vals = "'evalMap' requires a lists as 2nd and following operand of equal size, got sizes: " + std::to_string(arg_len) + ", " + std::to_string(getListLen(p));
                    deleteList(pls, "evalMap 1");
                    return pRes;
                }
            }
            pParams.push_back(p->pChild);
            p = p->pNext;
        }

        int parmCnt = pParams.size();
        ISAtom *pFi, *pC, *pCn, *pParamI;
        pC = gca();
        pC->t = ISAtom::TokType::LIST;
        pCn = pC;
        bool first = true;

        for (int i = 0; i < arg_len; i++) {
            pFi = gca();
            pFi->t = ISAtom::TokType::LIST;
            pFi->pChild = gca(pisa);
            if (pisa->pChild) pFi->pChild->pChild = copyList(pisa->pChild);
            pParamI = pFi->pChild;
            for (int j = 0; j < parmCnt; j++) {
                pParamI->pNext = gca(pParams[j]);
                if (pParams[j]->pChild) pParamI->pNext->pChild = copyList(pParams[j]->pChild);
                pParams[j] = pParams[j]->pNext;
                pParamI = pParamI->pNext;
            }
            bool bShowMap = false;
            if (bShowMap) {
                cout << "EI" << i << " ";
                print(pFi, local_symbols, ISAtom::DecorType::UNICODE, true);
                cout << endl;
            }
            ISAtom *pR = eval(pFi, local_symbols);
            if (first) {
                pCn->pChild = pR;
                pCn = pCn->pChild;
                first = false;
            } else {
                pCn->pNext = pR;
                pCn = pCn->pNext;
            }
            deleteList(pFi, "eval map 2");
        }
        if (first) {
            pC->pChild = gca();
        }
        deleteList(pls, "eval map 3");
        deleteList(pRes, "eval map 4");
        return pC;
    }

    ISAtom::TokType String2Type(string typestring) {
        for (int i = 0; i < tokTypeNames.size() - 1; i++) {
            if (tokTypeNames[i] == typestring) return (ISAtom::TokType)i;
        }
        return ISAtom::TokType::INVALID;
    }

    ISAtom *evalType(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);

        if (getListLen(pls) != 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'type' requires one operand";
            deleteList(pls, "evalType 1");
            return pRes;
        }
        pRes->t = ISAtom::TokType::SYMBOL;
        pRes->vals = tokTypeNames[pls->t];
        deleteList(pls, "evalType 2");
        return pRes;
    }

    ISAtom *evalConvtype(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);

        if (getListLen(pls) != 2 || (pls->pNext->t != ISAtom::TokType::STRING && pls->pNext->t != ISAtom::TokType::SYMBOL)) {
            pRes->t = ISAtom::TokType::ERROR;
            if (getListLen(pls) == 2) {
                pRes->vals = "'convtype' requires two operands (got: " + std::to_string(getListLen(pls)) + "), second needs to be a type, (got: " + tokTypeNames[pls->pNext->t] + "), valid types are: ";
                for (int i = 0; i < tokTypeNames.size() - 1; i++) {
                    pRes->vals += "'" + tokTypeNames[i] + " ";
                }
            } else {
                pRes->vals = "'convtype' requires two operands";

                cout << "Convtype input: ";
                print(pls, local_symbols, ISAtom::DecorType::UNICODE, true);
                cout << endl;
            }
            deleteList(pls, "evalConvtype 1");
            return pRes;
        }
        ISAtom::TokType dest_type = String2Type(pls->pNext->vals);
        if (dest_type == ISAtom::TokType::INVALID) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'convtype' destination type: " + pls->pNext->vals + " is invalid. Valid types are: ";
            for (int i = 0; i < tokTypeNames.size() - 1; i++) {
                pRes->vals += "'" + tokTypeNames[i] + " ";
            }
            deleteList(pls, "evalConvtype 2");
            return pRes;
        }
        ISAtom *source = gca(pls);
        if (pls->pChild) source->pChild = copyList(pls->pChild);
        deleteList(pls, "convType 3");
        //     vector<string>  = {"Nil", "Error", "Int", "Float", "String", "Boolean", "Symbol", "Quote", "List", "Invalid: internal error"};
        pRes->t = ISAtom::TokType::ERROR;
        pRes->vals = "Not implemented: conversion " + tokTypeNames[source->t] + " -> " + tokTypeNames[dest_type];

        switch (source->t) {
        case ISAtom::TokType::INT:
            switch (dest_type) {
            case ISAtom::TokType::INT:
                pRes->t = ISAtom::TokType::INT;
                pRes->val = source->val;
                break;
            case ISAtom::TokType::FLOAT:
                pRes->t = ISAtom::TokType::FLOAT;
                pRes->valf = source->val;
                break;
            case ISAtom::TokType::STRING:
                pRes->t = ISAtom::TokType::STRING;
                pRes->vals = std::to_string(source->val);
                break;
            case ISAtom::TokType::BOOLEAN:
                pRes->t = ISAtom::TokType::BOOLEAN;
                pRes->val = source->val;
                break;
            // case ISAtom::TokType::SYMBOL:
            //     break;
            // case ISAtom::TokType::ERROR:
            //     break;
            case ISAtom::TokType::LIST:
                pRes->t = ISAtom::TokType::LIST;
                pRes->pChild = gca();
                pRes->pChild->t = ISAtom::TokType::INT;
                pRes->pChild->val = source->val;
                pRes->pChild->pNext = gca();
                break;
            default:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Invalid conversion " + tokTypeNames[source->t] + " -> " + tokTypeNames[dest_type];
                break;
            }
            break;
        case ISAtom::TokType::FLOAT:
            switch (dest_type) {
            case ISAtom::TokType::INT:
                pRes->t = ISAtom::TokType::INT;
                pRes->val = source->valf;
                break;
            case ISAtom::TokType::FLOAT:
                pRes->t = ISAtom::TokType::FLOAT;
                pRes->valf = source->valf;
                break;
            case ISAtom::TokType::STRING:
                pRes->t = ISAtom::TokType::STRING;
                pRes->vals = std::to_string(source->valf);
                break;
            case ISAtom::TokType::BOOLEAN:
                pRes->t = ISAtom::TokType::BOOLEAN;
                pRes->val = source->valf;
                break;
            // case ISAtom::TokType::SYMBOL:
            //     break;
            // case ISAtom::TokType::ERROR:
            //     break;
            case ISAtom::TokType::LIST:
                pRes->t = ISAtom::TokType::LIST;
                pRes->pChild = gca();
                pRes->pChild->t = ISAtom::TokType::FLOAT;
                pRes->pChild->valf = source->valf;
                pRes->pChild->pNext = gca();
                break;
            default:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Invalid conversion " + tokTypeNames[source->t] + " -> " + tokTypeNames[dest_type];
                break;
            }
            break;
        case ISAtom::TokType::STRING:
            switch (dest_type) {
            case ISAtom::TokType::INT:
                if (is_int(source->vals)) {
                    pRes->t = ISAtom::TokType::INT;
                    pRes->val = atoi(source->vals.c_str());
                } else {
                    pRes->t = ISAtom::TokType::ERROR;
                    pRes->vals = "Invalid conversion " + source->vals + " is not INT convertible";
                }
                break;
            case ISAtom::TokType::FLOAT:
                if (is_int(source->vals) || is_float(source->vals)) {
                    pRes->t = ISAtom::TokType::FLOAT;
                    pRes->valf = atof(source->vals.c_str());
                } else {
                    pRes->t = ISAtom::TokType::ERROR;
                    pRes->vals = "Invalid conversion " + source->vals + " is not FLOAT convertible";
                }
                break;
            case ISAtom::TokType::STRING:
                pRes->t = ISAtom::TokType::STRING;
                pRes->vals = source->vals;
                break;
            case ISAtom::TokType::BOOLEAN:
                pRes->t = ISAtom::TokType::BOOLEAN;
                if (source->vals == "#t")
                    pRes->val = 1;
                else if (source->vals == "#f")
                    pRes->val = 0;
                else {
                    pRes->t = ISAtom::TokType::ERROR;
                    pRes->vals = "Invalid conversion " + source->vals + " is not BOOLEAN convertible";
                }
                break;
            case ISAtom::TokType::SYMBOL:
                pRes->t = ISAtom::TokType::SYMBOL;
                pRes->vals = source->vals;
                break;
            case ISAtom::TokType::ERROR:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = source->vals;
                break;
            case ISAtom::TokType::LIST:
                pRes->t = ISAtom::TokType::LIST;
                pRes->pChild = gca();
                pRes->pChild->t = ISAtom::TokType::STRING;
                pRes->pChild->vals = source->vals;
                pRes->pChild->pNext = gca();
                break;
            default:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Invalid conversion " + tokTypeNames[source->t] + " -> " + tokTypeNames[dest_type];
                break;
            }
            break;
        case ISAtom::TokType::BOOLEAN:
            switch (dest_type) {
            case ISAtom::TokType::INT:
                pRes->t = ISAtom::TokType::INT;
                pRes->val = source->val;
                break;
            case ISAtom::TokType::FLOAT:
                pRes->t = ISAtom::TokType::FLOAT;
                pRes->valf = source->val;
                break;
            case ISAtom::TokType::STRING:
                pRes->t = ISAtom::TokType::STRING;
                if (source->val)
                    pRes->vals = "#t";
                else
                    pRes->vals = "#f";
                break;
            case ISAtom::TokType::BOOLEAN:
                pRes->t = ISAtom::TokType::BOOLEAN;
                pRes->val = source->val;
                break;
            // case ISAtom::TokType::SYMBOL:
            //     break;
            // case ISAtom::TokType::ERROR:
            //     break;
            case ISAtom::TokType::LIST:
                pRes->t = ISAtom::TokType::LIST;
                pRes->pChild = gca();
                pRes->pChild->t = ISAtom::TokType::BOOLEAN;
                pRes->pChild->val = source->val;
                pRes->pChild->pNext = gca();
                break;
            default:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Invalid conversion " + tokTypeNames[source->t] + " -> " + tokTypeNames[dest_type];
                break;
            }
            break;
        case ISAtom::TokType::SYMBOL:
            switch (dest_type) {
            case ISAtom::TokType::STRING:
                pRes->t = ISAtom::TokType::STRING;
                pRes->vals = source->vals;
                break;
            case ISAtom::TokType::SYMBOL:
                pRes->t = ISAtom::TokType::SYMBOL;
                pRes->vals = source->vals;
                break;
            case ISAtom::TokType::LIST:
                pRes->t = ISAtom::TokType::LIST;
                pRes->pChild = gca();
                pRes->pChild->t = ISAtom::TokType::SYMBOL;
                pRes->pChild->vals = source->vals;
                pRes->pChild->pNext = gca();
                break;
            default:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Invalid conversion " + tokTypeNames[source->t] + " -> " + tokTypeNames[dest_type];
                break;
            }
            break;
        case ISAtom::TokType::ERROR:
            switch (dest_type) {
            case ISAtom::TokType::STRING:
                pRes->t = ISAtom::TokType::STRING;
                pRes->vals = source->vals;
                break;
            case ISAtom::TokType::ERROR:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = source->vals;
                break;
            case ISAtom::TokType::LIST:
                pRes->t = ISAtom::TokType::LIST;
                pRes->pChild = gca();
                pRes->pChild->t = ISAtom::TokType::ERROR;
                pRes->pChild->vals = source->vals;
                pRes->pChild->pNext = gca();
                break;
            default:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Invalid conversion " + tokTypeNames[source->t] + " -> " + tokTypeNames[dest_type];
                break;
            }
            break;
        case ISAtom::TokType::LIST:
            switch (dest_type) {
            case ISAtom::TokType::STRING:
                pRes->t = ISAtom::TokType::STRING;
                pRes->vals = stringify(source, local_symbols, ISAtom::DecorType::NONE, true);
                break;
            case ISAtom::TokType::LIST:
                deleteList(pRes, "convType 4");
                pRes = copyList(source);
                break;
            default:
                pRes->t = ISAtom::TokType::ERROR;
                pRes->vals = "Invalid conversion " + tokTypeNames[source->t] + " -> " + tokTypeNames[dest_type];
                break;
            }
            break;
        default:
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Invalid conversion " + tokTypeNames[source->t] + " -> " + tokTypeNames[dest_type];
            break;
        }
        deleteList(source, "convtype 5");
        return pRes;
    }

    ISAtom *
    evalEval(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
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
        return pResS;
    }

    ISAtom *evalFind(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pls) != 2 || ((pls->t != ISAtom::TokType::STRING || pls->pNext->t != ISAtom::TokType::STRING) && pls->t != ISAtom::TokType::LIST) || pls->pNext->t == ISAtom::TokType::LIST) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'find' two parameters: a STRING or LIST followed by an atom (not at list) which has to be a STRING, if first param is STRING";
            deleteList(pls, "evalFind 1");
            return pRes;
        }
        if (pls->t == ISAtom::TokType::STRING) {
            string source = pls->vals;
            string token = pls->pNext->vals;
            deleteList(pls, "evalFind 2");
            int index = source.find(token);
            if (index == source.npos) {  // NIL
                // nothing
            } else {
                pRes->t = ISAtom::TokType::INT;
                pRes->val = index;
            }
            return pRes;
        } else {
            ISAtom *source = pls->pChild;
            ISAtom *token = pls->pNext;
            int index = 0;
            bool found = false;
            while (source && !found) {
                if (source->t == token->t) {
                    switch (source->t) {
                    case ISAtom::TokType::BOOLEAN:
                    case ISAtom::TokType::INT:
                        if (source->val == token->val) found = true;
                        break;
                        if (source->val == token->val) found = true;
                        break;
                    case ISAtom::TokType::FLOAT:
                        if (source->valf == token->valf) found = true;
                        break;
                    case ISAtom::TokType::SYMBOL:
                    case ISAtom::TokType::STRING:
                        if (source->vals == token->vals) found = true;
                        break;
                    }
                }
                if (!found) index++;
                source = source->pNext;
            }
            deleteList(pls, "evalFind 2");
            if (!found) {  // NIL
                // nothing
            } else {
                pRes->t = ISAtom::TokType::INT;
                pRes->val = index;
            }
            return pRes;
        }
    }

    ISAtom *stringSubstring(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        int r1 = 0, r2 = 0;

        // print(pls, local_symbols, ISAtom::DecorType::UNICODE, true);
        // cout << " len=" << getListLen(pls) << endl;

        if (getListLen(pls) == 3 && pls->t == ISAtom::TokType::STRING && pls->pNext->t == ISAtom::TokType::INT && pls->pNext->pNext->t == ISAtom::TokType::INT) {
            r1 = pls->pNext->val;
            r2 = pls->pNext->pNext->val;
        } else if (getListLen(pls) == 2 && pls->t == ISAtom::TokType::STRING && pls->pNext->t == ISAtom::TokType::INT) {
            r1 = pls->pNext->val;
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'substring' requires a string and one or two INT operands, got " + std::to_string(getListLen(pls));
            cout << endl;
            print(pls, local_symbols, ISAtom::DecorType::UNICODE, true);
            cout << endl;
            deleteList(pls, "listSubstring 1");
            return pRes;
        }
        if (r1 < 0 || r2 < 0 || r1 >= pls->vals.length() || r1 + r2 > pls->vals.length()) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'substring' index out of range";
            deleteList(pls, "listSubstring 2");
            return pRes;
        }
        pRes->t = ISAtom::TokType::STRING;
        if (r2 != 0)
            pRes->vals = pls->vals.substr(r1, r2);
        else
            pRes->vals = pls->vals.substr(r1);
        deleteList(pls, "listSubstring 3");
        return pRes;
    }

    ISAtom *stringSplitstring(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);

        string splitter;
        if (getListLen(pls) == 2 && pls->t == ISAtom::TokType::STRING && pls->pNext->t == ISAtom::TokType::STRING) {
            splitter = pls->pNext->vals;
        } else if (getListLen(pls) == 1 && pls->t == ISAtom::TokType::STRING) {
            splitter = "";
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'splitstring' requires one or two string arguments";
            deleteList(pls, "splitstring 1");
            return pRes;
        }
        deleteList(pRes, "splitstring 2");
        string source = pls->vals;
        deleteList(pls, "splitstring 3");
        // cout << "Source: " << source << ", splitter: " << splitter << endl;
        pRes = gca();
        pRes->t = ISAtom::TokType::LIST;
        ISAtom *p = pRes;
        bool first = true;
        if (splitter == "") {
            for (int i = 0; i < source.length(); i++) {
                string split = source.substr(i, 1);
                if (first) {
                    first = false;
                    p->pChild = gca();
                    p = p->pChild;
                } else {
                    p->pNext = gca();
                    p = p->pNext;
                }
                p->t = ISAtom::TokType::STRING;
                p->vals = split;
            }
        } else {
            int index;
            while ((index = source.find(splitter)) != std::string::npos) {
                if (index == 0) {
                    source = source.substr(splitter.length());
                    continue;
                }
                if (first) {
                    first = false;
                    p->pChild = gca();
                    p = p->pChild;
                } else {
                    p->pNext = gca();
                    p = p->pNext;
                }
                p->t = ISAtom::TokType::STRING;
                p->vals = source.substr(0, index);
                source = source.substr(index + splitter.length());
            }
            if (source != "") {
                if (first) {
                    first = false;
                    p->pChild = gca();
                    p = p->pChild;
                } else {
                    p->pNext = gca();
                    p = p->pNext;
                }
                p->t = ISAtom::TokType::STRING;
                p->vals = source;
            }
        }

        if (first) {
            p->pChild = gca();
        } else {
            p->pNext = gca();
        }
        return pRes;
    }

    ISAtom *stringLowercase(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pls) != 1 || pls->t != ISAtom::TokType::STRING) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'lowercase' requires a string";
            deleteList(pls, "lowercase 1");
            return pRes;
        }
        pRes->t = ISAtom::TokType::STRING;
        pRes->vals = "";
        for (auto c : pls->vals) {
            pRes->vals += tolower(c);
        }
        deleteList(pls, "lowercase 2");
        return pRes;
    }

    ISAtom *stringUppercase(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pls) != 1 || pls->t != ISAtom::TokType::STRING) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'uppercase' requires a string";
            deleteList(pls, "uppercase 1");
            return pRes;
        }
        pRes->t = ISAtom::TokType::STRING;
        pRes->vals = "";
        for (auto c : pls->vals) {
            pRes->vals += toupper(c);
        }
        deleteList(pls, "uppercase 2");
        return pRes;
    }

    ISAtom *stringReplace(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);

        string source, from_tok, to_tok;
        if (getListLen(pls) == 3 && pls->t == ISAtom::TokType::STRING && pls->pNext->t == ISAtom::TokType::STRING && pls->pNext->pNext->t == ISAtom::TokType::STRING && pls->pNext->vals.length() > 0) {
            source = pls->vals;
            from_tok = pls->pNext->vals;
            to_tok = pls->pNext->pNext->vals;
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'replacestring' requires three string arguments, second (from_tok) must be length>0";
            deleteList(pls, "replacestring 1");
            return pRes;
        }
        deleteList(pls, "splitstring 2");

        pRes->t = ISAtom::TokType::STRING;
        pRes->vals = "";
        int index = source.find(from_tok);
        while (index != source.npos) {
            pRes->vals += source.substr(0, index) + to_tok;
            source = source.substr(index + from_tok.length());
            index = source.find(from_tok);
        }
        pRes->vals += source;
        return pRes;
    }

    ISAtom *listLength(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *p = (ISAtom *)pisa;

        ISAtom *pls = chainEval(pisa, local_symbols, true);

        if (getListLen(pls) != 1 || pls->t != ISAtom::TokType::LIST) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'len' requires list or quoted list operand, len=" + std::to_string(getListLen(pls)) + ", got type: " + tokTypeNames[pls->t];
            deleteList(pls, "listLen 1");
            return pRes;
        }
        pRes->t = ISAtom::TokType::INT;
        pRes->val = getListLen(pls->pChild);
        deleteList(pls, "listLen 2");
        return pRes;
    }

    ISAtom *listIndex(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);

        if (getListLen(pls) != 2 || pls->t != ISAtom::TokType::LIST || pls->pNext->t != ISAtom::TokType::INT) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'index' requires or quoted list and and integer operand";
            deleteList(pls, "listIndex 1");
            return pRes;
        }
        int index = pls->pNext->val;
        if (getListLen(pls->pChild) <= index || index < 0) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'index' out of range at index: " + std::to_string(index) + ", list is of size: " + std::to_string(getListLen(pls->pChild));
            deleteList(pls, "listIndex 2");
            return pRes;
        }
        ISAtom *p = pls->pChild;
        for (int i = 0; i < index; i++) {
            if (p->t == ISAtom::TokType::QUOTE) p = p->pNext;  // XXX another quote mess.
            p = p->pNext;
        }
        if (!p) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'index' walk caused internal error";
            deleteList(pls, "listIndex 3");
            return pRes;
        }
        deleteList(pRes, "listIndex 4");
        if (p->t == ISAtom::TokType::QUOTE) p = p->pNext;
        pRes = gca(p);
        if (p->pChild) pRes = copyList(p->pChild);
        deleteList(pls, "listIndex 5");
        return pRes;
    }

    ISAtom *listRange(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        int r1 = 0, r2;
        if (getListLen(pls) == 2 && pls->t == ISAtom::TokType::INT && pls->pNext->t == ISAtom::TokType::INT) {
            r1 = pls->val;
            r2 = pls->pNext->val;
        } else if (getListLen(pls) == 1 && pls->t == ISAtom::TokType::INT) {
            r2 = pls->val;
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'range' requires one or two INT operands, got " + std::to_string(getListLen(pls));
            deleteList(pls, "listRange 1");
            return pRes;
        }
        deleteList(pRes, "listRange 2");
        pRes = gca();
        pRes->t = ISAtom::TokType::LIST;
        ISAtom *p = pRes;
        bool first = true;
        for (int i = r1; i < r2; i++) {
            if (first) {
                p->pChild = gca();
                p = p->pChild;
                first = false;
            } else {
                p->pNext = gca();
                p = p->pNext;
            }
            p->val = i;
            p->t = ISAtom::TokType::INT;
        }
        if (first) {
            // p->pNext = gca();
            p->pChild = gca();
        } else {
            p->pNext = gca();
        }
        deleteList(pls, "listRange 3");
        return pRes;
    }

    ISAtom *listSublist(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        int r1 = 0, r2;

        // print(pls, local_symbols, ISAtom::DecorType::UNICODE, true);
        // cout << " len=" << getListLen(pls) << endl;

        if (getListLen(pls) == 3 && pls->t == ISAtom::TokType::LIST && pls->pNext->t == ISAtom::TokType::INT && pls->pNext->pNext->t == ISAtom::TokType::INT) {
            r1 = pls->pNext->val;
            r2 = pls->pNext->pNext->val;
        } else if (getListLen(pls) == 2 && pls->t == ISAtom::TokType::LIST && pls->pNext->t == ISAtom::TokType::INT) {
            r2 = pls->pNext->val;
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'sublist' requires a list and one or two INT operands, got " + std::to_string(getListLen(pls));
            deleteList(pls, "listSublist 1");
            return pRes;
        }
        deleteList(pRes, "listSublist 2");
        pRes = gca();
        pRes->t = ISAtom::TokType::LIST;
        ISAtom *p = pRes;
        bool first = true;
        ISAtom *pS = pls->pChild;
        for (int i = 0; i < getListLen(pls->pChild); i++) {
            if (i >= r1 && i < r2) {
                if (first) {
                    first = false;
                    p->pChild = gca(pS);
                    p = p->pChild;
                    if (pS->pChild) p->pChild = copyList(pS->pChild);
                } else {
                    p->pNext = gca(pS);
                    p = p->pNext;
                    if (pS->pChild) p->pChild = copyList(pS->pChild);
                }
                if (pS->t == ISAtom::TokType::QUOTE) {
                    pS = pS->pNext;
                    p->pNext = gca(pS);
                    p = p->pNext;
                    if (pS->pChild) p->pChild = copyList(pS->pChild);
                }
                pS = pS->pNext;
            }
        }
        if (first) {
            p->pChild = gca();
        } else {
            p->pNext = gca();
        }
        deleteList(pls, "listSublist 3");
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
        pRes->t = ISAtom::TokType::LIST;
        pRes->pChild = copyList(pls);
        deleteList(pls, "listList 1");
        return pStart;
    }

    ISAtom *listCons(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        ISAtom *pStart = pRes;
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pls) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cons' requires two args";
            return pRes;
        }
        ISAtom *c1 = gca(pls);
        if (pls->pChild) c1->pChild = copyList(pls->pChild);

        ISAtom *c2 = copyList(pls->pNext);
        deleteList(pls, "listCons 0");
        // if (c2->t == ISAtom::TokType::QUOTE) {
        //     ISAtom *c2_n = copyList(c2->pNext);
        //     deleteList(c2, "listCons 1");
        //     c2 = c2_n;
        // }
        if (c2->t != ISAtom::TokType::LIST) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cons' 2nd arg needs to eval to list (e.g. quoted list)";
            deleteList(c2, "listCons 2");
            deleteList(c1, "listCons 2.1");
            return pRes;
        }
        // pRes->t = ISAtom::TokType::QUOTE;
        // pRes->pNext = gca();
        // pRes = pRes->pNext;
        pRes->t = ISAtom::TokType::LIST;
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
        if (getListLen(pisa) != 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'car' requires one list arg";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (pls->t != ISAtom::TokType::LIST) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'car' requires one arg as list";
            deleteList(pls, "car 1");
            return pRes;
        }
        if (!pls->pChild->pNext) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'car' requires one arg as non-empty list";
            deleteList(pls, "car 2");
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
        if (getListLen(pisa) != 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'cdr' requires one list arg";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (pls->t != ISAtom::TokType::LIST) {
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
        pCdr->t = ISAtom::TokType::LIST;
        pCdr->pChild = copyList(pls->pChild->pNext);
        deleteList(pRes, "cdr 3");
        deleteList(pls, "cdr 4");
        return pCdr;
    }

    ISAtom *listAppend(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listAppend' requires two args";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pisa) != 2) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listAppend' (aft. eval) requires two args";
            deleteList(pls, "listAppend 1");
            return pRes;
        }
        if (pls->t != ISAtom::TokType::LIST) {
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
        if (getListLen(pisa) != 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listReverse' requires one args";
            return pRes;
        }
        ISAtom *pls = chainEval(pisa, local_symbols, true);
        if (getListLen(pisa) != 1) {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "'listReverse' (aft. eval) requires one args";
            return pRes;
        }
        if (pls->t != ISAtom::TokType::LIST) {
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
        if (getListLen(pisa) != 1) {
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

    ISAtom *load(string filename, vector<map<string, ISAtom *>> &local_symbols) {
        char buf[129];
        size_t nb;
        string cmd = "";
        ISAtom *pRes = gca();
        FILE *fp = fopen(filename.c_str(), "r");
        if (fp) {
            while (!feof(fp)) {
                nb = fread(buf, 1, 128, fp);
                buf[nb] = 0;
                cmd += buf;
            }
            fclose(fp);
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Could not read file: " + filename;
            return pRes;
        }
        if (cmd != "") {
            int lvl = 0;
            ISAtom *pisa_p = parse(cmd, nullptr, lvl);
            ISAtom *pisa_res = chainEval(pisa_p, local_symbols, false);
            deleteList(pisa_p, "evalLoad 4");
            deleteList(pRes, "evalLoad 5");
            return pisa_res;
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Empty file: " + filename;
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
        ISAtom *pR = load(pP->vals, local_symbols);
        deleteList(pls, "evalLoad 5.2");
        deleteList(pRes, "evalLoad 5.3");
        return pR;
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
        ISAtom *pNa = pvars->pChild;  // pDef->pChild;
        vector<string> localNames;
        int n = 0;
        bool err = false;

        bool bDebugParams = false;
        if (bDebugParams) {
            cout << "InputData: ";
            print(input_data, local_symbols, ISAtom::DecorType::UNICODE, true);
            cout << endl;
            cout << "pvars: ";
            print(pvars, local_symbols, ISAtom::DecorType::UNICODE, true);
            cout << endl;
            cout << "pfunc: ";
            print(pfunc, local_symbols, ISAtom::DecorType::UNICODE, true);
            cout << endl;
        }
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
        if (getListLen(input_data) != n - skipper) {
            err = true;
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Lambda requires " + std::to_string(n - skipper) + " arguments, " + std::to_string(getListLen(input_data)) + " given";
            pop_local_symbols(local_symbols);
            return pRes;
        }

        ISAtom *pCurVar;
        const ISAtom *pInp = input_data;
        for (auto var_name : localNames) {
            // General Quote-is-pNext mess:
            bool bQuoted = false;
            if (pInp->t == ISAtom::TokType::QUOTE) {
                pInp = pInp->pNext;
                bQuoted = true;
            }
            ISAtom *pS = gca(pInp);
            if (pInp->pChild) pS->pChild = copyList(pInp->pChild);
            pS->pNext = gca();
            /*
            cout << "L_EV_1: " << var_name << " -> ";
            print(pS, local_symbols, ISAtom::DecorType::UNICODE, true);
            cout << endl;
            */
            bool bDoEval = true;
            if (pS->t == ISAtom::TokType::STRING || pS->t == ISAtom::TokType::SYMBOL) {
                string func_name = pS->vals;
                if (is_inbuilt(func_name) || is_defined_func(func_name)) {
                    bDoEval = false;
                    // cout << "Not evaluating indirect function: " << func_name << endl;
                }
            }
            if (bQuoted) {
                bDoEval = false;
                // cout << "BUGGY?"; Probably not...
            }
            ISAtom *pT;
            if (bDoEval)
                pT = eval(pS, local_symbols);
            else
                pT = copyList(pS);

            set_local_symbol(var_name, pT, local_symbols);
            deleteList(pS, "LAM-DELSYM");
            pInp = pInp->pNext;
        }
        p = eval(pfunc, local_symbols);
        pop_local_symbols(local_symbols);
        deleteList(pRes, "lambda 1");
        return p;
    }

    ISAtom *eval_func(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols) {
        ISAtom *pRes = gca();
        string fun_name = pisa->vals;
        if (is_defined_func(fun_name)) {
            ISAtom *pDef = funcs[fun_name];
            ISAtom *pvars = gca(pDef);
            if (pDef->pChild) pvars->pChild = copyList(pDef->pChild);
            ISAtom *pfunc = pDef->pNext;
            ISAtom *p = lambda_eval(pisa->pNext, local_symbols, pvars, pfunc, 1);
            deleteList(pRes, "ev_func 1");
            deleteList(pvars, "ev_func 2");
            return p;
        } else {
            pRes->t = ISAtom::TokType::ERROR;
            pRes->vals = "Undefined function >" + pisa->vals + "< Internal error.";
            return pRes;
        }
    }

    ISAtom *eval(const ISAtom *pisa, vector<map<string, ISAtom *>> &local_symbols, bool func_only = false, bool bNested = false) {
        ISAtom *pN, *pRet = nullptr;  //, *pReti;

        ISAtom *p = (ISAtom *)pisa;
        pN = p->pNext;

        bool bShowEval = false;
        if (bShowEval) {
            cout << "Eval: ";
            print(pisa, local_symbols, ISAtom::DecorType::NONE, true);
            cout << endl;
        }

        switch (p->t) {
        case ISAtom::TokType::QUOTE:
            pRet = copyList(pN);
            // pRet = copyList(p);
            // cout << "BUGGY?";
            return pRet;
            break;
        case ISAtom::TokType::LIST:
            if (pisa->pChild->vals == "lambda") {
                ISAtom *pvars = gca(pisa->pChild->pNext);
                if (pisa->pChild->pNext->pChild) pvars->pChild = copyList(pisa->pChild->pNext->pChild);
                pRet = lambda_eval(pisa->pNext, local_symbols, pvars, pisa->pChild->pNext->pNext);
                deleteList(pvars, "LIST-LAMBDA");
            } else {
                if (bNested && pisa->pNext && pisa->pNext->t != ISAtom::TokType::NIL) {
                    if (bShowEval) {
                        cout << "CONTINUE on list eval: ";
                        print(pisa->pNext, local_symbols, ISAtom::DecorType::NONE, true);
                        cout << endl;
                    }
                    ISAtom *pEv = pisa->pChild;
                    ISAtom *pNx = pisa->pNext;

                    pRet = eval(pEv, local_symbols, true);
                    if (bShowEval) {
                        cout << "EV" << endl
                             << "pC: ";
                        print(pEv, local_symbols, ISAtom::DecorType::UNICODE, true);
                        cout << endl
                             << "pCN: ";
                        print(pEv->pNext, local_symbols, ISAtom::DecorType::UNICODE, true);
                        cout << endl
                             << "pN: ";
                        print(pNx, local_symbols, ISAtom::DecorType::UNICODE, true);
                        cout << endl;
                    }
                    if (!pRet->pNext) {
                        pRet->pNext = copyList(pisa->pNext);
                        ISAtom *pRetT = eval(pRet, local_symbols, true);
                        deleteList(pRet, "Indirect eval intermediate");
                        pRet = pRetT;
                    } else {
                        cout << "INTERNAL ERROR Wrong assumption on indirect nexts!";
                    }
                } else {
                    pRet = eval(pisa->pChild, local_symbols, true);
                }
                if (!pRet) {
                    cout << "EVAL returned nulltpr! ";
                    print(pisa, local_symbols, ISAtom::DecorType::UNICODE, true);
                    cout << endl;
                }
            }

            if (bShowEval && pRet) {
                cout << " = ";
                print(pRet, local_symbols, ISAtom::DecorType::UNICODE, true);
                cout << endl;
            }

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
                if (func_only) {
                    ISAtom *pS = gca(pisa);
                    ISAtom *pResolve = eval_symbol(pS, local_symbols);
                    if (pResolve->t == ISAtom::TokType::STRING || pResolve->t == ISAtom::TokType::SYMBOL) {
                        string func_name = pResolve->vals;
                        // cout << "translated func-name: >" << func_name << "<";
                        deleteList(pResolve, "Sym2Func resolver 1");
                        deleteList(pS, "Sym2Func resolver 1.1");
                        ISAtom *pCpisa = copyList(pisa);
                        pCpisa->vals = func_name;
                        if (is_inbuilt(pCpisa->vals)) {
                            pRet = inbuilts[pCpisa->vals](pCpisa->pNext, local_symbols);
                            deleteList(pCpisa, "Sym2Func resolver 2");
                            return pRet;
                        } else if (is_defined_func(pCpisa->vals)) {
                            pRet = eval_func(pCpisa, local_symbols);
                            deleteList(pCpisa, "Sym2Func resolver 3");
                            return pRet;
                        } else {
                            // didn't work
                            deleteList(pCpisa, "Sym2Func resolver 4");
                        }
                    } else {
                        deleteList(pResolve, "Sym2Func resover 5");
                        deleteList(pS, "Sym2Func resolver 5.1");
                    }
                }
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
        bool bShowEval = false;

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
            case ISAtom::TokType::LIST:
                if (is_quote) {
                    pCEi = copyList(pi);
                    is_quote = false;
                } else {
                    if (pi->pChild->pChild) {
                        if (bShowEval) {
                            cout << "INDIRECT! ";
                            print(pi->pChild, local_symbols, ISAtom::DecorType::UNICODE, true);
                            cout << endl;
                        }
                        pCEi = eval(pi->pChild, local_symbols, true, true);
                        if (bShowEval) {
                            cout << "IND_RESU: ";
                            print(pCEi, local_symbols, ISAtom::DecorType::UNICODE, true);
                            cout << endl;
                        }
                    } else {
                        pCEi = eval(pi->pChild, local_symbols, true);
                    }
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
