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
    IndraScheme() {
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

    bool eval(ISAtom *pisa) {
        return false;
    }
};

}  // namespace insch
