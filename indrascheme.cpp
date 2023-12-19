#include "indrascheme.h"

#include <iostream>  // cout, cin, streambuf, hex, endl, sgetc, sbumpc
#include <iomanip>   // setw, setfill
#include <fstream>   // fstream
#include <chrono>    // perf timings

// These inclusions required to set terminal mode.
#include <termios.h>  // struct termios, tcgetattr(), tcsetattr()
#include <stdio.h>    // perror(), stderr, stdin, fileno()

using insch::IndraScheme;
using insch::ISAtom;
using std::endl;
using std::string;

vector<string> input_history;
size_t max_input_history_len = 10;
size_t input_history_stack = 0;

bool initCharReader(struct termios *pTermSaved) {
    struct termios t;
    // Set terminal to single character mode.
    tcgetattr(fileno(stdin), &t);
    *pTermSaved = t;

    t.c_lflag &= (~ICANON & ~ECHO);
    t.c_cc[VTIME] = 0;
    t.c_cc[VMIN] = 1;
    if (tcsetattr(fileno(stdin), TCSANOW, &t) < 0) {
        std::cout << "Unable to set terminal to single character mode"
                  << std::endl;
        return false;
    }
    return true;
}

bool quitCharReader(struct termios *pTermSaved) {

    // Restore terminal mode.
    if (tcsetattr(fileno(stdin), TCSANOW, pTermSaved) < 0) {
        std::cout << "Unable to restore terminal mode" << std::endl;
        return false;
    }
    return true;
}

string charReader(string prompt, bool *pQuit) {
    *pQuit = false;
    struct termios termSaved;
    initCharReader(&termSaved);
    // Read single characters from cin.
    std::streambuf *pbuf = std::cin.rdbuf();
    bool done = false;
    bool esc_mode = false;
    string esc_string = "";
    string inp = "";
    string large_space = "                                                                                ";  // XXX
    std::cout << prompt;

    while (!done) {
        char c;
        if (pbuf->sgetc() == EOF)
            done = true;
        c = pbuf->sbumpc();
        switch (c) {
        case 0x0a:
        case 0x0d:
            input_history_stack = input_history.size();
            input_history.push_back(inp);
            if (input_history.size() > max_input_history_len) {
                input_history.erase(input_history.begin());
            }
            done = true;
            break;
        case 0x03:
        case 0x04:
            done = true;
            *pQuit = true;
            break;
        case 0x7f:
        case 0x08:
            if (inp.length() > 0) {
                inp = inp.substr(0, inp.length() - 1);
                std::cout << "\r" + prompt + inp + " " << std::flush;
                std::cout << "\r" + prompt + inp << std::flush;
            }
            break;
        case 0x1b:
            esc_mode = true;
            esc_string = "";
            break;
        default:
            if (!esc_mode) {
                inp += c;
                if (c < 32) {
                    std::cout << "[0x" << std::setw(2) << std::setfill('0')
                              << std::hex << int(c) << "]" << std::flush;
                } else {
                    //  std::cout << "[0x" << std::setw(2) << std::setfill('0') <<
                    //  std::hex << int(c) << "]" << std::flush;
                    std::cout << c << std::flush;
                }
            } else {
                esc_string += c;
                if (esc_string == "[A") {
                    if (input_history.size() == input_history_stack + 1) {
                        input_history.push_back(inp);
                        inp = input_history[input_history_stack];
                        cout << "\r" << prompt << inp << large_space << std::flush;
                        cout << "\r" << prompt << inp << std::flush;
                    } else {
                        if (input_history_stack > 0) {
                            --input_history_stack;
                            inp = input_history[input_history_stack];
                            cout << "\r" << prompt << inp << large_space << std::flush;
                            cout << "\r" << prompt << inp << std::flush;
                        }
                    }
                    esc_mode = false;
                    esc_string = "";
                } else if (esc_string == "[B") {
                    if (input_history.size() > input_history_stack + 1) {
                        ++input_history_stack;
                        inp = input_history[input_history_stack];
                        cout << "\r" << prompt << inp << large_space << std::flush;
                        cout << "\r" << prompt << inp << std::flush;
                    }
                    esc_mode = false;
                    esc_string = "";
                } else if (esc_string.length() > 1) {
                    esc_mode = false;
                    esc_string = "";
                }
            }
            break;
        }
    }
    quitCharReader(&termSaved);
    return inp;
}

void repl(std::string &prompt, std::string &prompt2) {
    std::string cmd, inp;
    bool fst;
    string ans;
    IndraScheme ins;

    while (true) {
        cmd = "";
        fst = true;
        while (true) {
            bool bq = false;
            inp = charReader(prompt, &bq);
            cmd += inp + "\n";
            if (bq || cmd == "(quit)\n") {
                return;
            }
            break;
        }
        ISAtom *pisa = ins.parse(cmd);

        // std::cout << std::endl;
        // ins.print(pisa);
        auto start = std::chrono::steady_clock::now();
        std::cout << endl;
        map<string, ISAtom *> ls;
        pisa = ins.chainEval(pisa, ls);
        cout << endl
             << "⟫ ";
        map<string, ISAtom *> lsyms;
        ins.print(pisa, lsyms);

        auto diff = std::chrono::steady_clock::now() - start;
        std::cout << endl;

        std::cout << "Eval dt: "
                  << std::chrono::duration<double, std::nano>(diff).count()
                  << " ns"
                  << ", " << endl;
    }
}

int main(int argc, char *argv[]) {
  const char *szTerm = std::getenv("TERM");
  string prompt, prompt2;
  string term(szTerm);
  if (term=="linux") {
    prompt = "I. "; prompt2 = " > ";
  } else {
    prompt = "ℑ⧽ "; prompt2 = " > ";
  }
    repl(prompt, prompt2);
    std::cout << "end-repl" << std::endl;
    return 0;
}
