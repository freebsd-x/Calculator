// Calculator.xapp — Scientific calculator for FreeBSD-X
//
// Features:
//   • Proper infix expression parser with operator precedence
//       (Shunting-yard / recursive-descent: +,-,*,/,^ with unary minus)
//   • Scientific functions: sin, cos, tan, asin, acos, atan, ln, log,
//       sqrt, abs, ceil, floor, exp  (deg/rad toggle)
//   • Constants: π, e
//   • Basic ↔ Scientific mode toggle (compact vs expanded button grid)
//   • Scrollable calculation history sidebar; click entry to recall expression
//   • Keyboard-driven input via TextInputWidget (Enter=evaluate, Esc=clear)
//   • Button grid appends to expression field — both mouse and keyboard work

#include "xkit/widget.h"
#include "xkit/application.h"
#include "xkit/xbus_client.h"
#include "shared/protocol.h"
#include <memory>
#include <string>
#include <vector>
#include <cstring>
#include <cmath>
#include <cctype>
#include <stdexcept>
#include <algorithm>
#include <functional>
#include <sstream>
#include <iomanip>

using namespace fbx::xkit;
using namespace fbx::shared;

// ─── Expression Parser ────────────────────────────────────────────────────────
// Recursive descent: handles +,-,*,/,^ (right-assoc), unary -, parentheses,
// named functions, and constants π / e.

namespace {

struct Parser {
    const char* p;
    bool deg_mode; // true = trig args in degrees

    void skip() { while (*p == ' ' || *p == '\t') ++p; }

    double number() {
        skip();
        char* end;
        double v = std::strtod(p, &end);
        if (end == p) throw std::runtime_error("expected number");
        p = end;
        return v;
    }

    // Try to consume a string (case-insensitive)
    bool tryWord(const char* w) {
        skip();
        size_t n = std::strlen(w);
        if (strncasecmp(p, w, n) == 0 && !std::isalnum((unsigned char)p[n])) {
            p += n;
            return true;
        }
        return false;
    }

    double expr();    // additive
    double term();    // multiplicative
    double power();   // ^ right-assoc
    double unary();   // unary -
    double primary(); // number | (expr) | func(expr) | const

    double toRad(double x) { return deg_mode ? x * M_PI / 180.0 : x; }
    double fromRad(double x) { return deg_mode ? x * 180.0 / M_PI : x; }
};

double Parser::primary() {
    skip();

    // Parenthesised sub-expression
    if (*p == '(') {
        ++p;
        double v = expr();
        skip();
        if (*p == ')') ++p;
        return v;
    }

    // Named functions — longest-match first
    if (tryWord("asin"))  { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return fromRad(std::asin(a)); }
    if (tryWord("acos"))  { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return fromRad(std::acos(a)); }
    if (tryWord("atan"))  { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return fromRad(std::atan(a)); }
    if (tryWord("sin"))   { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::sin(toRad(a)); }
    if (tryWord("cos"))   { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::cos(toRad(a)); }
    if (tryWord("tan"))   { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::tan(toRad(a)); }
    if (tryWord("sqrt"))  { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::sqrt(a); }
    if (tryWord("ln"))    { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::log(a); }
    if (tryWord("log"))   { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::log10(a); }
    if (tryWord("exp"))   { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::exp(a); }
    if (tryWord("abs"))   { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::fabs(a); }
    if (tryWord("ceil"))  { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::ceil(a); }
    if (tryWord("floor")) { skip(); if(*p=='('){++p;} double a=expr(); skip(); if(*p==')'){++p;} return std::floor(a); }

    // Constants
    if (tryWord("pi") || tryWord("π")) return M_PI;
    if (tryWord("e"))                  return M_E;
    if (tryWord("inf"))                return INFINITY;

    // Number literal
    return number();
}

double Parser::unary() {
    skip();
    if (*p == '-') { ++p; return -unary(); }
    if (*p == '+') { ++p; return  unary(); }
    return primary();
}

double Parser::power() {
    double base = unary();
    skip();
    if (*p == '^') { ++p; return std::pow(base, power()); } // right-assoc
    return base;
}

double Parser::term() {
    double v = power();
    while (true) {
        skip();
        if (*p == '*')      { ++p; v *= power(); }
        else if (*p == '/') { ++p; double d = power(); if (d==0) throw std::runtime_error("div/0"); v /= d; }
        else if (*p == '%') { ++p; double d = power(); if (d==0) throw std::runtime_error("mod/0"); v = std::fmod(v,d); }
        else break;
    }
    return v;
}

double Parser::expr() {
    double v = term();
    while (true) {
        skip();
        if (*p == '+')      { ++p; v += term(); }
        else if (*p == '-') {
            // Peek: if '-' is followed by a digit/( it's a binary minus
            ++p; v -= term();
        }
        else break;
    }
    return v;
}

// Evaluate an expression string. Throws std::runtime_error on parse error.
static double evaluate(const std::string& expr_str, bool deg_mode) {
    Parser p{ expr_str.c_str(), deg_mode };
    double result = p.expr();
    p.skip();
    if (*p.p != '\0')
        throw std::runtime_error(std::string("unexpected: ") + p.p);
    return result;
}

// Format a double result for display
static std::string fmtResult(double v) {
    if (std::isinf(v)) return v > 0 ? "∞" : "-∞";
    if (std::isnan(v)) return "NaN";
    // If integer-valued and small enough, show as integer
    if (v == std::floor(v) && std::fabs(v) < 1e15) {
        long long iv = static_cast<long long>(v);
        return std::to_string(iv);
    }
    // Otherwise up to 12 significant figures, strip trailing zeros
    char buf[64];
    std::snprintf(buf, sizeof(buf), "%.12g", v);
    return buf;
}

} // anonymous namespace

// ─── App ──────────────────────────────────────────────────────────────────────

int main(int /*argc*/, char** /*argv*/) {

    // ── State ─────────────────────────────────────────────────────────────────
    static bool sci_mode  = false;
    static bool deg_mode  = true;   // degrees (true) vs radians (false)
    static std::vector<std::string> history; // "expr = result" entries

    // Widgets that need cross-function access
    static std::shared_ptr<TextInputWidget> expr_input;
    static std::shared_ptr<TextWidget>      result_lbl;
    static std::shared_ptr<TextWidget>      mode_lbl;   // DEG/RAD
    static std::shared_ptr<FlexColumn>      history_col;
    static std::shared_ptr<FlexColumn>      btn_grid_col;
    static std::shared_ptr<FlexRow>         root_body;

    // Forward declarations
    static std::function<void()> rebuild_buttons;
    static std::function<void()> rebuild_history;
    static std::function<void()> do_evaluate;
    static std::function<void(const std::string&)> append;

    // ── Evaluate current expression ───────────────────────────────────────────
    do_evaluate = []() {
        if (!expr_input) return;
        std::string expr = expr_input->text();
        if (expr.empty()) return;
        std::string res;
        try {
            // Replace display symbols with parser-friendly tokens
            std::string clean = expr;
            // × → *,  ÷ → /,  π → pi,  √ → sqrt(   — handled by replacing chars
            for (size_t i = 0; i < clean.size(); ) {
                // UTF-8 multi-byte: crude replacement
                unsigned char c = (unsigned char)clean[i];
                if (c == 0xC3) {
                    unsigned char c2 = (unsigned char)clean[i+1];
                    if (c2 == 0x97) { clean.replace(i,2,"*");  continue; } // ×
                    if (c2 == 0xB7) { clean.replace(i,2,"/");  continue; } // ÷
                    if (c2 == 0xB2) { clean.replace(i,2,"^2"); continue; } // ²
                }
                if (c == 0xCF && (unsigned char)clean[i+1] == 0x80) { // π
                    clean.replace(i,2,"pi"); continue;
                }
                if (c == 0xE2) {
                    // √ = E2 88 9A
                    if (i+2<clean.size() &&
                        (unsigned char)clean[i+1]==0x88 &&
                        (unsigned char)clean[i+2]==0x9A) {
                        clean.replace(i,3,"sqrt("); continue;
                    }
                    // ∞
                    if (i+2<clean.size() &&
                        (unsigned char)clean[i+1]==0x88 &&
                        (unsigned char)clean[i+2]==0x9E) {
                        clean.replace(i,3,"inf"); continue;
                    }
                }
                ++i;
            }
            double v = evaluate(clean, deg_mode);
            res = fmtResult(v);
        } catch (const std::exception& ex) {
            res = std::string("Error: ") + ex.what();
        }

        if (result_lbl) result_lbl->setText(res);

        // Add to history (skip error entries)
        if (!res.starts_with("Error")) {
            history.push_back(expr + " = " + res);
            if (history.size() > 50) history.erase(history.begin());
            rebuild_history();
            // Place result into expression for chaining
            expr_input->setText(res);
        }
    };

    // ── Append token to expression ────────────────────────────────────────────
    append = [](const std::string& tok) {
        if (!expr_input) return;
        std::string cur = expr_input->text();
        // If the current content is a plain number result (from prior eval),
        // clicking a function/operator continues; clicking a digit replaces.
        expr_input->setText(cur + tok);
    };

    // ── Rebuild history list ──────────────────────────────────────────────────
    rebuild_history = []() {
        if (!history_col) return;
        history_col->clearChildren();

        auto hdr = std::make_shared<TextWidget>("History", 12.0f);
        hdr->setColor(Color{0.6f, 0.6f, 0.6f, 1.0f});
        history_col->addChild([&]{ auto c = std::make_shared<FlexColumn>(std::vector<std::shared_ptr<Widget>>{hdr}); c->setPadding(6.0f); return c; }());
        history_col->addChild(std::make_shared<DividerWidget>());

        if (history.empty()) {
            auto empty = std::make_shared<TextWidget>("No history", 11.0f);
            empty->setColor(Color{0.4f, 0.4f, 0.4f, 1.0f});
            history_col->addChild(
                [&]{ auto c = std::make_shared<FlexColumn>(std::vector<std::shared_ptr<Widget>>{empty}); c->setPadding(6.0f); return c; }());
            return;
        }
        // Show newest first
        for (int i = (int)history.size() - 1; i >= 0; --i) {
            const std::string& entry = history[i];
            // Extract the expression part (before " = ")
            std::string entry_expr = entry;
            auto sep = entry.find(" = ");
            if (sep != std::string::npos)
                entry_expr = entry.substr(0, sep);

            auto lbl = std::make_shared<TextWidget>(entry, 10.5f);
            lbl->setColor(Color{0.72f, 0.72f, 0.72f, 1.0f});
            std::string recall = entry_expr;
            auto btn = std::make_shared<ButtonWidget>(lbl, [recall]() {
                if (expr_input) expr_input->setText(recall);
            });
            btn->setPadding(4.0f);
            btn->setBackgroundColor(Color{0.12f, 0.12f, 0.14f, 1.0f});
            history_col->addChild(btn);
            history_col->addChild(std::make_shared<DividerWidget>(0.4f));
        }

        // Clear history button
        auto clr = std::make_shared<ButtonWidget>(
            std::make_shared<TextWidget>("Clear history", 11.0f),
            []() { history.clear(); rebuild_history(); });
        clr->setBackgroundColor(Color{0.22f, 0.12f, 0.12f, 1.0f});

        clr->setPadding(6.0f);
        history_col->addChild(std::make_shared<Spacer>(0, 8));
        history_col->addChild(clr);
    };

    // ── Build button grid ─────────────────────────────────────────────────────
    rebuild_buttons = []() {
        if (!btn_grid_col) return;
        btn_grid_col->clearChildren();

        // Helper: make a calculator button
        auto mk = [](const std::string& label,
                     std::function<void()> action,
                     Color bg = {0.20f, 0.20f, 0.22f, 1.0f},
                     float fs = 15.0f) {
            auto lbl = std::make_shared<TextWidget>(label, fs);
            lbl->setColor(Color{0.92f, 0.92f, 0.92f, 1.0f});
            auto btn = std::make_shared<ButtonWidget>(lbl, std::move(action));
            btn->setBackgroundColor(bg);

            btn->setPadding(0.0f);
            btn->setFlexGrow(1.0f);     // share the row's width; the row stretches it to its height
            return btn;
        };

        Color op_bg  = {0.26f, 0.26f, 0.30f, 1.0f};
        Color fn_bg  = {0.18f, 0.22f, 0.30f, 1.0f};
        Color eq_bg  = {0.22f, 0.48f, 0.22f, 1.0f};
        Color clr_bg = {0.38f, 0.16f, 0.16f, 1.0f};

        auto row = [](std::vector<std::shared_ptr<Widget>> btns,
                      float gap = 4.0f) {
            // Rows share the keypad's height (min 40 px) and stretch their
            // buttons to it, so the keypad fills the window instead of a
            // fixed 50 px-per-row block in the corner.
            auto r = std::make_shared<FlexRow>(std::move(btns));
            r->setGap(gap);
            r->setFlexGrow(1.0f);
            r->setCrossAxisAlignment(CrossAxisAlignment::Stretch);
            return r;
        };

        // ── Scientific row 1: trig ────────────────────────────────────────────
        if (sci_mode) {
            btn_grid_col->addChild(row({
                mk("sin",  [](){ append("sin("); },  fn_bg),
                mk("cos",  [](){ append("cos("); },  fn_bg),
                mk("tan",  [](){ append("tan("); },  fn_bg),
                mk("asin", [](){ append("asin("); }, fn_bg),
                mk("acos", [](){ append("acos("); }, fn_bg),
                mk("atan", [](){ append("atan("); }, fn_bg),
            }));
            btn_grid_col->addChild(std::make_shared<Spacer>(0, 4));

            // ── Scientific row 2: log/exp/root ────────────────────────────────
            btn_grid_col->addChild(row({
                mk("ln",   [](){ append("ln(");   }, fn_bg),
                mk("log",  [](){ append("log(");  }, fn_bg),
                mk("√",    [](){ append("sqrt("); }, fn_bg),
                mk("xⁿ",   [](){ append("^");     }, fn_bg),
                mk("x²",   [](){ append("^2");    }, fn_bg),
                mk("exp",  [](){ append("exp(");  }, fn_bg),
            }));
            btn_grid_col->addChild(std::make_shared<Spacer>(0, 4));

            // ── Scientific row 3: constants/misc ─────────────────────────────
            btn_grid_col->addChild(row({
                mk("π",    [](){ append("π");     }, fn_bg),
                mk("e",    [](){ append("e");     }, fn_bg),
                mk("(",    [](){ append("(");     }, op_bg),
                mk(")",    [](){ append(")");     }, op_bg),
                mk("%",    [](){ append("%");     }, op_bg),
                mk(deg_mode ? "DEG" : "RAD", [](){
                    deg_mode = !deg_mode;
                    if (mode_lbl)
                        mode_lbl->setText(deg_mode ? "DEG" : "RAD");
                    rebuild_buttons();
                }, Color{0.22f,0.26f,0.35f,1.0f}),
            }));
            btn_grid_col->addChild(std::make_shared<DividerWidget>());
            btn_grid_col->addChild(std::make_shared<Spacer>(0, 4));
        } else {
            // Basic mode: just parentheses + percent row
            btn_grid_col->addChild(row({
                mk("(",    [](){ append("(");  }, op_bg),
                mk(")",    [](){ append(")");  }, op_bg),
                mk("%",    [](){ append("%");  }, op_bg),
                mk("C",    [](){
                    if (expr_input) expr_input->setText("");
                    if (result_lbl) result_lbl->setText("");
                }, clr_bg),
            }));
            btn_grid_col->addChild(std::make_shared<Spacer>(0, 4));
        }

        // ── Main numeric + operator grid (shared across modes) ────────────────
        // Row: 7 8 9 ÷ [C in sci / already done in basic]
        if (sci_mode) {
            btn_grid_col->addChild(row({
                mk("7",  [](){ append("7"); }),
                mk("8",  [](){ append("8"); }),
                mk("9",  [](){ append("9"); }),
                mk("÷",  [](){ append("/"); }, op_bg),
                mk("C",  [](){
                    if (expr_input) expr_input->setText("");
                    if (result_lbl) result_lbl->setText("");
                }, clr_bg),
                mk("⌫",  [](){
                    if (!expr_input) return;
                    std::string s = expr_input->text();
                    if (!s.empty()) {
                        // Pop UTF-8 char
                        size_t n = s.size();
                        while (n > 0 && (s[n-1] & 0xC0) == 0x80) --n;
                        if (n > 0) --n;
                        expr_input->setText(s.substr(0, n));
                    }
                }, op_bg),
            }));
        } else {
            btn_grid_col->addChild(row({
                mk("7",  [](){ append("7"); }),
                mk("8",  [](){ append("8"); }),
                mk("9",  [](){ append("9"); }),
                mk("÷",  [](){ append("/"); }, op_bg),
                mk("⌫",  [](){
                    if (!expr_input) return;
                    std::string s = expr_input->text();
                    if (!s.empty()) {
                        size_t n = s.size();
                        while (n > 0 && (s[n-1] & 0xC0) == 0x80) --n;
                        if (n > 0) --n;
                        expr_input->setText(s.substr(0, n));
                    }
                }, op_bg),
            }));
        }
        btn_grid_col->addChild(std::make_shared<Spacer>(0, 4));

        btn_grid_col->addChild(row({
            mk("4",  [](){ append("4"); }),
            mk("5",  [](){ append("5"); }),
            mk("6",  [](){ append("6"); }),
            mk("×",  [](){ append("*"); }, op_bg),
            mk("−",  [](){ append("-"); }, op_bg),
        }));
        btn_grid_col->addChild(std::make_shared<Spacer>(0, 4));

        btn_grid_col->addChild(row({
            mk("1",  [](){ append("1"); }),
            mk("2",  [](){ append("2"); }),
            mk("3",  [](){ append("3"); }),
            mk("+",  [](){ append("+"); }, op_bg),
            mk("^",  [](){ append("^"); }, op_bg),
        }));
        btn_grid_col->addChild(std::make_shared<Spacer>(0, 4));

        // Bottom row: 0 . ± =
        auto zero_btn = std::make_shared<ButtonWidget>(
            [](){ auto t=std::make_shared<TextWidget>("0",15.0f); t->setColor({0.92f,0.92f,0.92f,1.0f}); return t; }(),
            [](){ append("0"); });
        zero_btn->setBackgroundColor(Color{0.20f,0.20f,0.22f,1.0f});

        zero_btn->setFlexGrow(2.0f);  // wider zero

        btn_grid_col->addChild(row({
            zero_btn,
            mk(".",  [](){ append("."); }),
            mk("±",  [](){
                if (!expr_input) return;
                std::string s = expr_input->text();
                if (!s.empty() && s[0] == '-') s = s.substr(1);
                else s = "-" + s;
                expr_input->setText(s);
            }, op_bg),
            mk("=",  [](){ do_evaluate(); }, eq_bg, 18.0f),
        }));
    };

    // ── Expression input ──────────────────────────────────────────────────────
    expr_input = std::make_shared<TextInputWidget>("");
    expr_input->setPlaceholder("Type an expression, or use the keys");
    expr_input->setOnChanged([](const std::string&) { do_evaluate(); });

    // ── Result display ────────────────────────────────────────────────────────
    result_lbl = std::make_shared<TextWidget>("0", 28.0f);
    result_lbl->setColor(Color{0.88f, 0.95f, 0.88f, 1.0f});

    // ── Display area ──────────────────────────────────────────────────────────
    mode_lbl = std::make_shared<TextWidget>("DEG", 10.0f);
    mode_lbl->setColor(Color{0.45f, 0.60f, 0.45f, 1.0f});

    auto sci_btn = std::make_shared<ButtonWidget>(
        std::make_shared<TextWidget>("SCI", 10.0f),
        []() {
            sci_mode = !sci_mode;
            rebuild_buttons();
        });
    sci_btn->setBackgroundColor(Color{0.20f, 0.25f, 0.30f, 1.0f});

    sci_btn->setPadding(4.0f);

    auto display_top = std::make_shared<FlexRow>(
        std::vector<std::shared_ptr<Widget>>{
            mode_lbl,
            std::make_shared<Spacer>(8, 0),
            sci_btn,
        });
    display_top->setCrossAxisAlignment(CrossAxisAlignment::Center);
    display_top->setPadding(4.0f);

    auto display_area = std::make_shared<FlexColumn>(
        std::vector<std::shared_ptr<Widget>>{
            display_top,
            [&]{ auto c = std::make_shared<FlexColumn>(std::vector<std::shared_ptr<Widget>>{expr_input}); c->setPadding(6.0f);
                  c->setCrossAxisAlignment(CrossAxisAlignment::Stretch); return c; }(),   // the field spans the width
            [&]{ auto c = std::make_shared<FlexColumn>(std::vector<std::shared_ptr<Widget>>{result_lbl}); c->setPadding(8.0f); return c; }(),
            std::make_shared<DividerWidget>(1.0f),
        });
    display_area->setBackgroundColor(Color{0.07f, 0.08f, 0.07f, 1.0f});
    display_area->setGap(0.0f);

    // ── Button grid ───────────────────────────────────────────────────────────
    btn_grid_col = std::make_shared<FlexColumn>(
        std::vector<std::shared_ptr<Widget>>{});
    btn_grid_col->setPadding(8.0f);
    btn_grid_col->setGap(4.0f);
    btn_grid_col->setFlexGrow(1.0f);   // the keypad takes the rest of the panel
    btn_grid_col->setBackgroundColor(Color{0.09f, 0.09f, 0.11f, 1.0f});

    // ── History sidebar ───────────────────────────────────────────────────────
    history_col = std::make_shared<FlexColumn>(
        std::vector<std::shared_ptr<Widget>>{});
    history_col->setPadding(8.0f);
    history_col->setGap(2.0f);
    history_col->setFixedSize(180.0f, 0.0f);
    history_col->setBackgroundColor(Color{0.10f, 0.10f, 0.13f, 1.0f});

    auto history_scroll = std::make_shared<ScrollViewWidget>(history_col);
    history_scroll->setFixedSize(180.0f, 0.0f);   // a fixed-width sidebar (no flex-grow: it competed with the keypad for width)

    // ── Calculator panel (display + buttons) ──────────────────────────────────
    auto calc_panel = std::make_shared<FlexColumn>(
        std::vector<std::shared_ptr<Widget>>{
            display_area,
            btn_grid_col,
        });
    calc_panel->setFlexGrow(1.0f);
    calc_panel->setGap(0.0f);
    calc_panel->setCrossAxisAlignment(CrossAxisAlignment::Stretch);

    // ── Root ──────────────────────────────────────────────────────────────────
    root_body = std::make_shared<FlexRow>(std::vector<std::shared_ptr<Widget>>{
        calc_panel,
        std::make_shared<DividerWidget>(),
        history_scroll,
    });
    root_body->setGap(0.0f);
    root_body->setFlexGrow(1.0f);
    root_body->setCrossAxisAlignment(CrossAxisAlignment::Stretch);   // panels fill the height

    auto root = std::make_shared<FlexColumn>(std::vector<std::shared_ptr<Widget>>{
        root_body,
    });
    root->setGap(0.0f);
    root->setBackgroundColor(Color{0.09f, 0.09f, 0.11f, 1.0f});

    // Initial build
    rebuild_buttons();
    rebuild_history();

    return XApplication::Run(root, "/tmp/freebsdx.sock", /*sandboxed=*/false,
                             /*is_shell=*/false,
        [](XBusClient&) {
            XApplication::setTitle("Calculator");
        });
}
