package com.newardassociates.lispy;

import java.io.IOException;
import java.io.Reader;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.LinkedList;

/**
 * Streaming S-expression parser that extends Reader.
 * Supports:
 *   - Lists
 *   - Atoms
 *   - Quoted strings with escapes
 *   - Semicolon comments
 */
public class SExprReader implements AutoCloseable {

    // SExprs are basically either Atoms or Lists.
    // If they are Atoms, they can be either symbols, string literals,
    // or numbers (int or float).
    // For simplicity, we just treat everything as strings here,
    // and let the evaluator handle type interpretation.
    //
    public interface SExpr {}
    public static class Atom implements SExpr {
        public final Object value; 
        public final boolean isSymbol;
        public final boolean isStringLiteral;
        public final boolean isNumber;
        public final boolean isInteger;
        public final boolean isDouble;

        public Atom(String v) { 
            if (v == null) {
                throw new RuntimeException("Atom value cannot be null");
            }

            // Figure out what this is
            if (v.startsWith("\"") && v.endsWith("\"")) {
                // String literal
                isSymbol = false;
                isStringLiteral = true;
                isNumber = isInteger = isDouble = false;
                value = v.substring(1, v.length() - 1);
            } else if (v.matches("-?\\d+")) {
                // Integer
                isSymbol = false;
                isStringLiteral = false;
                isNumber = true;
                isInteger = true;
                isDouble = false;
                value = Integer.parseInt(v);
            } else if (v.matches("-?\\d*\\.\\d+")) {
                // Double
                isSymbol = false;
                isStringLiteral = false;
                isNumber = true;
                isDouble = true;
                isInteger = false;
                value = Double.parseDouble(v);
            } else {
                // Symbol
                isSymbol = true;
                isStringLiteral = false;
                isNumber = isInteger = isDouble = false;
                value = v;
            }
        }
        public String toString() { return value.toString(); }

        public String toSymbol() { return (String)value; }
        public String toStringLiteral() { return "\"" + ((String)value) + "\""; }
        public Integer toInteger() { return (Integer)value; }
        public Double toDouble() { return (Double)this.value; }
    }

    public static class ListExpr implements SExpr {
        public final List<SExpr> value = new ArrayList<>();
        public String toString() { return value.toString(); }
    }

    /* ===== Reader internals ===== */

    private final Reader in;
    private int pushback = -1; // for 1-char lookahead

    public SExprReader(Reader source) {
        this.in = source;
    }

    public int read() throws IOException {
        // Pass-through read with pushback support
        if (pushback != -1) {
            int c = pushback;
            pushback = -1;
            return c;
        }
        return in.read();
    }

    public void close() throws IOException {
        in.close();
    }

    private int peek() throws IOException {
        if (pushback == -1) pushback = in.read();
        return pushback;
    }

    private boolean eof(int c) { return c == -1; }

    public SExpr parse() throws IOException {
        skipWhitespaceAndComments();
        if (eof(peek())) return null;       // End-of-stream
        return parseExpr();
    }

    private SExpr parseExpr() throws IOException {
        skipWhitespaceAndComments();

        int c = peek();
        if (c == '(') return parseList();
        if (c == '"') return parseString();
        return parseAtom();
    }

    private ListExpr parseList() throws IOException {
        expect('(');
        ListExpr list = new ListExpr();

        skipWhitespaceAndComments();
        while (peek() != ')') {
            if (eof(peek()))
                throw new RuntimeException("Unexpected EOF inside list");
            list.value.add(parseExpr());
            skipWhitespaceAndComments();
        }
        expect(')');
        return list;
    }

    private Atom parseAtom() throws IOException {
        StringBuilder sb = new StringBuilder();

        while (true) {
            int p = peek();
            if (eof(p)) break;
            char c = (char)p;

            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == ';')
                break;

            sb.append(c);
            read();
        }

        if (sb.length() == 0)
            throw new RuntimeException("Expected atom but found: " + (char)peek());

        return new Atom(sb.toString());
    }

    private Atom parseString() throws IOException {
        expect('"');

        StringBuilder sb = new StringBuilder("\"");
        while (true) {
            int c = read();
            if (c == -1) throw new RuntimeException("Unexpected EOF in string literal");

            if (c == '\\') { // escape
                int esc = read();
                if (esc == -1) throw new RuntimeException("Unexpected EOF after escape");
                switch (esc) {
                    case '"': sb.append('"'); break;
                    case '\\': sb.append('\\'); break;
                    case 'n': sb.append('\n'); break;
                    case 't': sb.append('\t'); break;
                    default:  sb.append((char)esc); break; // raw escape
                }
            } else if (c == '"') {
                sb.append('"');
                break; // end of string
            } else {
                sb.append((char)c);
            }
        }

        System.out.println("Parsed string literal: " + sb.toString());
        return new Atom(sb.toString());
    }

    private void skipWhitespaceAndComments() throws IOException {
        while (true) {
            int c = peek();
            if (c == -1) return;

            // whitespace
            if (Character.isWhitespace((char)c)) {
                read();
                continue;
            }

            // comment
            if (c == ';') {
                read(); // consume ;
                while (true) {
                    int d = read();
                    if (d == -1 || d == '\n') break;
                }
                continue;
            }

            return; // non-whitespace, non-comment
        }
    }

    private void expect(char expected) throws IOException {
        int c = read();
        if (c != expected)
            throw new RuntimeException("Expected '" + expected + "' but got '" + (char)c + "'");
    }
}
