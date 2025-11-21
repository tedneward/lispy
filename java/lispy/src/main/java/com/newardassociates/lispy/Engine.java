package com.newardassociates.lispy;

import java.util.List;

import com.newardassociates.lispy.SExprReader;
import static com.newardassociates.lispy.SExprReader.*;

import java.util.LinkedList;

/**
 * The Engine class will drive the Lispy interpreter.
 * 
 * Corresponds directly to lis.py[11:80]. Engine instances are each
 * independent interpreters, and instance methods there correspond
 * with the top-level functions in lis.py.
 * 
 * @author Ted Neward
 */
public class Engine {
    private SExprReader reader;
    private Env env = new Env();
    private List<SExpr> exprs;

    public Engine() { }

    public List<SExpr> parse(String program) throws java.io.IOException {
        reader = new SExprReader(new java.io.StringReader(program));

        SExpr sexpr;
        while ((sexpr = reader.parse()) != null) {
            System.out.println("Parsing " + sexpr.toString());
        }

        return List.of();
    }

    public Object eval(SExprReader.SExpr sexpr) throws java.io.IOException {
        if (sexpr instanceof SExprReader.ListExpr) {
            return eval((SExprReader.ListExpr)sexpr);
        } else if (sexpr instanceof SExprReader.Atom) {
            return eval((SExprReader.Atom)sexpr);
        } else if (sexpr instanceof SExprReader.StringLiteral) {
            return eval((SExprReader.StringLiteral)sexpr);
        } else {
            throw new java.io.IOException("Unknown SExpr type: " + sexpr.getClass().getName());
        }
    }

    public Object eval(List<SExprReader.SExpr> token) throws java.io.IOException {
        if (token.get(0) instanceof SExprReader.Atom) {
            String verb = ((SExprReader.Atom)token.get(0)).value;
            if (verb.equals("begin")) {
                Object result = null;
                for (int i = 1; i < token.size(); i++) {
                    result = eval((SExprReader.SExpr)token.get(i));
                }
                return result;
            }
            else if (verb.equals("+") || verb.equals("*")) {
                boolean add = verb.equals("+") ? 0 : 1;
                int iAcc = add ? 0 : 1;
                double dAcc = add ? 0.0 : 1.0;
                boolean isDouble = false;
                for (int i = 1; i < token.size(); i++) {
                    Object val = eval(token.get(i));
                    if (isDouble) {
                        dAcc = (add ? dAcc + ((Number)val).doubleValue() : dAcc * ((Number)val).doubleValue());
                    } else if (val instanceof Double) {
                        isDouble = true;
                        dAcc = add ? iAcc + ((Double)val).doubleValue() : iAcc * ((Double)val).doubleValue();
                    } else {
                        iAcc = add ? iAcc + ((Number)val).intValue() : iAcc * ((Number)val).intValue();
                }
                return acc;
            }
            else if (verb.equals("-")) {
                return eval(token.get(1)) - eval(token.get(2));
            } 
            else if (verb.equals("/")) {
                SExprReader.Atom atom = (SExprReader.Atom)token.get(1);
                return 1 / atom.toInteger();
            }
        }
        else {
            // ... umm....
        }

        return null;
    }
    public Object eval(SExprReader.Atom sexpr) throws java.io.IOException {
        String val = atom.value;
        try {
            return atom.toInteger();
        } catch (NumberFormatException nfe) {
            try {
                return atom.toDouble();
            } catch (NumberFormatException nfe2) {
                return val;
            }
        }
    }
    public Object eval(SExprReader.StringLiteral sexpr) throws java.io.IOException {
        return sexpr.value;
    }
}
