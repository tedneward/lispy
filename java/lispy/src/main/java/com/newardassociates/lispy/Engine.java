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
        if (sexpr == null) {
            return null;
        } else if (sexpr instanceof SExprReader.ListExpr) {
            return eval(((SExprReader.ListExpr)sexpr).value);
        } else if (sexpr instanceof SExprReader.Atom) {
            return eval((SExprReader.Atom)sexpr);
        } else if (sexpr instanceof SExprReader.StringLiteral) {
            return eval((SExprReader.StringLiteral)sexpr);
        } else {
            throw new java.io.IOException("Unknown SExpr type: " + sexpr.getClass().getName());
        }
    }

    public Object eval(List<SExprReader.SExpr> list) throws java.io.IOException {
        if (list.size() == 0) {
            return null;
        }

        if (list.get(0) instanceof SExprReader.Atom) {
            String verb = ((SExprReader.Atom)list.get(0)).value;
            if (verb.equals("begin")) {
                Object result = null;
                for (int i = 1; i < list.size(); i++) {
                    result = eval((SExprReader.SExpr)list.get(i));
                }
                return result;
            }
            else if (verb.equals("+")) {
                Object accum = Integer.valueOf(0);
                for (int i = 1; i < list.size(); i++) {
                    Object val = eval(list.get(i));
                    if (accum instanceof Double || val instanceof Double) {
                        accum = ((Number)accum).doubleValue() + ((Number)val).doubleValue();
                    } else {
                        accum = ((Number)accum).intValue() + ((Number)val).intValue();
                    }
                }
                return accum;
            }
            else if (verb.equals("*")) {
                Object accum = Integer.valueOf(1);
                for (int i = 1; i < list.size(); i++) {
                    Object val = eval(list.get(i));
                    if (accum instanceof Double || val instanceof Double) {
                        accum = ((Number)accum).doubleValue() * ((Number)val).doubleValue();
                    } else {
                        accum = ((Number)accum).intValue() * ((Number)val).intValue();
                    }
                }
                return accum;
            }
            else if (verb.equals("-")) {
                Object lhs = eval(list.get(1));
                Object rhs = eval(list.get(2));
                if (lhs instanceof Double || rhs instanceof Double) {
                    return ((Number)lhs).doubleValue() - ((Number)rhs).doubleValue();
                } else {
                    return ((Number)lhs).intValue() - ((Number)rhs).intValue();
                }
            } 
            else if (verb.equals("/")) {
                Object lhs = eval(list.get(1));
                Object rhs = eval(list.get(2));
                if (lhs instanceof Double || rhs instanceof Double) {
                    return ((Number)lhs).doubleValue() / ((Number)rhs).doubleValue();
                } else {
                    return ((Number)lhs).intValue() / ((Number)rhs).intValue();
                }
            }
        }
        else {
            // ... umm....
        }

        return null;
    }
    public Object evalAtom(SExprReader.Atom sexpr) throws java.io.IOException {
        try {
            return sexpr.toInteger();
        } catch (NumberFormatException nfe) {
            try {
                return sexpr.toDouble();
            } catch (NumberFormatException nfe2) {
                return sexpr.value;
            }
        }
    }
    public Object evalStringLiteral(SExprReader.StringLiteral sexpr) {
        return sexpr.value;
    }
}