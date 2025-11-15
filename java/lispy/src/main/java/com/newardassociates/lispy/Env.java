package com.newardassociates.lispy;

import java.util.HashMap;
import java.util.Map;

/**
 * "An environment: a dict of {'var':val} pairs, with an outer Env."
 * 
 * Corresponds directly to lis.py[82:89]
 * 
 * @author Ted Neward
 */
public class Env {
/*
    def find(self, var):
        "Find the innermost Env where var appears."
        return self if (var in self) else self.outer.find(var)

*/
    private Map<String, Object> env = new HashMap<>();
    private Env outer = null;

    public Env(String[] parms, Object[] args, Env outer) {
        this.outer = outer;
        for (int i = 0; i < parms.length; i++) {
            this.env.put(parms[i], args[i]);
        }
    }
    public Env(Env outer) {
        this.outer = outer;
    }
    public Env() { }

    public void set(String key, Object value) {
        this.env.put(key, value);
    }

    public Object get(String key) {
        // Always look local first before checking
        // outer scope(s)
        //
        if (this.env.containsKey(key))
            return this.env.get(key);
        else if (this.outer != null)
            return this.outer.get(key);
        else
            return null;
    }    
}
